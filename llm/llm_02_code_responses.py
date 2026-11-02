# ------------------------------------------------------------------------------
# Title:   Coding: the small model of each provider codes every open answer
# Purpose: Same procedure as R/ai_process_responses.R, for one provider at a
#          time. Each answer is sent on its own, ten times (ten independent
#          draws at temperature 1). In each draw the first whole number in
#          the reply is the code; a draw with no valid code is retried up to
#          three attempts. The answer's code is the most frequent valid code
#          (smallest code on ties). If none of the ten draws gives a valid
#          code, the ten draws are run once more.
#          All draws are sent concurrently, paced under the provider's rate
#          limit, in a random order (fixed seed) so that any change on the
#          provider's side during the run is spread over all variables.
#          Every call is appended to a log as it returns; rerunning the script
#          skips the draws already done.
# Usage:   uv run llm/llm_02_code_responses.py --provider anthropic
#          uv run llm/llm_02_code_responses.py --provider google --variables ses_gender env_regulation --n-per-variable 50
# Output:  <out>/<provider>/calls.jsonl              one line per API call
#          <out>/<provider>/<variable>_t1.0_n10.json  same structure as the
#                                                     files read by R/assemble_json.R
# ------------------------------------------------------------------------------

import argparse
import asyncio
import hashlib
import json
import random
import time
import zlib
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path
from zoneinfo import ZoneInfo

import pandas as pd
from tqdm import tqdm

from llm_helpers import (PROVIDERS, Pacer, ask, build_prompt, error_kind, is_valid, load_keys,
                         load_settings, make_client, modal_code, parse_code,
                         read_jsonl, transient_errors)

RESEARCH = Path(__file__).parent.parent
DRAWS = 10
ATTEMPTS = 3
SEED = 20261023

parser = argparse.ArgumentParser()
parser.add_argument("--provider", required=True, choices=PROVIDERS + ["google_openrouter"])
parser.add_argument("--variables", nargs="*", default=None)
parser.add_argument("--n-per-variable", type=int, default=None, help="random subsample, for pilots")
parser.add_argument("--via-openrouter", action="store_true",
                    help="google only: send through OpenRouter instead of Google's API")
parser.add_argument("--share", default="0/1",
                    help="k/n: take only the k-th of n disjoint shares of the draws, so that two routes can run at once")
parser.add_argument("--inputs", default=str(RESEARCH / "data/llm/inputs"))
parser.add_argument("--prompts", default=str(RESEARCH / "data/tmp/ai_prompts"))
parser.add_argument("--out", default=str(RESEARCH / "data/tmp"))
args = parser.parse_args()

provider = args.provider
# The Gemini coder can be reached directly or through OpenRouter. Both routes
# write to the same log, so a run started on one can be finished on the other;
# each call records which route served it.
route = "google_openrouter" if args.via_openrouter and provider == "google" else provider
s = load_settings(route)
variables = json.loads(Path(args.inputs, "variables.json").read_text())

# keep_default_na=False keeps answers such as "NA" or "None" as text
tasks = pd.read_csv(Path(args.inputs, "tasks.csv"), keep_default_na=False)
n_all_tasks = len(tasks)
if args.variables:
    tasks = tasks[tasks["variable"].isin(args.variables)]
if args.n_per_variable:
    tasks = tasks.groupby("variable").sample(n=args.n_per_variable, random_state=2026)

prompts = {}
for var_name in tasks["variable"].unique():
    path = Path(args.prompts, s.get("prompts_from", provider), f"prompt_{var_name}.json")
    prompts[var_name] = json.loads(path.read_text())

# One prompt per answer: the system message, then the user template filled
# with that single answer
built = {}
for task in tasks.to_dict("records"):
    p = prompts[task["variable"]]
    prompt, user_message = build_prompt(p["system_message"], p["user_template"],
                                        variables[task["variable"]], task)
    built[(task["variable"], task["row"])] = (prompt, user_message, task)

out_dir = Path(args.out, provider)
out_dir.mkdir(parents=True, exist_ok=True)
# One log per route; a draw done on any route counts as done
log_path = out_dir / ("calls.jsonl" if route == provider else f"calls_{route}.jsonl")
share_k, share_n = (int(x) for x in args.share.split("/"))


def read_all_logs():
    calls = []
    for path in sorted(out_dir.glob("calls*.jsonl")):
        calls += read_jsonl(path)
    return calls


client = make_client(route, load_keys()[route], timeout=60)
pacer = Pacer(s["requests_per_minute"])
# Spending and daily request counts are kept per route (direct or OpenRouter)
this_route = [c for c in read_all_logs() if c.get("route", provider) == route]
spent = sum(c["cost"] for c in this_route)

# Daily request limits (Gemini) reset at midnight Pacific time
PACIFIC = ZoneInfo("America/Los_Angeles")
today = datetime.now(PACIFIC).date()
calls_today = sum(1 for c in this_route
                  if datetime.fromisoformat(c["time"]).astimezone(PACIFIC).date() == today)
daily_cap = s.get("requests_per_day", float("inf"))


def finished_draws(calls):
    # A draw is finished once it has a valid code or three attempts
    by_draw = {}
    for c in calls:
        key = (c["variable"], c["row"], c["round"], c["draw"])
        done = by_draw.get(key, False)
        by_draw[key] = done or c["valid"] or c["attempt"] >= ATTEMPTS
    return {k for k, done in by_draw.items() if done}


async def code_draw(job):
    global spent, calls_today
    if spent >= s["budget_usd"] or calls_today >= daily_cap:
        return []
    prompt, _, task = built[(job["variable"], job["row"])]
    lines = []
    for attempt in range(1, ATTEMPTS + 1):
        try:
            reply = await ask(route, client, s, "coder", prompt, pacer)
        except Exception as err:
            if error_kind(err) is None:
                raise
            # Still rate-limited after all retries: leave the draw open, it is
            # picked up again on the next pass
            break
        code = parse_code(reply["text"])
        spent += reply["cost"]
        calls_today += 1
        lines.append({**job, "attempt": attempt, "route": route,
                      "response_id": task["response_id"],
                      "prompt_sha256": hashlib.sha256(prompt.encode()).hexdigest(),
                      "code": code, "valid": is_valid(code, variables[job["variable"]]),
                      "time": datetime.now(timezone.utc).isoformat(timespec="seconds"), **reply})
        if lines[-1]["valid"]:
            break
    return lines


async def run(jobs, label):
    gate = asyncio.Semaphore(s["max_concurrent"])

    async def one(job):
        async with gate:
            return await code_draw(job)

    with open(log_path, "a", encoding="utf-8") as log:
        for result in tqdm(asyncio.as_completed([one(j) for j in jobs]), total=len(jobs), desc=label):
            for line in await result:
                log.write(json.dumps(line, ensure_ascii=False) + "\n")
            log.flush()


def in_share(v, r, d):
    # A fixed split of the draws: each draw always falls in the same share
    return zlib.crc32(f"{v}|{r}|{d}".encode()) % share_n == share_k


def draw_jobs(answer_keys, round_number, done):
    jobs = [{"variable": v, "row": r, "round": round_number, "draw": d}
            for (v, r) in answer_keys for d in range(1, DRAWS + 1)
            if (v, r, round_number, d) not in done and in_share(v, r, d)]
    random.Random(SEED + round_number).shuffle(jobs)
    return jobs


async def main():
    start = time.monotonic()
    answer_keys = list(built)

    # Up to three passes: draws left open by long rate-limit spells are retried
    for attempt_pass in range(1, 4):
        jobs = draw_jobs(answer_keys, 1, finished_draws(read_all_logs()))
        if not jobs or spent >= s["budget_usd"] or calls_today >= daily_cap:
            break
        print(f"{provider}: {len(answer_keys):,} answers, {len(jobs):,} draws to run (pass {attempt_pass})")
        await run(jobs, f"{provider} draws")

    # Answers with no valid code in any of the ten draws are run once more
    # Only answers whose ten draws are all finished (on any route) and none valid
    calls = read_all_logs()
    done = finished_draws(calls)
    valid_round1 = {(c["variable"], c["row"]) for c in calls if c["round"] == 1 and c["valid"]}
    rerun = [k for k in answer_keys if k not in valid_round1
             and all((k[0], k[1], 1, d) in done for d in range(1, DRAWS + 1))]
    if rerun and spent < s["budget_usd"] and calls_today < daily_cap:
        jobs = draw_jobs(rerun, 2, finished_draws(calls))
        print(f"{len(rerun)} answers had no valid code; running their ten draws again")
        await run(jobs, f"{provider} reruns")

    calls = read_all_logs()
    write_outputs(calls)
    summarise(calls, time.monotonic() - start)


def write_outputs(calls):
    # One file per variable, with the fields R/assemble_json.R reads
    by_answer = {}
    for c in calls:
        by_answer.setdefault((c["variable"], c["row"]), []).append(c)

    results = {}
    for key, (prompt, user_message, task) in built.items():
        answer_calls = by_answer.get(key, [])
        last_round = max([c["round"] for c in answer_calls], default=1)
        draws = []
        for d in range(1, DRAWS + 1):
            attempts = [c for c in answer_calls if c["round"] == last_round and c["draw"] == d]
            final = attempts[-1] if attempts else None
            draws.append({"prompt_num": d,
                          "model_response": final["text"] if final else None,
                          "code": final["code"] if final and final["valid"] else None,
                          "retry_count": len(attempts),
                          "success": bool(final and final["valid"])})
        valid = [d["code"] for d in draws if d["success"]]
        p = prompts[key[0]]
        entry = {"row": int(task["row"]), "responseId": task["response_id"],
                 "system_message": p["system_message"], "user_message": user_message,
                 "open_response": task["open_response"]}
        if valid:
            code, freq = modal_code(valid)
            entry.update(code=code, mode_frequency=freq, total_valid_responses=len(valid),
                         success_rate=round(len(valid) / DRAWS * 100, 1))
        else:
            entry.update(code=None, mode_frequency=0, total_valid_responses=0, success_rate=0,
                         error="No valid responses from any of the 10 prompts")
        entry["all_responses"] = draws
        results.setdefault(key[0], []).append(entry)

    for var_name, entries in results.items():
        entries.sort(key=lambda e: e["row"])
        path = Path(args.out, provider, f"{var_name}_t1.0_n10.json")
        path.write_text(json.dumps(entries, ensure_ascii=False, indent=1), encoding="utf-8")


def summarise(calls, seconds):
    calls = pd.DataFrame(calls)
    n_answers = len(built)
    coded = calls[calls["valid"]].groupby(["variable", "row"]).ngroups
    draws = calls.groupby(["variable", "row", "round", "draw"]).ngroups
    cost_per_call = calls["cost"].mean()
    calls_per_draw = len(calls) / draws
    projected = cost_per_call * calls_per_draw * n_all_tasks * DRAWS

    print(f"\n{provider}: {s['coder']}")
    print(f"  answers coded          {coded:,} of {n_answers:,} ({coded / n_answers:.1%})")
    print(f"  API calls              {len(calls):,} for {draws:,} draws ({calls_per_draw:.3f} calls per draw)")
    print(f"  valid replies          {calls['valid'].mean():.1%}; empty replies {(calls['text'].str.strip() == '').mean():.1%}")
    print(f"  tokens per call        input {calls['input_tokens'].mean():.0f}, output {calls['output_tokens'].mean():.1f}, "
          f"of which reasoning {calls['reasoning_tokens'].mean():.1f}")
    print(f"  stop reasons           {dict(Counter(calls['stop_reason']))}")
    print(f"  served by              {dict(Counter(calls['served_by']))}, {dict(Counter(calls['model_version']))}")
    this_run = sum(1 for t in calls["time"] if datetime.fromisoformat(t).timestamp() >= time.time() - seconds)
    print(f"  time this run          {seconds / 60:.1f} min ({this_run / max(seconds, 1) * 60:,.0f} calls per minute)")
    print(f"  rate-limit/server/connection errors retried: {transient_errors}")
    print(f"  cost so far            ${calls['cost'].sum():.4f} (${cost_per_call * 1000:.4f} per 1,000 calls)")
    print(f"  projected full run     ${projected:.2f} for {n_all_tasks:,} answers x {DRAWS} draws")
    if spent >= s["budget_usd"]:
        print(f"  STOPPED at the budget of ${s['budget_usd']}; rerun to continue after raising it")
    if calls_today >= daily_cap:
        print(f"  STOPPED at {daily_cap:,} requests today; rerun after midnight Pacific time to continue")


asyncio.run(main())
