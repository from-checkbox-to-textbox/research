# ------------------------------------------------------------------------------
# Title:   Checks for the coding rules in llm_helpers.py
# Purpose: Show that the Python port gives the same results as the R pipeline
#          used for the original Gemini run, without calling any API:
#            1. the template fill matches glue::glue_data() (reference fills
#               written by R/llm_00_export_inputs.R);
#            2. code parsing and the modal code reproduce every stored Gemini
#               draw and every stored code in data/tmp/*_t0.1_n10.json.
#          With --run-dir, it also checks a finished coding run: every call
#          in the log was built from exactly one answer.
# Usage:   uv run llm/check_helpers.py --inputs data/llm/inputs [--run-dir data/tmp]
# ------------------------------------------------------------------------------

import argparse
import glob
import hashlib
import json
from pathlib import Path

import pandas as pd

from llm_helpers import (build_prompt, fill_template, load_settings, modal_code,
                         parse_code, read_jsonl)

RESEARCH = Path(__file__).parent.parent

parser = argparse.ArgumentParser()
parser.add_argument("--inputs", default=str(RESEARCH / "data/llm/inputs"))
parser.add_argument("--run-dir", default=None, help="folder holding <provider>/calls.jsonl")
parser.add_argument("--prompts", default=str(RESEARCH / "data/tmp/ai_prompts"))
args = parser.parse_args()

variables = json.loads(Path(args.inputs, "variables.json").read_text())

# 1. Template fill
fill_check = json.loads(Path(args.inputs, "fill_check.json").read_text())
n = 0
for var_name, examples in fill_check.items():
    v = variables[var_name]
    for ex in examples:
        ours = fill_template(ex["template"], {
            "variable_name": var_name, "question_text": v["question_text"],
            "options_block": v["options_block"], "open_response": ex["open_response"]})
        assert ours == ex["filled"], f"Fill differs from glue for {var_name}"
        n += 1
print(f"Template fill matches glue on {n} examples")

# 2. Parsing and modal code on the stored Gemini draws
n_draws = n_answers = 0
for path in sorted(glob.glob(str(RESEARCH / "data/tmp/*_t0.1_n10.json"))):
    for entry in json.loads(Path(path).read_text()):
        valid = []
        for draw in entry["all_responses"]:
            if draw.get("success"):
                assert parse_code(draw["gemini_response"]) == draw["code"], path
                valid.append(draw["code"])
                n_draws += 1
        if valid:
            code, freq = modal_code(valid)
            assert code == entry["code"], f"Mode differs in {path}, row {entry['row']}"
            assert freq == entry["mode_frequency"]
            assert len(valid) == entry["total_valid_responses"]
            assert round(len(valid) / 10 * 100, 1) == entry["success_rate"]
        n_answers += 1
print(f"Parsing reproduces {n_draws:,} stored draws; modal code reproduces {n_answers:,} stored codes")

# 3. One answer per request, in a finished run
if args.run_dir:
    tasks = pd.read_csv(Path(args.inputs, "tasks.csv"), keep_default_na=False)
    tasks = tasks.set_index(["variable", "row"])
    for provider in sorted(p.name for p in Path(args.run_dir).iterdir() if p.is_dir()):
        calls = [c for path in sorted(Path(args.run_dir, provider).glob("calls*.jsonl")) for c in read_jsonl(path)]
        if not calls:
            continue
        prompts = {}
        for var_name in {c["variable"] for c in calls}:
            prompts[var_name] = json.loads(Path(args.prompts, load_settings(provider).get("prompts_from", provider), f"prompt_{var_name}.json").read_text())
        for c in calls:
            p = prompts[c["variable"]]
            task = tasks.loc[(c["variable"], c["row"])]
            prompt, _ = build_prompt(p["system_message"], p["user_template"], variables[c["variable"]], task)
            assert hashlib.sha256(prompt.encode()).hexdigest() == c["prompt_sha256"], \
                f"{provider}: call for {c['variable']} row {c['row']} does not match its single answer"
        print(f"{provider}: all {len(calls):,} logged calls were built from exactly one answer")
