# ------------------------------------------------------------------------------
# Title:   Prompt design: the large model of each provider writes the prompts
# Purpose: For each variable, send the meta-prompt (built in R by
#          R/llm_00_export_inputs.R from docs/prompts/meta_prompt_template.txt)
#          to the provider's large model. Its reply holds the system message
#          and the user template that the small model will use to code every
#          answer. Parsing follows parse_ai_response() in
#          R/ai_prompt_generator.R. Variables are sent in parallel; a prompt
#          that already exists is kept, so the script can be rerun safely.
# Usage:   uv run llm/llm_01_design_prompts.py --provider all
#          uv run llm/llm_01_design_prompts.py --provider anthropic --variables ses_gender env_regulation
# Output:  <out>/<provider>/prompt_<variable>.json (same fields as the files in
#          data/tmp/ai_prompts/, plus the raw reply, tokens and cost)
# ------------------------------------------------------------------------------

import argparse
import asyncio
import json
import time
from datetime import datetime
from pathlib import Path

from llm_helpers import (PROVIDERS, ask, fill_template, load_keys, load_settings,
                         make_client, parse_designer_reply)

RESEARCH = Path(__file__).parent.parent

parser = argparse.ArgumentParser()
parser.add_argument("--provider", default="all", choices=PROVIDERS + ["all"])
parser.add_argument("--variables", nargs="*", default=None)
parser.add_argument("--inputs", default=str(RESEARCH / "data/llm/inputs"))
parser.add_argument("--out", default=str(RESEARCH / "data/tmp/ai_prompts"))
args = parser.parse_args()

variables = json.loads(Path(args.inputs, "variables.json").read_text())
var_names = args.variables or list(variables)
providers = PROVIDERS if args.provider == "all" else [args.provider]
keys = load_keys()


async def design_one(provider, client, s, var_name):
    path = Path(args.out, provider, f"prompt_{var_name}.json")
    if path.exists():
        return f"{provider:9s} {var_name:18s} kept existing prompt"
    v = variables[var_name]
    start = time.monotonic()
    reply = await ask(provider, client, s, "designer", v["meta_prompt"])
    try:
        system_message, user_template = parse_designer_reply(reply["text"])
    except ValueError as err:   # json.JSONDecodeError is a ValueError
        raise ValueError(f"{provider} {var_name}: could not read the reply as JSON ({err}); rerun to try again")

    # The template must fill without error and include the answer to code
    if "{open_response}" not in user_template.replace(" ", ""):
        raise ValueError(f"{provider} {var_name}: the template has no {{open_response}} placeholder")
    fill_template(user_template, {"variable_name": "x", "question_text": "x",
                                  "options_block": "x", "open_response": "x"})

    prompt = {
        "variable_name": var_name,
        "generation_timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        "generation_time_seconds": round(time.monotonic() - start, 2),
        "model_used": s["designer"],
        "model_version": reply["model_version"],
        "served_by": reply["served_by"],
        "question_text": v["question_text"],
        "system_message": system_message,
        "user_template": user_template,
        "options_block": v["options_block"],
        "is_numeric": v["is_numeric"],
        "designer_reply": reply["text"],
        "input_tokens": reply["input_tokens"],
        "output_tokens": reply["output_tokens"],
        "reasoning_tokens": reply["reasoning_tokens"],
        "cost_usd": round(reply["cost"], 6),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(prompt, ensure_ascii=False, indent=2), encoding="utf-8")
    return (f"{provider:9s} {var_name:18s} {prompt['generation_time_seconds']:6.1f}s  "
            f"{len(system_message):5d} chars  ${reply['cost']:.4f}")


async def main():
    jobs = []
    for provider in providers:
        s = load_settings(provider)
        client = make_client(provider, keys[provider], timeout=900)
        jobs += [design_one(provider, client, s, v) for v in var_names]
    for line in await asyncio.gather(*jobs, return_exceptions=True):
        print(line if isinstance(line, str) else f"FAILED: {line}")


asyncio.run(main())
