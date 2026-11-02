# ------------------------------------------------------------------------------
# Title:   Numbers reported on the coding procedure
# Purpose: From the per-draw outputs and the call logs of the three providers,
#          computes (1) for each provider, the share of the ten draws that
#          agree with the modal code, averaged over all coded answers;
#          (2) among the answers to the ordinal attitude questions on which
#          the providers' modal codes differ, the share whose codes are one
#          step apart on the scale; (3) the number of model calls, by provider
#          and stage, and their cost.
# Usage:   python3 llm/llm_04_coding_numbers.py
# Output:  results/tables/coding_numbers.csv
# ------------------------------------------------------------------------------

import csv
import glob
import json
import os
from collections import defaultdict

PROVIDERS = ["google", "anthropic", "openai"]
DRAWS = "data/tmp/{provider}/{variable}_t1.0_n10.json"

# Substantive codes of each ordinal attitude question, in scale order
ORDINAL = {
    "env_spending": [1, 4, 5], "env_regulation": [1, 4, 5, 6, 7],
    "env_priority": [1, 4, 5, 6, 7], "env_oil": [1, 4, 5, 6, 7],
    "env_exaggerated": [1, 4, 5, 6], "env_carbon_tax": [1, 4, 5, 6],
    "immi_integration": [1, 4, 5, 6, 7], "immi_take_jobs": [1, 4, 5, 6, 7],
    "immi_spending": [1, 4, 5], "immi_more": [1, 4, 5, 6, 7],
    "immi_border": [1, 4, 5, 6],
}

# The per-draw outputs and call logs are not in the repository (DATA_AVAILABILITY.md)
if not glob.glob(DRAWS.format(provider="google", variable="*")):
    raise SystemExit("The per-draw outputs in data/tmp/<provider>/ are not included in this repository (see DATA_AVAILABILITY.md)")

rows = []

# (1) Agreement of the ten draws with the modal code
for provider in PROVIDERS:
    shares = []
    for path in sorted(glob.glob(DRAWS.format(provider=provider, variable="*"))):
        for answer in json.load(open(path)):
            if answer.get("total_valid_responses"):
                shares.append(answer["mode_frequency"] / answer["total_valid_responses"])
    rows.append((f"{provider}: draws agreeing with the modal code", sum(shares) / len(shares)))
    rows.append((f"{provider}: coded answers", len(shares)))

# (2) Distance between the providers' codes when they differ
differ = one_step = 0
for variable, scale in ORDINAL.items():
    positions = defaultdict(list)
    for provider in PROVIDERS:
        for answer in json.load(open(DRAWS.format(provider=provider, variable=variable))):
            if answer.get("code") in scale:
                positions[answer["row"]].append(scale.index(answer["code"]))
    for p in positions.values():
        if len(p) >= 2 and len(set(p)) > 1:
            differ += 1
            one_step += max(p) - min(p) == 1
rows.append(("answers on which the providers' codes differ", differ))
rows.append(("share of those whose codes are one step apart", one_step / differ))

# (3) Calls and cost: coding calls in the logs, prompt-writing calls in the prompt files
coding_calls = coding_cost = 0
for path in sorted(glob.glob("data/tmp/*/calls*.jsonl")):
    n = cost = 0
    for line in open(path):
        n += 1
        cost += json.loads(line).get("cost") or 0
    rows.append((f"coding calls in {path}", n))
    coding_calls += n
    coding_cost += cost
prompt_files = sorted(glob.glob("data/tmp/ai_prompts/*/prompt_*.json"))
prompt_cost = sum(json.load(open(f)).get("cost_usd") or 0 for f in prompt_files)
rows.append(("prompt-writing calls", len(prompt_files)))
rows.append(("total model calls", coding_calls + len(prompt_files)))
rows.append(("cost of the coding calls (USD)", coding_cost))
rows.append(("cost of the prompt-writing calls (USD)", prompt_cost))
rows.append(("total cost (USD)", coding_cost + prompt_cost))

os.makedirs("results/tables", exist_ok=True)
with open("results/tables/coding_numbers.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(["statistic", "value"])
    writer.writerows(rows)
for name, value in rows:
    print(f"{name:65s} {value}")
