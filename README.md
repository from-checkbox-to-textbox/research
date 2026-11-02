# From Checkbox to Textbox

Replication code for the article *From Checkbox to Textbox: Can LLM-Processed Open-Ended Questions Measure the Same Latent Variables as Traditional Closed-Ended Questions?*

In a survey experiment, 3,372 Canadian respondents were randomly assigned to closed-ended or open-ended versions of the same environmental and immigration questions. Models from three providers coded the open-ended answers into the closed-ended categories, and the article compares the two conditions.

## Contents

```
R/                 raw data, cleaning, consensus coding, analysis and figures
llm/               coding of the open-ended answers by the three providers (Python)
docs/              coding configuration and the meta-prompt template
data/              coded answers and cleaned data
data/tmp/          prompts, per-draw outputs and call logs of the coding
run_all.R          runs the analysis from the coded answers to every output
Makefile           builds everything, from the raw data to the article PDF
```

## Reproducing the results

Requirements: R (4.6) with the packages dplyr, tidyr, haven, psych, GPArotation, ggplot2, scales, stringr, jsonlite, httr and glue; Python (3.12 or later) for the scripts in `llm/`, with dependencies pinned in `llm/uv.lock`; latexmk to compile the article.

From the root of the repository:

```
make
```

This reads the raw survey data from `data/raw/survey.rds`, which is not included (see `DATA_AVAILABILITY.md`), rebuilds the consensus codes, cleans both conditions, writes every figure and table to `results/`, copies the figures into the article and compiles it to `build/main.pdf`. The article itself is an Overleaf project kept outside this repository, in `article/`. `make analysis` stops before the article.

`make` starts from the coded answers saved in `data/` and makes no API call, so it reproduces the article exactly. Without the raw survey file, the scripts that start from the cleaned data in `data/` still run, including every figure (see `DATA_AVAILABILITY.md`). `make full` first reruns the API calls, the coding of the open-ended answers by the three providers and the exploratory strategic-vote coding, then everything else. It needs the providers' API keys, costs about 100 dollars, and gives codes that differ slightly from the saved ones, since the models sample at a temperature of 1. The models and settings are in `llm/models.toml`.

## Where each result comes from

| Result in the article | Script |
|---|---|
| Raw survey data (responses recorded before June 23, 2025) | `R/00_get_survey.R` |
| Figure 1 (coding procedure) | drawn in the manuscript |
| Figure 2 (factor loadings, reliability) | `R/fig_facanal_dumbell_12vars.R` |
| Figure 3 (regressions by condition) | `R/fig_regression_difference.R` |
| Sample, assignment and completion | `R/sup_sample_counts.R` |
| Coding: draw agreement, disagreements, calls and cost | `llm/llm_04_coding_numbers.py` |
| Coding: consensus rules | `R/llm_03_consensus.R` |
| Bare yes or no answers | `R/sup_bare_answers.R` |
| Missing answers (Results and appendix) | `R/sup_nonresponse_decomposition.R` |
| Missing answers by education (Results and appendix) | `R/sup_missing_by_education.R` |
| Factor and regression numbers in the Results | `R/sup_results_numbers.R` |
| Where respondents left the survey | `R/sup_attrition_stage.R` |
| Quoted answers | `R/sup_quotes.R` |
| Exploratory strategic vote | `R/sup_vote_strategic.R` (calls an API; output in `data/vote_strategic.csv`) |
| Appendix: questions and response categories | `R/sup_questionnaire.R` |
| Appendix: balance between conditions | `R/sup_completer_composition.R`, `R/sup_dwelling_semidetached.R` |
| Appendix: codes of each provider and of the 2025 coding | `R/sup_robustness_coders.R` |
| Appendix: scree plot | `R/fig_scree_by_scale.R` |
| Appendix: pairwise correlations | `R/fig_facanal_dumbell_12vars_pairwise.R` |
| Appendix: tests of the differences | `R/sup_regression_interactions.R` |

## Data

See `DATA_AVAILABILITY.md`.
