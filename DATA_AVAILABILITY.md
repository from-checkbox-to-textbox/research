# Data availability

The raw survey data are not included. The scripts read them from `data/raw/survey.rds`, which holds the responses recorded before June 23, 2025, the export analysed in the article. For anonymous review, the script that downloads them from the research group's data lake (`R/00_get_survey.R`) is withheld, since it identifies the group.

The coded answers and the cleaned data used in the article are in `data/`. The coded-answer files (`data/df_clean_<model>_10_1.rds`) include the respondents' open-ended answers and their survey response identifiers, so that the coding can be checked. The per-draw outputs and the call logs of the three-provider coding (`data/tmp/<provider>/`) and the coding inputs (`data/llm/`) are not tracked in this repository and will be deposited with the replication archive.

Without the raw survey file, every script that starts from `data/` runs, including all the figures, the factor and regression numbers, and the appendix tables on balance, providers and the tests of the differences. Six scripts need the raw file: `R/cleaning___master.R`, `R/sup_sample_counts.R`, `R/sup_nonresponse_decomposition.R`, `R/sup_attrition_stage.R`, `R/sup_quotes.R` and `R/sup_questionnaire.R`. `llm/llm_04_coding_numbers.py` needs the per-draw outputs and call logs.

The prompts written by the models for each question are in `data/tmp/ai_prompts/<provider>/`, and the meta-prompt template is in `docs/prompts/meta_prompt_template.txt`. The prompt files directly in `data/tmp/ai_prompts/` come from the earlier coding with Gemini models (2025), which the article reports in an appendix.
