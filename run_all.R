# Runs the analysis of the article from the coded answers to every figure,
# table and number, in order. Run from the repository root:
#   Rscript run_all.R
#
# The raw survey data are read from data/raw/survey.rds, written by
# R/00_get_survey.R from the research group's data lake.
#
# The coding of the open-ended answers (stage 1) calls the providers' APIs and
# is not rerun here. Its outputs are in data/: the coded answers of each
# provider (data/df_clean_<model>_10_1.rds) and their per-draw files and call
# logs (data/tmp/<provider>/). To rerun it:
#   Rscript R/llm_00_export_inputs.R                       # inputs and meta-prompt
#   uv run llm/llm_01_design_prompts.py --provider all     # large models write the prompts
#   uv run llm/llm_02_code_responses.py --provider google  # small models code every answer
#   uv run llm/llm_02_code_responses.py --provider anthropic
#   uv run llm/llm_02_code_responses.py --provider openai
#   Rscript R/assemble_json.R                              # codes merged with the survey data
# The exploratory strategic-vote coding (R/sup_vote_strategic.R) also calls an
# API and is not rerun here.

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)

scripts <- c(
  # Stage 2: consensus of the three providers and cleaning of both conditions
  "R/llm_03_consensus.R",
  "R/cleaning___master.R",
  # Stage 3: figures of the article
  "R/fig_facanal_dumbell_12vars.R",           # Figure 2
  "R/fig_regression_difference.R",            # Figure 3
  # Stage 4: numbers of the text and appendix tables and figures
  "R/sup_sample_counts.R",                    # Design: sample and completion
  "R/sup_completer_composition.R",            # Appendix: balance
  "R/sup_dwelling_semidetached.R",            # Design footnote on dwelling type
  "R/sup_bare_answers.R",                     # Bare yes or no answers
  "R/sup_nonresponse_decomposition.R",        # Results and appendix: missing answers
  "R/sup_attrition_stage.R",                  # Limitations: where respondents left
  "R/sup_quotes.R",                           # Quoted answers
  "R/sup_questionnaire.R",                    # Appendix: questions and response categories
  "R/sup_regression_interactions.R",          # Appendix: tests of the differences
  "R/sup_results_numbers.R",                  # Results: factor and regression numbers
  "R/fig_scree_by_scale.R",                   # Appendix: scree plot
  "R/fig_facanal_dumbell_12vars_pairwise.R",  # Appendix: pairwise correlations
  "R/sup_robustness_coders.R"                 # Appendix: each provider and the 2025 coding
)

for (script in scripts) {
  cat("\n==>", script, "\n")
  if (system2("Rscript", script) != 0) stop(script, " failed")
}

# Numbers on the coding procedure (Python, standard library only)
if (system2("python3", c("-I", "llm/llm_04_coding_numbers.py")) != 0) {
  stop("llm/llm_04_coding_numbers.py failed")
}

cat("\nDone. Outputs are in results/.\n")
