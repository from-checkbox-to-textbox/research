#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:   Export the inputs for the three-provider LLM coding
# Purpose: Write everything the Python scripts in llm/ need from the raw survey
#          file: one row per open-ended answer to code, and for each variable
#          the question wording, the valid codes, the options block and the
#          meta-prompt sent to the prompt-design model. The meta-prompt fills
#          docs/prompts/meta_prompt_template.txt with each question's wording,
#          response categories, three example answers and the language mix of
#          the sample, as in the original Gemini run, so every provider gets
#          the same instructions. Answers to code: completed surveys,
#          non-empty open answer.
# Usage:   Rscript R/llm_00_export_inputs.R [raw_file] [out_dir]
# Output:  <out_dir>/tasks.csv        one row per answer to code
#          <out_dir>/variables.json   question, codes and meta-prompt per variable
#          <out_dir>/fill_check.json  user messages filled by glue with the
#                                     existing Gemini prompts, used by
#                                     llm/check_helpers.py to test the Python fill
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)
  library(glue)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
raw_file <- if (length(args) >= 1) args[1] else "data/raw/survey.rds"
out_dir  <- if (length(args) >= 2) args[2] else "data/llm/inputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df <- readRDS(raw_file)
config <- read.csv("docs/prompt_config.csv", stringsAsFactors = FALSE)
template_text <- paste(readLines("docs/prompts/meta_prompt_template.txt", warn = FALSE), collapse = "\n")

# Language mix of the whole sample, stated in every meta-prompt
french_pct <- round((sum(df$UserLanguage == "FR-CA", na.rm = TRUE) / nrow(df)) * 100, 1)
english_pct <- round((sum(df$UserLanguage == "EN", na.rm = TRUE) / nrow(df)) * 100, 1)
language_info <- glue("Language distribution: {french_pct}% French (FR-CA), {english_pct}% English (EN)")

variables <- list()
tasks <- list()
fill_check <- list()

for (i in seq_len(nrow(config))) {
  var_name   <- config$variable_name[i]
  closed_col <- config$closed_column[i]
  open_col   <- config$open_column[i]
  var_data <- df[[var_name]]

  # Question wording
  label <- attr(var_data, "label")
  question_text <- if (!is.null(label) && nzchar(label)) as.character(label) else var_name

  # Response categories: the labelled codes, or the range of a numeric question
  labels_vec <- attr(var_data, "labels")
  is_numeric <- is.null(labels_vec) || length(labels_vec) == 0
  if (is_numeric) {
    numeric_values <- var_data[!is.na(var_data)]
    numeric_range <- range(numeric_values, na.rm = TRUE)
    variable_type <- "numeric"
    categories_info <- glue("Valid range: {numeric_range[1]} to {numeric_range[2]}")
    sample_values <- paste(head(unique(sort(numeric_values)), 10), collapse = ", ")
  } else {
    order_idx <- order(as.integer(labels_vec))
    category_codes <- as.integer(labels_vec)[order_idx]
    category_texts <- names(labels_vec)[order_idx]
    variable_type <- "categorical"
    categories_info <- paste0(category_codes, ": ", category_texts, collapse = "\n")
    sample_values <- paste(head(category_texts, 5), collapse = ", ")
  }

  # Topic of the question
  variable_domain <- "general survey"
  if (grepl("^ses_", var_name)) variable_domain <- "socioeconomic/demographic"
  if (grepl("^env_", var_name)) variable_domain <- "environmental policy"
  if (grepl("^immi_", var_name)) variable_domain <- "immigration policy"
  if (grepl("^vote_", var_name)) variable_domain <- "voting behavior"

  # Three example answers, taken from the column <variable>_open. Two questions
  # store their text under another name (ses_educ_open, immi_integration_ope),
  # so in the original run their meta-prompt said that no column was found;
  # this is kept so that the meta-prompts are the ones the models received.
  example_col <- paste0(var_name, "_open")
  if (example_col %in% names(df)) {
    examples <- df[[example_col]][!is.na(df[[example_col]]) & trimws(df[[example_col]]) != ""]
    sample_responses <- if (length(examples) > 0) {
      paste0("\"", head(examples, 3), "\"", collapse = ", ")
    } else {
      "No sample responses available"
    }
  } else {
    sample_responses <- "No open-ended responses column found"
  }

  meta_prompt <- glue(template_text,
                      variable_name = var_name,
                      question_text = question_text,
                      variable_type = variable_type,
                      variable_domain = variable_domain,
                      sample_values = sample_values,
                      categories_info = categories_info,
                      sample_responses = sample_responses,
                      language_info = language_info)

  # Valid codes of the closed-ended question
  labels_closed <- attr(df[[closed_col]], "labels")
  if (is_numeric || is.null(labels_closed) || length(labels_closed) == 0) {
    value_range <- range(df[[closed_col]], na.rm = TRUE)
    codes <- value_range[1]:value_range[2]
  } else {
    codes <- as.integer(labels_closed)
  }
  question_closed <- attr(df[[closed_col]], "label")
  question_closed <- if (!is.null(question_closed) && nzchar(question_closed)) as.character(question_closed) else var_name

  variables[[var_name]] <- list(
    variable_name = var_name,
    question_text = question_closed,
    options_block = as.character(categories_info),
    is_numeric = is_numeric,
    codes = as.integer(codes),
    meta_prompt = as.character(meta_prompt)
  )

  # Answers to code: completed surveys, non-empty open answer
  answers <- df[[open_col]]
  keep <- which(!is.na(answers) & trimws(answers) != "" & df$Finished == 1)
  tasks[[var_name]] <- data.frame(
    variable = var_name,
    row = keep,
    response_id = df$ResponseId[keep],
    open_response = as.character(answers[keep]),
    stringsAsFactors = FALSE
  )

  # Reference fills of the first three answers with the 2025 Gemini prompt,
  # used by llm/check_helpers.py to test the Python fill
  gemini_prompt <- fromJSON(file.path("data/tmp/ai_prompts", glue("prompt_{var_name}.json")))
  fill_check[[var_name]] <- list()
  for (r in head(keep, 3)) {
    context <- list(variable_name = var_name, question_text = question_closed,
                    options_block = categories_info, open_response = df[[open_col]][r])
    fill_check[[var_name]][[length(fill_check[[var_name]]) + 1]] <- list(
      template = gemini_prompt$user_template,
      open_response = as.character(df[[open_col]][r]),
      filled = as.character(glue_data(context, gemini_prompt$user_template, .open = "{", .close = "}"))
    )
  }

  cat(glue("{var_name}: {length(keep)} answers to code"), "\n")
}

tasks <- do.call(rbind, tasks)
write.csv(tasks, file.path(out_dir, "tasks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write_json(variables, file.path(out_dir, "variables.json"), auto_unbox = TRUE, pretty = TRUE)
write_json(fill_check, file.path(out_dir, "fill_check.json"), auto_unbox = TRUE, pretty = TRUE)

cat(glue("\n{nrow(tasks)} answers across {length(variables)} variables written to {out_dir}"), "\n")
