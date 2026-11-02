# Coded answers of each provider, merged with the survey data
# Script: assemble_json.R
# Purpose: reads the modal codes written by llm/llm_02_code_responses.py for
#          each provider and question (data/tmp/<provider>/<question>_t1.0_n10.json)
#          and adds them to the survey data of the respondents who completed
#          the survey, as <question>_ai. One file per provider, read by
#          R/llm_03_consensus.R.
# Usage:   Rscript R/assemble_json.R [raw_file] [out_dir]
# Output:  <out_dir>/df_clean_gemini_3_5_flash_lite_10_1.rds
#          <out_dir>/df_clean_claude_haiku_5_5_10_1.rds
#          <out_dir>/df_clean_openai_gpt_oss_20b_10_1.rds

library(dplyr)
library(jsonlite)
library(haven)

args <- commandArgs(trailingOnly = TRUE)
raw_file <- if (length(args) >= 1) args[1] else "data/raw/survey.rds"
out_dir <- if (length(args) >= 2) args[2] else "data"

survey <- readRDS(raw_file) %>%
  filter(Finished == 1)

providers <- c(
  google = "df_clean_gemini_3_5_flash_lite_10_1.rds",
  anthropic = "df_clean_claude_haiku_5_5_10_1.rds",
  openai = "df_clean_openai_gpt_oss_20b_10_1.rds"
)

for (provider in names(providers)) {
  coded <- survey
  json_files <- list.files(file.path("data/tmp", provider), pattern = "_t1\\.0_n10\\.json$", full.names = TRUE)

  # One column of codes per question, matched on the response identifier
  for (json_file in json_files) {
    question <- sub("_t1\\.0_n10\\.json$", "", basename(json_file))
    codes <- fromJSON(json_file) %>%
      select(responseId, code) %>%
      filter(!is.na(code))
    names(codes)[2] <- paste0(question, "_ai")
    coded <- coded %>% left_join(codes, by = c("ResponseId" = "responseId"))
    cat(provider, question, nrow(codes), "codes\n")
  }

  saveRDS(coded, file.path(out_dir, providers[[provider]]))
}
