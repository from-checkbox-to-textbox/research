# Missing answers to the twelve attitude questions, by condition
# Script: sup_nonresponse_decomposition.R
# Purpose: among respondents who completed the survey, count the answers that
#          have no position on the response scale (the cleaned item is
#          missing) and split them by type. Open-ended condition: blank
#          answers and written answers that the three-provider consensus left
#          without a position. Closed-ended condition: skipped questions and
#          the explicit "don't know" option. Also counts, for the number of
#          immigrants Canada should admit, the written answers without a
#          position that contain a digit.
# Output:  results/tables/nonresponse_decomposition.csv

library(dplyr)
library(haven)

merged <- readRDS("data/df_clean_merged.rds")
consensus <- readRDS("data/df_clean_consensus_3providers_10_1.rds")
raw <- readRDS("data/raw/survey.rds")

# Question, cleaned item and open-ended text column
items <- tribble(
  ~question, ~item, ~text_col,
  "env_spending", "env_gov_spending_more_num", "env_spending_open",
  "env_regulation", "env_regulation_stricter_num", "env_regulation_open",
  "env_priority", "env_priority_over_jobs_num", "env_priority_open",
  "env_cause", "env_climate_cause_human_bin", "env_cause_open",
  "env_oil", "env_against_oil_support_num", "env_oil_open",
  "env_exaggerated", "env_climate_change_not_exaggerated_num", "env_exaggerated_open",
  "env_carbon_tax", "env_carbon_tax_maintained_num", "env_carbon_tax_open",
  "immi_integration", "immi_dont_fit_in_num", "immi_integration_ope",
  "immi_take_jobs", "immi_take_jobs_num", "immi_take_jobs_open",
  "immi_spending", "immi_gov_spending_less_num", "immi_spending_open",
  "immi_more", "immi_admit_less_num", "immi_more_open",
  "immi_border", "immi_stricter_border_num", "immi_border_open"
)

# Rows are in the same order in the cleaned and source data of each condition
clean_open <- merged %>% filter(treatment == "open")
clean_closed <- merged %>% filter(treatment == "closed")
text_open <- consensus %>% filter(assignment == "open")
raw_closed <- raw %>% filter(assignment == "closed", Finished == 1)
stopifnot(nrow(clean_open) == nrow(text_open), nrow(clean_closed) == nrow(raw_closed))

# Count the missing answers of each question, by type
decomp <- items %>% select(question)
for (i in seq_len(nrow(items))) {
  missing_open <- is.na(clean_open[[items$item[i]]])
  text <- trimws(text_open[[items$text_col[i]]])
  blank <- is.na(text) | text == ""
  missing_closed <- is.na(clean_closed[[items$item[i]]])
  skipped <- is.na(raw_closed[[items$question[i]]])

  decomp$open_answers[i] <- nrow(clean_open)
  decomp$open_missing[i] <- sum(missing_open)
  decomp$open_blank[i] <- sum(missing_open & blank)
  decomp$open_written[i] <- sum(missing_open & !blank)
  decomp$open_written_digit[i] <- sum(missing_open & !blank & grepl("[0-9]", text))
  decomp$closed_answers[i] <- nrow(clean_closed)
  decomp$closed_missing[i] <- sum(missing_closed)
  decomp$closed_skipped[i] <- sum(missing_closed & skipped)
  decomp$closed_dont_know[i] <- sum(missing_closed & !skipped)
}
totals <- decomp %>%
  summarise(question = "total", across(where(is.numeric), sum))
decomp <- bind_rows(decomp, totals)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(decomp, "results/tables/nonresponse_decomposition.csv", row.names = FALSE)
print(as.data.frame(decomp))

# LaTeX table for the appendix
labels <- c(
  env_spending = "Environment & Spending", env_regulation = "& Stricter regulation",
  env_priority = "& Jobs before the environment", env_cause = "& Cause of climate change",
  env_oil = "& Support for the oil industry", env_exaggerated = "& Threat exaggerated",
  env_carbon_tax = "& Carbon tax", immi_integration = "Immigration & Do not fit in",
  immi_take_jobs = "& Take jobs", immi_spending = "& Spending",
  immi_more = "& Number to admit", immi_border = "& Border controls", total = "Total &"
)
rows <- sprintf(
  "%s & %.1f & %d & %d & %.1f & %d & %d \\\\",
  labels[decomp$question],
  100 * decomp$open_missing / decomp$open_answers, decomp$open_blank, decomp$open_written,
  100 * decomp$closed_missing / decomp$closed_answers, decomp$closed_skipped, decomp$closed_dont_know
)
rows[length(rows)] <- paste("\\midrule\n", rows[length(rows)])
tex <- c(
  "\\begin{tabular}{llrrrrrr}",
  "\\toprule",
  " & & \\multicolumn{3}{c}{Open-ended} & \\multicolumn{3}{c}{Closed-ended} \\\\",
  "\\cmidrule(lr){3-5} \\cmidrule(lr){6-8}",
  " & Question & Missing (\\%) & Blank & Written & Missing (\\%) & Skipped & Don't know \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tex, "results/tables/nonresponse_decomposition.tex")
