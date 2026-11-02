# Bare yes or no answers to the twelve attitude questions
# Script: sup_bare_answers.R
# Purpose: share of the written open-ended answers to the attitude questions
#          that are a bare yes or no, defined as an answer that reads "oui",
#          "non", "yes" or "no" once lowercased and stripped of punctuation
#          and spaces.
# Output:  results/tables/bare_answers.csv

library(dplyr)

consensus <- readRDS("data/df_clean_consensus_3providers_10_1.rds") %>%
  filter(assignment == "open")

text_cols <- c(
  "env_spending_open", "env_regulation_open", "env_priority_open",
  "env_cause_open", "env_oil_open", "env_exaggerated_open",
  "env_carbon_tax_open", "immi_integration_ope", "immi_take_jobs_open",
  "immi_spending_open", "immi_more_open", "immi_border_open"
)

# Count the written answers and the bare ones, question by question
bare <- tibble(question = text_cols, written = NA_integer_, bare = NA_integer_)
for (i in seq_along(text_cols)) {
  text <- consensus[[text_cols[i]]]
  text <- text[!is.na(text) & trimws(text) != ""]
  normalized <- tolower(gsub("[[:punct:][:space:]]", "", text))
  bare$written[i] <- length(text)
  bare$bare[i] <- sum(normalized %in% c("oui", "non", "yes", "no"))
}

bare <- bind_rows(bare, summarise(bare, question = "total", across(where(is.numeric), sum))) %>%
  mutate(share_bare = bare / written)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(bare, "results/tables/bare_answers.csv", row.names = FALSE)
print(as.data.frame(bare))
