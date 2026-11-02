# Answers quoted in the text
# Script: sup_quotes.R
# Purpose: finds each open-ended answer quoted in the article in the data,
#          verbatim (after trimming spaces), and reports the consensus code
#          of the three providers with its response category in the
#          closed-ended question.
# Output:  results/tables/quotes.csv

library(dplyr)
library(haven)

consensus <- readRDS("data/df_clean_consensus_3providers_10_1.rds") %>%
  filter(assignment == "open")
raw <- readRDS("data/raw/survey.rds")

quotes <- tribble(
  ~question, ~text_col, ~quote,
  "immi_more", "immi_more_open", "autour de 250000",
  "env_spending", "env_spending_open", "2 milliards",
  "env_carbon_tax", "env_carbon_tax_open",
  "Oui si les taxes sont utilisées pour développer des technologies moins polluantes",
  "immi_take_jobs", "immi_take_jobs_open", "In the most basic sense, yes, but they also generate jobs."
)

# Look up each quote and the code the consensus gave it
quotes$matches <- NA_integer_
quotes$code <- NA_character_
quotes$category <- NA_character_
for (i in seq_len(nrow(quotes))) {
  hits <- which(trimws(consensus[[quotes$text_col[i]]]) == quotes$quote[i])
  code <- consensus[[paste0(quotes$question[i], "_ai")]][hits]
  labels <- attr(raw[[quotes$question[i]]], "labels")
  quotes$matches[i] <- length(hits)
  quotes$code[i] <- paste(code, collapse = ";")
  quotes$category[i] <- paste(ifelse(code %in% labels, names(labels)[match(code, labels)], "no position"),
                              collapse = ";")
}
out <- quotes %>% select(question, quote, matches, code, category)
stopifnot(all(out$matches >= 1))

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "results/tables/quotes.csv", row.names = FALSE)
print(as.data.frame(out))
