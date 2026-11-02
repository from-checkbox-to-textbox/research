# Where respondents who did not complete the survey left it
# Script: sup_attrition_stage.R
# Purpose: among the respondents assigned to each condition who did not
#          complete the survey, counts those who left within the
#          socio-demographic block, after it but before answering any
#          attitude question, and during the attitude questions. The
#          socio-demographic block comes first and ends with income; an
#          answer counts when it is not empty.
# Output:  results/tables/attrition_stage.csv

library(dplyr)
library(haven)

raw <- readRDS("data/raw/survey.rds") %>%
  filter(assignment %in% c("closed", "open"), Finished != 1)

attitude <- c(
  "env_spending", "env_regulation", "env_priority", "env_cause", "env_oil",
  "env_exaggerated", "env_carbon_tax", "immi_integration", "immi_take_jobs",
  "immi_spending", "immi_more", "immi_border"
)
open_text <- paste0(attitude, "_open")
open_text[open_text == "immi_integration_open"] <- "immi_integration_ope"

# An answer counts when it is not empty. Income is the last socio-demographic
# question. In the open-ended condition the answers are in the text columns.
closed <- raw %>% filter(assignment == "closed")
closed_income <- !is.na(closed$ses_income)
closed_attitude <- rowSums(!is.na(closed[, attitude])) > 0

open <- raw %>% filter(assignment == "open")
open_answered <- as.data.frame(lapply(open[, c("ses_income_open", open_text)],
                                      function(x) !is.na(x) & trimws(x) != ""))
open_income <- open_answered$ses_income_open
open_attitude <- rowSums(open_answered[, open_text]) > 0

closed$stage <- case_when(
  closed_attitude ~ "during the attitude questions",
  closed_income ~ "after the socio-demographic block",
  TRUE ~ "within the socio-demographic block"
)
open$stage <- case_when(
  open_attitude ~ "during the attitude questions",
  open_income ~ "after the socio-demographic block",
  TRUE ~ "within the socio-demographic block"
)

out <- bind_rows(
  closed %>% count(condition = assignment, stage),
  open %>% count(condition = assignment, stage)
)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "results/tables/attrition_stage.csv", row.names = FALSE)
print(as.data.frame(out))
