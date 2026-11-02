# Sample, random assignment and completion
# Script: sup_sample_counts.R
# Purpose: counts the respondents assigned to each condition, those who
#          completed the survey, the completion rates and the fieldwork
#          dates. Responses without an assignment (survey previews and
#          responses that never reached the randomizer) are not part of the
#          experiment.
# Output:  results/tables/sample_counts.csv

library(dplyr)
library(haven)

raw <- readRDS("data/raw/survey.rds")
assigned <- raw %>% filter(assignment %in% c("closed", "open"))

counts <- assigned %>%
  group_by(assignment) %>%
  summarise(assigned = n(), completed = sum(Finished == 1)) %>%
  mutate(completion_rate = completed / assigned)

out <- bind_rows(
  tibble(statistic = "responses without an assignment", value = nrow(raw) - nrow(assigned)),
  tibble(statistic = "respondents assigned", value = nrow(assigned)),
  tibble(statistic = paste(counts$assignment, "assigned"), value = counts$assigned),
  tibble(statistic = paste(counts$assignment, "completed"), value = counts$completed),
  tibble(statistic = paste(counts$assignment, "completion rate"), value = counts$completion_rate)
)
dates <- range(as.Date(assigned$StartDate))

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "results/tables/sample_counts.csv", row.names = FALSE)
print(as.data.frame(out), digits = 4)
cat("Fieldwork:", format(dates[1]), "to", format(dates[2]), "\n")
