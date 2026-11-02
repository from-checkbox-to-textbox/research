# Missing answers to the twelve attitude questions, by education and condition
# Script: sup_missing_by_education.R
# Purpose: among respondents who completed the survey, compare the share of
#          answers without a position on the response scale between
#          respondents with and without a university degree, in each
#          condition. Tests whether the gap between conditions differs by
#          education with an interaction between the condition and the
#          university degree. Respondents whose education is unknown are left
#          out.
# Output:  results/tables/missing_by_education.csv
#          results/tables/missing_by_education.tex

library(dplyr)

merged <- readRDS("data/df_clean_merged.rds")

items <- c(
  "env_gov_spending_more_num", "env_regulation_stricter_num",
  "env_priority_over_jobs_num", "env_climate_cause_human_bin",
  "env_against_oil_support_num", "env_climate_change_not_exaggerated_num",
  "env_carbon_tax_maintained_num", "immi_dont_fit_in_num", "immi_take_jobs_num",
  "immi_gov_spending_less_num", "immi_admit_less_num", "immi_stricter_border_num"
)

# Share of the twelve answers that are missing, for each respondent
df <- merged %>%
  filter(!is.na(ses_education_char)) %>%
  mutate(
    university = ifelse(ses_education_univ_bin == 1, "University degree", "No university degree"),
    share_missing = rowSums(is.na(across(all_of(items)))) / length(items)
  )
cat("Respondents left out for unknown education:", nrow(merged) - nrow(df), "\n")

# Share of missing answers by condition and education
by_group <- df %>%
  group_by(treatment, university) %>%
  summarise(
    respondents = n(),
    answers = n() * length(items),
    missing = sum(share_missing * length(items)),
    .groups = "drop"
  ) %>%
  mutate(missing_pct = 100 * missing / answers)
print(as.data.frame(by_group))

# Does the gap between conditions differ by education?
model <- lm(share_missing ~ treatment * ses_education_univ_bin, data = df)
interaction <- summary(model)$coefficients["treatmentopen:ses_education_univ_bin", ]
ci <- confint(model)["treatmentopen:ses_education_univ_bin", ]
print(summary(model)$coefficients)

gap <- by_group %>%
  select(treatment, university, missing_pct) %>%
  tidyr::pivot_wider(names_from = treatment, values_from = missing_pct) %>%
  mutate(gap = open - closed)
print(as.data.frame(gap))

out <- by_group %>%
  left_join(gap %>% select(university, gap), by = "university") %>%
  mutate(
    interaction_pts = 100 * interaction[["Estimate"]],
    interaction_ci_low = 100 * ci[[1]],
    interaction_ci_high = 100 * ci[[2]],
    interaction_p = interaction[["Pr(>|t|)"]]
  )
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "results/tables/missing_by_education.csv", row.names = FALSE)

# LaTeX table for the appendix
wide <- by_group %>%
  select(treatment, university, respondents, missing_pct) %>%
  tidyr::pivot_wider(names_from = treatment, values_from = c(respondents, missing_pct)) %>%
  left_join(gap %>% select(university, gap), by = "university")
rows <- sprintf(
  "%s & %d & %.1f & %d & %.1f & %.1f \\\\",
  wide$university, wide$respondents_open, wide$missing_pct_open,
  wide$respondents_closed, wide$missing_pct_closed, wide$gap
)
tex <- c(
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Open-ended} & \\multicolumn{2}{c}{Closed-ended} & \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  " & Respondents & Missing (\\%) & Respondents & Missing (\\%) & Difference \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tex, "results/tables/missing_by_education.tex")
