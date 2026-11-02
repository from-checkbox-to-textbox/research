# Composition of completers by condition
# Script: sup_completer_composition.R
# Purpose: compares the socio-demographic profile of the respondents who
#          completed the open-ended and the closed-ended survey (the balance
#          table of the appendix). Dwelling type is condo, townhouse or
#          detached house against any other dwelling.
# Output:  results/tables/completer_composition.csv
#          results/tables/completer_composition.tex

library(dplyr)
library(tidyr)

data <- readRDS("data/df_clean_merged.rds")

covariates <- c(
  "ses_age_num",
  "ses_age_under_34_bin", "ses_age_35_54_bin", "ses_age_55_plus_bin",
  "ses_female_bin", "ses_qc_bin", "ses_owner_bin", "ses_english_bin",
  "ses_education_bhs_bin", "ses_education_hs_bin", "ses_education_univ_bin",
  "ses_income_low_bin", "ses_income_mid_bin", "ses_income_high_bin"
)

labels <- c(
  ses_age_num = "Age (years)",
  ses_age_under_34_bin = "Age 18-34",
  ses_age_35_54_bin = "Age 35-54",
  ses_age_55_plus_bin = "Age 55 and over",
  ses_female_bin = "Female",
  ses_qc_bin = "Quebec",
  ses_owner_bin = "Dwelling: condo, townhouse or detached house",
  ses_english_bin = "English speaker",
  ses_education_bhs_bin = "No high school diploma",
  ses_education_hs_bin = "High school diploma (no university degree)",
  ses_education_univ_bin = "University degree",
  ses_income_low_bin = "Income: low",
  ses_income_mid_bin = "Income: middle",
  ses_income_high_bin = "Income: high"
)

# One row per variable: means, standardized difference (difference in means
# over the pooled standard deviation) and p-value (t-test for age, chi-squared
# test for the indicators)
composition <- tibble(variable = covariates, label = labels[covariates])
for (i in seq_along(covariates)) {
  v <- covariates[i]
  x_closed <- data[[v]][data$treatment == "closed"]
  x_open <- data[[v]][data$treatment == "open"]
  x_closed <- x_closed[!is.na(x_closed)]
  x_open <- x_open[!is.na(x_open)]

  composition$n_closed[i] <- length(x_closed)
  composition$n_open[i] <- length(x_open)
  composition$mean_closed[i] <- mean(x_closed)
  composition$mean_open[i] <- mean(x_open)
  composition$smd[i] <- (mean(x_open) - mean(x_closed)) / sqrt((sd(x_closed)^2 + sd(x_open)^2) / 2)
  if (v == "ses_age_num") {
    composition$p_value[i] <- t.test(x_open, x_closed)$p.value
  } else {
    condition <- c(rep("closed", length(x_closed)), rep("open", length(x_open)))
    composition$p_value[i] <- suppressWarnings(chisq.test(table(condition, c(x_closed, x_open)))$p.value)
  }
}
composition$label <- unname(composition$label)

print(composition %>% mutate(across(where(is.numeric), ~ round(.x, 3))), n = 20)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(composition, "results/tables/completer_composition.csv", row.names = FALSE)

# LaTeX table for the appendix (the caption and note are in the article)
lines <- c(
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  " & \\textbf{Closed-ended} & \\textbf{Open-ended} & \\textbf{SMD} & \\textbf{\\textit{p}} \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(composition))) {
  r <- composition[i, ]
  # Percentages, except age (mean in years)
  scale_by <- if (r$variable == "ses_age_num") 1 else 100
  pv <- if (r$p_value < 0.001) "<.001" else sub("^0", "", sprintf("%.3f", r$p_value))
  lines <- c(lines, sprintf("%s & %.1f & %.1f & %.3f & %s \\\\",
                            r$label, scale_by * r$mean_closed, scale_by * r$mean_open, r$smd, pv))
}
lines <- c(lines, "\\bottomrule", "\\end{tabular}")
writeLines(lines, "results/tables/completer_composition.tex")
