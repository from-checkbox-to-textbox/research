# Numbers reported in the Results section
# Script: sup_results_numbers.R
# Purpose: computes, in one place, the factor analysis and regression numbers
#          quoted in the text, with the same models as
#          fig_facanal_dumbell_12vars.R, fig_facanal_dumbell_12vars_pairwise.R
#          and fig_regression_difference.R.
# Output:  results/tables/results_numbers.csv

library(dplyr)
library(psych)

df <- readRDS("data/df_clean_merged.rds")

env_items <- c(
  "env_gov_spending_more_num", "env_regulation_stricter_num",
  "env_priority_over_jobs_num", "env_climate_cause_human_bin",
  "env_against_oil_support_num", "env_climate_change_not_exaggerated_num",
  "env_carbon_tax_maintained_num"
)
immi_items <- c(
  "immi_dont_fit_in_num", "immi_take_jobs_num", "immi_gov_spending_less_num",
  "immi_admit_less_num", "immi_stricter_border_num"
)
predictors <- c(
  "ses_age_55_plus_bin", "ses_female_bin", "ses_qc_bin", "ses_owner_bin",
  "ses_english_bin", "ses_education_univ_bin", "ses_income_high_bin",
  "vote_federal_lpc_bin", "vote_federal_cpc_bin", "vote_federal_ndp_bin",
  "vote_federal_bq_bin"
)

# Each number is added as one row of this table
numbers <- data.frame(statistic = character(0), value = numeric(0))

# Factor structure and reliability --------------------------------------------
# One-factor model on complete cases (as in the paper) and on pairwise
# correlations (robustness footnote), for each scale and condition
max_loading_change <- 0
max_alpha_change <- 0
max_eigen_change <- 0
for (s in c("env", "immi")) {
  items <- if (s == "env") env_items else immi_items
  loadings <- list()
  for (cnd in c("closed", "open")) {
    d <- df[df$treatment == cnd, items]
    complete <- d[complete.cases(d), ]
    pairwise <- d[rowSums(!is.na(d)) > 0, ]

    fa_complete <- fa(complete, nfactors = 1, rotate = "oblimin", fm = "ml")
    fa_pairwise <- fa(pairwise, nfactors = 1, rotate = "oblimin", fm = "ml")
    alpha_complete <- psych::alpha(complete)$total$raw_alpha
    alpha_pairwise <- psych::alpha(pairwise)$total$raw_alpha
    loadings[[cnd]] <- as.numeric(fa_complete$loadings)

    numbers[nrow(numbers) + 1, ] <- list(paste(s, cnd, "n complete cases"), nrow(complete))
    numbers[nrow(numbers) + 1, ] <- list(paste(s, cnd, "alpha"), alpha_complete)
    numbers[nrow(numbers) + 1, ] <- list(paste(s, cnd, "first eigenvalue"), fa_complete$e.values[1])

    max_loading_change <- max(max_loading_change,
                              abs(as.numeric(fa_complete$loadings) - as.numeric(fa_pairwise$loadings)))
    max_alpha_change <- max(max_alpha_change, abs(alpha_complete - alpha_pairwise))
    max_eigen_change <- max(max_eigen_change, abs(fa_complete$e.values[1] - fa_pairwise$e.values[1]))
  }
  numbers[nrow(numbers) + 1, ] <- list(paste(s, "mean loading gap (closed minus open)"),
                                       mean(loadings$closed - loadings$open))
  numbers[nrow(numbers) + 1, ] <- list(paste(s, "minimum loading"), min(loadings$closed, loadings$open))
}
numbers[nrow(numbers) + 1, ] <- list("pairwise max loading change", max_loading_change)
numbers[nrow(numbers) + 1, ] <- list("pairwise max alpha change", max_alpha_change)
numbers[nrow(numbers) + 1, ] <- list("pairwise max first eigenvalue change", max_eigen_change)

# Relations with other variables ----------------------------------------------
df <- df |>
  mutate(open = as.integer(treatment == "open")) |>
  group_by(treatment) |>
  mutate(env_scale_std = as.numeric(scale(env_scale)),
         immi_scale_std = as.numeric(scale(immi_scale))) |>
  ungroup()
df_closed <- filter(df, treatment == "closed")
df_open <- filter(df, treatment == "open")

# For each scale and predictor: the coefficient in each condition (0-1 scale),
# whether it is significant, the standardized coefficient in each condition,
# and the test of the difference on scales standardized within condition
slopes <- expand.grid(scale = c("env_scale", "immi_scale"), predictor = predictors,
                      stringsAsFactors = FALSE)
slopes[c("closed", "open", "std_closed", "std_open", "difference", "low", "high", "p")] <- NA_real_
slopes$sig_both <- NA
for (i in seq_len(nrow(slopes))) {
  f <- as.formula(paste(slopes$scale[i], "~", slopes$predictor[i]))
  f_std <- as.formula(paste0(slopes$scale[i], "_std ~ ", slopes$predictor[i]))
  m_closed <- lm(f, data = df_closed)
  m_open <- lm(f, data = df_open)
  ci_closed <- confint(m_closed)[2, ]
  ci_open <- confint(m_open)[2, ]
  slopes$closed[i] <- coef(m_closed)[2]
  slopes$open[i] <- coef(m_open)[2]
  slopes$sig_both[i] <- (ci_closed[1] > 0 | ci_closed[2] < 0) & (ci_open[1] > 0 | ci_open[2] < 0)
  slopes$std_closed[i] <- coef(lm(f_std, data = df_closed))[2]
  slopes$std_open[i] <- coef(lm(f_std, data = df_open))[2]

  m <- lm(as.formula(paste0(slopes$scale[i], "_std ~ ", slopes$predictor[i], " * open")), data = df)
  k <- paste0(slopes$predictor[i], ":open")
  slopes$difference[i] <- coef(m)[k]
  slopes$low[i] <- confint(m)[k, 1]
  slopes$high[i] <- confint(m)[k, 2]
  slopes$p[i] <- coef(summary(m))[k, 4]
}

numbers[nrow(numbers) + 1, ] <- list("significant in both conditions", sum(slopes$sig_both))
numbers[nrow(numbers) + 1, ] <- list("significant in both conditions with the same sign",
                                     sum(slopes$sig_both & sign(slopes$closed) == sign(slopes$open)))
numbers[nrow(numbers) + 1, ] <- list("sign changes", sum(sign(slopes$closed) != sign(slopes$open)))
numbers[nrow(numbers) + 1, ] <- list("standardized differences not significant at .05", sum(slopes$p >= .05))
numbers[nrow(numbers) + 1, ] <- list("median half-width of the 95% CI of the standardized differences",
                                     median((slopes$high - slopes$low) / 2))

# Details for the Conservative vote and for the significant differences
for (i in which(slopes$predictor == "vote_federal_cpc_bin" | slopes$p < .05)) {
  lab <- paste(slopes$scale[i], slopes$predictor[i])
  numbers[nrow(numbers) + 1, ] <- list(paste(lab, "coefficient closed"), slopes$closed[i])
  numbers[nrow(numbers) + 1, ] <- list(paste(lab, "coefficient open"), slopes$open[i])
  numbers[nrow(numbers) + 1, ] <- list(paste(lab, "standardized coefficient closed"), slopes$std_closed[i])
  numbers[nrow(numbers) + 1, ] <- list(paste(lab, "standardized coefficient open"), slopes$std_open[i])
  numbers[nrow(numbers) + 1, ] <- list(paste(lab, "standardized difference"), slopes$difference[i])
  numbers[nrow(numbers) + 1, ] <- list(paste(lab, "standardized difference CI low"), slopes$low[i])
  numbers[nrow(numbers) + 1, ] <- list(paste(lab, "standardized difference CI high"), slopes$high[i])
  numbers[nrow(numbers) + 1, ] <- list(paste(lab, "standardized difference p"), slopes$p[i])
}

# Tests on the unstandardized scales come from sup_regression_interactions.R
raw <- read.csv("results/tables/regression_interactions.csv")
numbers[nrow(numbers) + 1, ] <- list("unstandardized differences significant at .05",
                                     sum(raw$units == "raw" & raw$p < .05))

# Spread of each scale
numbers[nrow(numbers) + 1, ] <- list("env scale SD closed", sd(df_closed$env_scale, na.rm = TRUE))
numbers[nrow(numbers) + 1, ] <- list("immi scale SD closed", sd(df_closed$immi_scale, na.rm = TRUE))
numbers[nrow(numbers) + 1, ] <- list("env scale SD open", sd(df_open$env_scale, na.rm = TRUE))
numbers[nrow(numbers) + 1, ] <- list("immi scale SD open", sd(df_open$immi_scale, na.rm = TRUE))

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(numbers, "results/tables/results_numbers.csv", row.names = FALSE)
print(numbers, digits = 3)
