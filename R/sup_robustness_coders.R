# Main results with each provider's codes and with the earlier Gemini coding
# Script: sup_robustness_coders.R
# Purpose: rebuilds the open-ended condition with the codes of each provider
#          alone (Google, Anthropic, OpenAI) and with the 2025 coding by
#          Gemini 2.0 Flash-Lite, and compares each version with the closed-
#          ended condition as in the paper: one-factor model on complete cases
#          (Cronbach's alpha, first eigenvalue, loadings) and, for the 22
#          regressions of a scale on one predictor, the sign of the coefficient
#          and the test of the difference between conditions on scales
#          standardized within condition. The consensus codes used in the
#          paper are the first row. Reads the coded files and never writes to
#          them.
# Output:  results/tables/robustness_coders.csv
#          results/tables/robustness_coders.tex

library(dplyr)
library(tidyr)
library(psych)

consensus <- readRDS("data/df_clean_consensus_3providers_10_1.rds") %>%
  filter(assignment == "open")
gemini_2025 <- readRDS("data/df_clean_gemini_2_0_flash_lite_10_1.rds") %>%
  filter(assignment == "open")
closed <- readRDS("data/df_clean_closed.rds")

vars <- c(
  "ses_age", "ses_gender", "ses_province", "ses_dwelling", "ses_language",
  "ses_education", "ses_income", "env_spending", "env_regulation",
  "env_priority", "env_cause", "env_oil", "env_exaggerated", "env_carbon_tax",
  "immi_integration", "immi_take_jobs", "immi_spending", "immi_more",
  "immi_border", "vote_federal"
)
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

# The five codings of the open-ended answers: the column suffix of each
# question's codes and the data they come from
codings <- tibble(
  coding = c("Consensus of the three providers", "Google (Gemini 3.5 Flash-Lite)",
             "Anthropic (Claude Haiku 5.5)", "OpenAI (GPT-OSS-20B)",
             "Gemini 2.0 Flash-Lite (2025)"),
  suffix = c(NA, "_ai_google", "_ai_anthropic", "_ai_openai", "_ai")
)

# Closed-ended condition: first row of the table
results <- tibble(coding = "Closed-ended condition")
for (s in c("env", "immi")) {
  d <- closed[, if (s == "env") env_items else immi_items]
  d <- d[complete.cases(d), ]
  f <- fa(d, nfactors = 1, rotate = "oblimin", fm = "ml")
  results[[paste0(s, "_n")]] <- nrow(d)
  results[[paste0(s, "_alpha")]] <- psych::alpha(d)$total$raw_alpha
  results[[paste0(s, "_eigen")]] <- f$e.values[1]
  results[[paste0(s, "_min_loading")]] <- min(as.numeric(f$loadings))
}

for (k in seq_len(nrow(codings))) {
  # Clean the open-ended condition with this coding. The consensus version is
  # the cleaned file of the paper. The others replace each question's codes
  # and run the same cleaning scripts as cleaning__master_open.R.
  if (is.na(codings$suffix[k])) {
    open <- readRDS("data/df_clean_open.rds")
  } else {
    df_raw <- if (codings$coding[k] == "Gemini 2.0 Flash-Lite (2025)") gemini_2025 else consensus
    for (v in vars) df_raw[[v]] <- df_raw[[paste0(v, codings$suffix[k])]]
    df_clean <- data.frame(id = 1:nrow(df_raw))
    suppressWarnings({
      source("R/cleaning_ses.R")
      source("R/cleaning_env.R")
      source("R/cleaning_immi.R")
      source("R/cleaning_vote.R")
      source("R/cleaning_scales.R")
    })
    open <- df_clean
  }

  df <- bind_rows(mutate(open, treatment = "open"), mutate(closed, treatment = "closed")) %>%
    mutate(open = as.integer(treatment == "open")) %>%
    group_by(treatment) %>%
    mutate(env_scale_std = as.numeric(scale(env_scale)),
           immi_scale_std = as.numeric(scale(immi_scale))) %>%
    ungroup()

  row <- tibble(coding = codings$coding[k])

  # One-factor model on complete cases, in each condition
  for (s in c("env", "immi")) {
    items <- if (s == "env") env_items else immi_items
    d_open <- df[df$treatment == "open", items]
    d_open <- d_open[complete.cases(d_open), ]
    d_closed <- df[df$treatment == "closed", items]
    d_closed <- d_closed[complete.cases(d_closed), ]
    f_open <- fa(d_open, nfactors = 1, rotate = "oblimin", fm = "ml")
    f_closed <- fa(d_closed, nfactors = 1, rotate = "oblimin", fm = "ml")
    row[[paste0(s, "_n")]] <- nrow(d_open)
    row[[paste0(s, "_alpha")]] <- psych::alpha(d_open)$total$raw_alpha
    row[[paste0(s, "_eigen")]] <- f_open$e.values[1]
    row[[paste0(s, "_min_loading")]] <- min(as.numeric(f_open$loadings))
    row[[paste0(s, "_loading_gap")]] <- mean(as.numeric(f_closed$loadings) - as.numeric(f_open$loadings))
  }

  # Regressions of each scale on each predictor: same sign in both conditions,
  # and test of the difference on the standardized scales
  same_sign <- 0
  not_significant <- 0
  for (scale_var in c("env_scale", "immi_scale")) {
    for (x in predictors) {
      f <- as.formula(paste(scale_var, "~", x))
      b_closed <- coef(lm(f, data = filter(df, treatment == "closed")))[2]
      b_open <- coef(lm(f, data = filter(df, treatment == "open")))[2]
      same_sign <- same_sign + (sign(b_closed) == sign(b_open))
      m <- lm(as.formula(paste0(scale_var, "_std ~ ", x, " * open")), data = df)
      not_significant <- not_significant + (coef(summary(m))[paste0(x, ":open"), 4] >= .05)
    }
  }
  row$same_sign <- unname(same_sign)
  row$differences_not_significant <- unname(not_significant)

  results <- bind_rows(results, row)
}

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(results, "results/tables/robustness_coders.csv", row.names = FALSE)
print(as.data.frame(results), digits = 3)

# LaTeX table for the appendix (empty cells for the closed-ended row)
num_text <- results %>%
  mutate(across(c(env_alpha, immi_alpha), ~ formatC(.x, format = "f", digits = 3)),
         across(c(env_eigen, env_min_loading, immi_eigen, immi_min_loading),
                ~ formatC(.x, format = "f", digits = 2)),
         same_sign = ifelse(is.na(same_sign), "", paste0(same_sign, "/22")),
         differences_not_significant = ifelse(is.na(differences_not_significant), "",
                                              paste0(differences_not_significant, "/22")))
rows <- sprintf(
  "%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
  num_text$coding,
  num_text$env_alpha, num_text$env_eigen, num_text$env_min_loading,
  num_text$immi_alpha, num_text$immi_eigen, num_text$immi_min_loading,
  num_text$same_sign, num_text$differences_not_significant
)
tex <- c(
  "\\begin{tabular}{lrrrrrrrr}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Environment} & \\multicolumn{3}{c}{Immigration} & \\multicolumn{2}{c}{Regressions} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-9}",
  "Coding & $\\alpha$ & $\\lambda_1$ & Min. loading & $\\alpha$ & $\\lambda_1$ & Min. loading & Same sign & Not significant \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tex, "results/tables/robustness_coders.tex")
