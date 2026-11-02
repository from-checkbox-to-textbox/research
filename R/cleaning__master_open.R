library(dplyr)
library(tidyr)
library(ggplot2)

# Consensus codes of the three providers (R/llm_03_consensus.R)
df <- readRDS("data/df_clean_consensus_3providers_10_1.rds")

vars_to_remove <- c(
  "ses_age",
  "ses_gender",
  "ses_province",
  "ses_dwelling",
  "ses_language",
  "ses_education",
  "ses_income",
  "env_spending",
  "env_regulation",
  "env_priority",
  "env_cause",
  "env_oil",
  "env_exaggerated",
  "env_carbon_tax",
  "immi_integration",
  "immi_take_jobs",
  "immi_spending",
  "immi_more",
  "immi_border",
  "vote_federal"
)

df_open <- df %>%
  filter(assignment == "open")

df_raw_base <- df_open %>%
  select(-all_of(vars_to_remove)) %>%
  rename(
    ses_age = ses_age_ai,
    ses_gender = ses_gender_ai,
    ses_province = ses_province_ai,
    ses_dwelling = ses_dwelling_ai,
    ses_language = ses_language_ai,
    ses_education = ses_education_ai,
    ses_income = ses_income_ai,
    env_spending = env_spending_ai,
    env_regulation = env_regulation_ai,
    env_priority = env_priority_ai,
    env_cause = env_cause_ai,
    env_oil = env_oil_ai,
    env_exaggerated = env_exaggerated_ai,
    env_carbon_tax = env_carbon_tax_ai,
    immi_integration = immi_integration_ai,
    immi_take_jobs = immi_take_jobs_ai,
    immi_spending = immi_spending_ai,
    immi_more = immi_more_ai,
    immi_border = immi_border_ai,
    vote_federal = vote_federal_ai
  )

# Substantive codes of each ordinal attitude question, in scale order
ordinal_codes <- list(
  env_spending = c(1, 4, 5),
  env_regulation = c(1, 4, 5, 6, 7),
  env_priority = c(1, 4, 5, 6, 7),
  env_oil = c(1, 4, 5, 6, 7),
  env_exaggerated = c(1, 4, 5, 6),
  env_carbon_tax = c(1, 4, 5, 6),
  immi_integration = c(1, 4, 5, 6, 7),
  immi_take_jobs = c(1, 4, 5, 6, 7),
  immi_spending = c(1, 4, 5),
  immi_more = c(1, 4, 5, 6, 7),
  immi_border = c(1, 4, 5, 6)
)

# When one provider gave no position and the other two chose different
# positions, the consensus code is the midpoint of those two positions, which
# can fall between two response categories. These answers are cleaned twice,
# once with each of the two provider codes, and the two cleaned values are
# averaged. The average equals the value of the midpoint because every recode
# of an ordinal question is equally spaced, which is checked first: each
# question is cleaned once with each of its codes, and every recoded variable
# that takes a different value for each code must change by equal steps.
for (v in names(ordinal_codes)) {
  df_raw <- df_raw_base[rep(1, length(ordinal_codes[[v]])), ]
  df_raw[[v]] <- ordinal_codes[[v]]
  df_clean <- data.frame(id = 1:nrow(df_raw))
  suppressWarnings({
    source("R/cleaning_ses.R")
    source("R/cleaning_env.R")
    source("R/cleaning_immi.R")
    source("R/cleaning_vote.R")
    source("R/cleaning_scales.R")
  })
  for (w in names(df_clean)) {
    x <- df_clean[[w]]
    if (is.numeric(x) && !anyNA(x) && length(unique(x)) == length(x)) {
      stopifnot(length(unique(round(diff(x), 10))) == 1)
    }
  }
}

# Lower and higher of the two provider codes behind each midpoint
df_raw_lo <- df_raw_base
df_raw_hi <- df_raw_base
for (v in names(ordinal_codes)) {
  for (i in which(df_open[[paste0(v, "_rule")]] == "midpoint")) {
    codes <- c(
      df_open[[paste0(v, "_ai_google")]][i],
      df_open[[paste0(v, "_ai_anthropic")]][i],
      df_open[[paste0(v, "_ai_openai")]][i]
    )
    codes <- codes[codes %in% ordinal_codes[[v]]]
    df_raw_lo[[v]][i] <- min(codes)
    df_raw_hi[[v]][i] <- max(codes)
  }
}

# Clean both versions with the same scripts as the closed-ended condition
df_raw <- df_raw_lo
df_clean <- data.frame(id = 1:nrow(df_raw))
source("R/cleaning_ses.R")
source("R/cleaning_env.R")
source("R/cleaning_immi.R")
source("R/cleaning_vote.R")
source("R/cleaning_scales.R")
clean_lo <- df_clean

df_raw <- df_raw_hi
df_clean <- data.frame(id = 1:nrow(df_raw))
source("R/cleaning_ses.R")
source("R/cleaning_env.R")
source("R/cleaning_immi.R")
source("R/cleaning_vote.R")
source("R/cleaning_scales.R")
clean_hi <- df_clean
stopifnot(identical(names(clean_lo), names(clean_hi)))

# Average the two cleaned values. Labels (e.g. immi_gov_spending_char) are
# kept only when both cleanings agree.
df_clean <- clean_lo
for (v in names(df_clean)) {
  if (is.numeric(df_clean[[v]])) {
    df_clean[[v]] <- (clean_lo[[v]] + clean_hi[[v]]) / 2
  } else {
    differ <- is.na(clean_lo[[v]]) != is.na(clean_hi[[v]]) |
      (!is.na(clean_lo[[v]]) & clean_lo[[v]] != clean_hi[[v]])
    df_clean[[v]][differ] <- NA
  }
}

# save data in RDS format
saveRDS(df_clean, "data/df_clean_open.rds")
