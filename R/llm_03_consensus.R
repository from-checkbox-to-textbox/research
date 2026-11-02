# Three-provider consensus code for each open-ended answer
# Script: llm_03_consensus.R
# Purpose: combine the codes assigned by the three coding pipelines (Google
#          Gemini 3.5 Flash-Lite, Anthropic Claude Haiku 5.5, OpenAI GPT-OSS-20B;
#          see llm/) into one code per answer. The code kept is the one at least
#          two of the three providers agree on. When all three differ:
#            - ordinal attitude items: the middle code, if all three are
#              substantive answers (no "don't know"). If one provider coded
#              "don't know" and the other two chose different positions, the
#              midpoint of those two positions on the ordinal scale. A
#              midpoint that falls between two response categories is stored
#              as the mean of their two codes (e.g. 6.5 between 6 and 7), so
#              the cleaning must recode it as the mean of their two values;
#            - age: the median of the three values;
#            - nominal items (cause of climate change, socio-demographics,
#              vote): missing.
#          An answer coded by only one provider keeps that code and is flagged.
#          Each item also gets *_agree_n (number of providers giving the kept
#          code) and *_rule (how the code was obtained), and each provider's
#          own code is kept as *_ai_google, *_ai_anthropic, *_ai_openai.
# Output:  data/df_clean_consensus_3providers_10_1.rds
#          results/tables/consensus_agreement.csv

library(dplyr)
library(tidyr)

runs <- c(
  google    = "data/df_clean_gemini_3_5_flash_lite_10_1.rds",
  anthropic = "data/df_clean_claude_haiku_5_5_10_1.rds",
  openai    = "data/df_clean_openai_gpt_oss_20b_10_1.rds"
)
dfs <- lapply(runs, readRDS)
stopifnot(identical(dfs$google$ResponseId, dfs$anthropic$ResponseId),
          identical(dfs$google$ResponseId, dfs$openai$ResponseId))

ai_vars <- grep("_ai$", names(dfs$google), value = TRUE)

# Ordinal position of the substantive codes ("don't know" left out), as in
# sup_cross_model_agreement.R
ordinal_codes <- list(
  env_spending_ai     = c(1, 4, 5),
  env_regulation_ai   = c(1, 4, 5, 6, 7),
  env_priority_ai     = c(1, 4, 5, 6, 7),
  env_oil_ai          = c(1, 4, 5, 6, 7),
  env_exaggerated_ai  = c(1, 4, 5, 6),
  env_carbon_tax_ai   = c(1, 4, 5, 6),
  immi_integration_ai = c(1, 4, 5, 6, 7),
  immi_take_jobs_ai   = c(1, 4, 5, 6, 7),
  immi_spending_ai    = c(1, 4, 5),
  immi_more_ai        = c(1, 4, 5, 6, 7),
  immi_border_ai      = c(1, 4, 5, 6)
)

out <- dfs$google %>% select(-all_of(ai_vars))
summary_rows <- list()

for (v in ai_vars) {
  codes <- cbind(google = dfs$google[[v]], anthropic = dfs$anthropic[[v]], openai = dfs$openai[[v]])
  code <- rep(NA_real_, nrow(codes))
  agree_n <- rep(NA_integer_, nrow(codes))
  rule <- rep(NA_character_, nrow(codes))

  # Consensus rule for each answer, in this order
  for (i in seq_len(nrow(codes))) {
    x <- codes[i, ]
    x <- x[!is.na(x)]
    counts <- table(x)
    if (length(x) == 0) {
      agree_n[i] <- 0L
      rule[i] <- "none"
    } else if (length(x) == 1) {
      # Only one provider returned a code: keep it
      code[i] <- x
      agree_n[i] <- 1L
      rule[i] <- "one_provider"
    } else if (max(counts) >= 2) {
      # At least two providers agree
      code[i] <- as.numeric(names(counts)[which.max(counts)])
      agree_n[i] <- max(counts)
      rule[i] <- "majority"
    } else if (v == "ses_age_ai" && length(x) == 3) {
      # Age: median of the three values
      code[i] <- median(x)
      agree_n[i] <- 1L
      rule[i] <- "median"
    } else if (v %in% names(ordinal_codes) && length(x) == 3 && all(x %in% ordinal_codes[[v]])) {
      # Three different positions on an ordinal scale: the middle one
      code[i] <- sort(x)[2]
      agree_n[i] <- 1L
      rule[i] <- "middle"
    } else if (v %in% names(ordinal_codes) && length(x) == 3 && sum(x %in% ordinal_codes[[v]]) == 2) {
      # One provider coded "don't know", the other two chose different
      # positions: the midpoint of those two positions
      scale_codes <- ordinal_codes[[v]]
      position <- mean(match(x[x %in% scale_codes], scale_codes))
      code[i] <- mean(scale_codes[c(floor(position), ceiling(position))])
      agree_n[i] <- 1L
      rule[i] <- "midpoint"
    } else {
      # Nominal question and no two providers agree
      agree_n[i] <- 1L
      rule[i] <- "no_majority"
    }
  }

  out[[v]] <- code
  out[[sub("_ai$", "_agree_n", v)]] <- agree_n
  out[[sub("_ai$", "_rule", v)]] <- rule
  for (p in names(runs)) out[[paste0(v, "_", p)]] <- codes[, p]

  coded <- rule != "none"
  summary_rows[[v]] <- tibble(
    item = sub("_ai$", "", v),
    n_answers = sum(coded),
    all_three_agree = mean(agree_n[coded] == 3),
    two_of_three = mean(agree_n[coded] == 2 & rule[coded] == "majority"),
    middle_or_median = mean(rule[coded] %in% c("middle", "median")),
    midpoint = mean(rule[coded] == "midpoint"),
    no_majority = mean(rule[coded] == "no_majority"),
    one_provider = mean(rule[coded] == "one_provider")
  )
}

summary_tab <- bind_rows(summary_rows)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(summary_tab, "results/tables/consensus_agreement.csv", row.names = FALSE)
saveRDS(out, "data/df_clean_consensus_3providers_10_1.rds")

print(summary_tab %>% mutate(across(where(is.double), ~ round(.x, 3))), n = Inf)
cat("\nSaved data/df_clean_consensus_3providers_10_1.rds\n")
