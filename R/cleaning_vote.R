# Which party did you vote for during the 2025 federal election?
attributes(df_raw$vote_federal)
table(df_raw$vote_federal, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    vote_federal_char = case_when(
      df_raw$vote_federal == 8 ~ "lpc",
      df_raw$vote_federal == 9 ~ "cpc",
      df_raw$vote_federal == 10 ~ "ndp",
      df_raw$vote_federal == 11 ~ "bq",
      df_raw$vote_federal == 12 ~ "gpc",
      df_raw$vote_federal == 13 ~ "ppc",
      df_raw$vote_federal == 14 ~ "other_party",
      df_raw$vote_federal == 15 ~ "spoiled",
      df_raw$vote_federal == 16 ~ "note_vote",
    )
  )
table(df_clean$vote_federal_char, useNA = "always")

# vote federal parties
df_clean$vote_federal_lpc_bin <- ifelse(df_raw$vote_federal == 8, 1, 0)
table(df_clean$vote_federal_lpc_bin, useNA = "always")

df_clean$vote_federal_cpc_bin <- ifelse(df_raw$vote_federal == 9, 1, 0)
table(df_clean$vote_federal_cpc_bin, useNA = "always")

df_clean$vote_federal_ndp_bin <- ifelse(df_raw$vote_federal == 10, 1, 0)
table(df_clean$vote_federal_ndp_bin, useNA = "always")

df_clean$vote_federal_bq_bin <- ifelse(df_raw$vote_federal == 11, 1, 0)
table(df_clean$vote_federal_bq_bin, useNA = "always")

df_clean$vote_federal_gpc_bin <- ifelse(df_raw$vote_federal == 12, 1, 0)
table(df_clean$vote_federal_gpc_bin, useNA = "always")

df_clean$vote_federal_ppc_bin <- ifelse(df_raw$vote_federal == 13, 1, 0)
table(df_clean$vote_federal_ppc_bin, useNA = "always")

