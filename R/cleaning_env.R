# How much should the federal government spend on the environment?
attributes(df_raw$env_spending)
table(df_raw$env_spending, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    env_gov_spending_more_num = case_when(
      df_raw$env_spending == 1 ~ 0 / 2,
      df_raw$env_spending == 4 ~ 1 / 2,
      df_raw$env_spending == 5 ~ 2 / 2
    )
  )
table(df_clean$env_gov_spending_more_num, useNA = "always")

# Environmental regulation should be stricter, even if it leads to consumers having to pay higher prices.
attributes(df_raw$env_regulation)
table(df_raw$env_regulation, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    env_regulation_stricter_num = case_when(
      df_raw$env_regulation == 1 ~ 0 / 4,
      df_raw$env_regulation == 4 ~ 1 / 4,
      df_raw$env_regulation == 5 ~ 2 / 4,
      df_raw$env_regulation == 6 ~ 3 / 4,
      df_raw$env_regulation == 7 ~ 4 / 4
    )
  )
table(df_clean$env_regulation_num, useNA = "always")

# When there is a conflict between protecting the environment and creating jobs, jobs should come first.
attributes(df_raw$env_priority)
table(df_raw$env_priority, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    env_priority_over_jobs_num = case_when(
      df_raw$env_priority == 1 ~ 4 / 4,
      df_raw$env_priority == 4 ~ 3 / 4,
      df_raw$env_priority == 5 ~ 2 / 4,
      df_raw$env_priority == 6 ~ 1 / 4,
      df_raw$env_priority == 7 ~ 0 / 4
    )
  )
table(df_clean$env_priority_over_jobs_num, useNA = "always")

# jobs first
df_clean$env_jobs_first_bin <- ifelse(df_raw$env_priority %in% 6:7, 1, 0)
table(df_clean$env_jobs_first_bin, useNA = "always")

# What do you think is the main cause of climate change?
attributes(df_raw$env_cause)
table(df_raw$env_cause, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    env_climate_change_cause_char = case_when(
      df_raw$env_cause == 1 ~ "human",
      df_raw$env_cause == 4 ~ "natural"
    )
  )
table(df_clean$env_climate_change_cause_char, useNA = "always")

# cause human ("Don't know", code 6, is missing as for the other items; "Other", code 5, is 0)
df_clean$env_climate_cause_human_bin <- ifelse(df_raw$env_cause == 6, NA, ifelse(df_raw$env_cause == 1, 1, 0))
table(df_clean$env_climate_cause_human_bin, useNA = "always")

# To what extent should the government support the oil industry? 
attributes(df_raw$env_oil)
table(df_raw$env_oil, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    env_against_oil_support_num = case_when(
      df_raw$env_oil == 1 ~ 4 / 4,
      df_raw$env_oil == 4 ~ 3 / 4,
      df_raw$env_oil == 5 ~ 2 / 4,
      df_raw$env_oil == 6 ~ 1 / 4,
      df_raw$env_oil == 7 ~ 0 / 4
    )
  )
table(df_clean$env_against_oil_support_num, useNA = "always")

# support oil
df_clean$env_support_oil_bin <- ifelse(df_raw$env_oil %in% 6:7, 1, 0)
table(df_clean$env_support_oil_bin, useNA = "always")

# The threat of climate change is exaggerated
attributes(df_raw$env_exaggerated)
table(df_raw$env_exaggerated, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    env_climate_change_not_exaggerated_num = case_when(
      df_raw$env_exaggerated == 1 ~ 3 / 3, # four categories (1, 4, 5, 6)
      df_raw$env_exaggerated == 4 ~ 2 / 3,
      df_raw$env_exaggerated == 5 ~ 1 / 3,
      df_raw$env_exaggerated == 6 ~ 0 / 3
    )
  )
table(df_clean$env_climate_change_not_exaggerated_num, useNA = "always")

# exaggerated
df_clean$env_climate_change_exaggerated_bin <- ifelse(df_raw$env_exaggerated %in% 5:6, 1, 0) # agree (5, 6) on the four-category item
table(df_clean$env_climate_change_exaggerated_bin, useNA = "always")

# The federal carbon tax should be maintained as an environmental measure
attributes(df_raw$env_carbon_tax)
table(df_raw$env_carbon_tax, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    env_carbon_tax_maintained_num = case_when(
      df_raw$env_carbon_tax == 1 ~ 0 / 3,
      df_raw$env_carbon_tax == 4 ~ 1 / 3,
      df_raw$env_carbon_tax == 5 ~ 2 / 3,
      df_raw$env_carbon_tax == 6 ~ 3 / 3,
    )
  )
table(df_clean$env_carbon_tax_maintained_num, useNA = "always")

