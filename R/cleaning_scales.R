# All seven environment items enter the scale (government spending included).
env_scale <- df_clean %>%
  select(starts_with("env_")) %>%
  select(
    env_gov_spending_more_num,
    env_regulation_stricter_num,
    env_priority_over_jobs_num,
    env_climate_cause_human_bin,
    env_against_oil_support_num,
    env_climate_change_not_exaggerated_num,
    env_carbon_tax_maintained_num
  ) %>%
  drop_na()

df_clean <- df_clean %>%
  mutate(
    env_scale = rowMeans(
      across(c(env_gov_spending_more_num,
               env_regulation_stricter_num,
               env_priority_over_jobs_num,
               env_climate_cause_human_bin,
               env_against_oil_support_num,
               env_climate_change_not_exaggerated_num,
               env_carbon_tax_maintained_num)),
      na.rm = TRUE
    )
  )

immi_scale <- df_clean %>%
  select(starts_with("immi_")) %>%
  select(
    immi_dont_fit_in_num,
    immi_take_jobs_num,
    immi_gov_spending_less_num,
    immi_admit_less_num,
    immi_stricter_border_num
  ) %>%
  drop_na()

df_clean <- df_clean %>%
  mutate(
    immi_scale = rowMeans(
      across(c(immi_dont_fit_in_num,
               immi_take_jobs_num,
               immi_gov_spending_less_num,
               immi_admit_less_num,
               immi_stricter_border_num)),
      na.rm = TRUE
    )
  )
