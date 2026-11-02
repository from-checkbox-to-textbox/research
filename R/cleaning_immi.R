# Too many recent immigrants just don't want to fit in Canadian society.
attributes(df_raw$immi_integration)
table(df_raw$immi_integration, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    immi_dont_fit_in_num = case_when(
      df_raw$immi_integration == 1 ~ 0 / 4,
      df_raw$immi_integration == 4 ~ 1 / 4,
      df_raw$immi_integration == 5 ~ 2 / 4,
      df_raw$immi_integration == 6 ~ 3 / 4,
      df_raw$immi_integration == 7 ~ 4 / 4,
    )
  )
table(df_clean$immi_dont_fit_in_num, useNA = "always")

# Immigrants take jobs away from Canadians.
attributes(df_raw$immi_take_jobs)
table(df_raw$immi_take_jobs, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    immi_take_jobs_num = case_when(
      df_raw$immi_take_jobs == 1 ~ 0 / 4,
      df_raw$immi_take_jobs == 4 ~ 1 / 4,
      df_raw$immi_take_jobs == 5 ~ 2 / 4,
      df_raw$immi_take_jobs == 6 ~ 3 / 4,
      df_raw$immi_take_jobs == 7 ~ 4 / 4,
    )
  )
table(df_clean$immi_take_jobs_num, useNA = "always")

# immi take jobs bin
df_clean$immi_take_jobs_bin <- ifelse(df_raw$immi_take_jobs %in% 6:7, 1, 0)
table(df_clean$immi_take_jobs_bin, useNA = "always")

# How much should the federal government spend on immigrants and minorities?
attributes(df_raw$immi_spending)
table(df_raw$immi_spending, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    immi_gov_spending_char = case_when(
      df_raw$immi_spending == 1 ~ "less",
      df_raw$immi_spending == 4 ~ "same",
      df_raw$immi_spending == 5 ~ "more"
    )
  )
table(df_clean$immi_gov_spending_char, useNA = "always")

# numeric
df_clean$immi_gov_spending_less_num <- case_when(
  df_raw$immi_spending == 1 ~ 2 / 2,
  df_raw$immi_spending == 4 ~ 1 / 2,
  df_raw$immi_spending == 5 ~ 0 / 2
)
table(df_clean$immi_gov_spending_less_num, useNA = "always")

# How many immigrants should the country admit?
attributes(df_raw$immi_more)
table(df_raw$immi_more, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    immi_admit_less_num = case_when(
      df_raw$immi_more == 1 ~ 4 / 4,
      df_raw$immi_more == 4 ~ 3 / 4,
      df_raw$immi_more == 5 ~ 2 / 4,
      df_raw$immi_more == 6 ~ 1 / 4,
      df_raw$immi_more == 7 ~ 0 / 4
    )
  )
table(df_clean$immi_admit_less_num, useNA = "always")

# immi admit less immigrants
df_clean$immi_admit_less_bin <- ifelse(df_raw$immi_more %in% c(1, 4), 1, 0)
table(df_clean$immi_admit_less_bin, useNA = "always")

# Canada's border controls should be strengthened to limit the number of people applying for refugee status
attributes(df_raw$immi_border)
table(df_raw$immi_border, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    immi_stricter_border_num = case_when(
      df_raw$immi_border == 1 ~ 0 / 3,
      df_raw$immi_border == 4 ~ 1 / 3,
      df_raw$immi_border == 5 ~ 2 / 3,
      df_raw$immi_border == 6 ~ 3 / 3,
    )
  )
table(df_clean$immi_stricter_border_num, useNA = "always")


