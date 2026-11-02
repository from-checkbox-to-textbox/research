# age
attributes(df_raw$ses_age)
table(df_raw$ses_age, useNA = "always")
df_clean$ses_age_num <- as.numeric(df_raw$ses_age)
table(df_clean$ses_age_num, useNA = "always")

## 34 or less
df_clean$ses_age_under_34_bin <- ifelse(df_raw$ses_age <= 34, 1, 0)
table(df_clean$ses_age_under_34_bin, useNA = "always")

# 35 to 54
df_clean$ses_age_35_54_bin <- ifelse(df_raw$ses_age >= 35 & df_clean$ses_age_num <= 54, 1, 0)
table(df_clean$ses_age_35_54_bin, useNA = "always")

# 55 or older
df_clean$ses_age_55_plus_bin <- ifelse(df_raw$ses_age >= 55, 1, 0)
table(df_clean$ses_age_55_plus_bin, useNA = "always")

# gender
attributes(df_raw$ses_gender)
table(df_raw$ses_gender, useNA = "always")
df_clean$ses_gender_char <- NA
df_clean$ses_gender_char[df_raw$ses_gender == 1] <- "woman"
df_clean$ses_gender_char[df_raw$ses_gender == 2] <- "man"
df_clean$ses_gender_char[df_raw$ses_gender == 3] <- "non_binary"
df_clean$ses_gender_char[df_raw$ses_gender == 4] <- "queer"
df_clean$ses_gender_char[df_raw$ses_gender == 5] <- "agender"
table(df_clean$ses_gender_char, useNA = "always")

# female
df_clean$ses_female_bin <- ifelse(df_raw$ses_gender == 1, 1, 0)
table(df_clean$ses_female_bin, useNA = "always")

# province
attributes(df_raw$ses_province)
table(df_raw$ses_province, useNA = "always")
df_clean$ses_province_char <- NA
df_clean$ses_province_char[df_raw$ses_province == 1] <- "ab"
df_clean$ses_province_char[df_raw$ses_province == 2] <- "bc"
df_clean$ses_province_char[df_raw$ses_province == 3] <- "mb"
df_clean$ses_province_char[df_raw$ses_province == 4] <- "nb"
df_clean$ses_province_char[df_raw$ses_province == 5] <- "nl"
df_clean$ses_province_char[df_raw$ses_province == 6] <- "ns"
df_clean$ses_province_char[df_raw$ses_province == 7] <- "nt"
df_clean$ses_province_char[df_raw$ses_province == 8] <- "nu"
df_clean$ses_province_char[df_raw$ses_province == 9] <- "on"
df_clean$ses_province_char[df_raw$ses_province == 10] <- "pe"
df_clean$ses_province_char[df_raw$ses_province == 11] <- "qc"
df_clean$ses_province_char[df_raw$ses_province == 12] <- "sk"
df_clean$ses_province_char[df_raw$ses_province == 13] <- "yt"
table(df_clean$ses_province_char, useNA = "always")

# ROC
df_clean$ses_roc_bin <- ifelse(df_raw$ses_province != 11, 1, 0)
table(df_clean$ses_roc_bin, useNA = "always")

# Quebec
df_clean$ses_qc_bin <- ifelse(df_raw$ses_province == 11, 1, 0)
table(df_clean$ses_qc_bin, useNA = "always")

# dwelling
attributes(df_raw$ses_dwelling)
table(df_raw$ses_dwelling, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    ses_dwelling_char = case_when(
      df_raw$ses_dwelling == 1 ~ "high_rise",
      df_raw$ses_dwelling == 13 ~ "appartment_fewer_than_5",
      df_raw$ses_dwelling == 14 ~ "condo",
      df_raw$ses_dwelling == 15 ~ "co-op",
      df_raw$ses_dwelling == 16 ~ "public_housing",
      df_raw$ses_dwelling == 17 ~ "duplex",
      df_raw$ses_dwelling == 18 ~ "loft",
      df_raw$ses_dwelling == 19 ~ "townhouse",
      df_raw$ses_dwelling == 20 ~ "house",
      df_raw$ses_dwelling == 21 ~ "mobile_home",
      df_raw$ses_dwelling == 22 ~ "other"
    )
  )
table(df_clean$ses_dwelling_char, useNA = "always")

# owner 
df_clean$ses_owner_bin <- ifelse(is.na(df_raw$ses_dwelling), NA, ifelse(df_raw$ses_dwelling %in% c(14, 19, 20), 1, 0))
table(df_clean$ses_owner_bin, useNA = "always")

# language
attributes(df_raw$ses_language)
table(df_raw$ses_language, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    ses_language_char = case_when(
      df_raw$ses_language == 1 ~ "english",
      df_raw$ses_language == 2 ~ "french",
      df_raw$ses_language == 3 ~ "other"
    )
  )
table(df_clean$ses_language_char, useNA = "always")

# english
df_clean$ses_english_bin <- ifelse(df_raw$ses_language == 1, 1, 0)
table(df_clean$ses_english_bin, useNA = "always")

# education
attributes(df_raw$ses_education)
table(df_raw$ses_education, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    ses_education_char = case_when(
      df_raw$ses_education == 1 ~ "no_schooling",
      df_raw$ses_education == 4 ~ "elementary_school",
      df_raw$ses_education == 5 ~ "high_school",
      df_raw$ses_education == 6 ~ "college",
      df_raw$ses_education == 7 ~ "undergrad",
      df_raw$ses_education == 8 ~ "master",
      df_raw$ses_education == 9 ~ "phd"
    )
  )
table(df_clean$ses_education_char, useNA = "always")
# education before high school
df_clean$ses_education_bhs_bin <- ifelse(df_raw$ses_education < 5, 1, 0)
table(df_clean$ses_education_bhs_bin, useNA = "always")
# education high school 
df_clean$ses_education_hs_bin <- ifelse(is.na(df_raw$ses_education), NA, ifelse(df_raw$ses_education %in% 5:6, 1, 0))
table(df_clean$ses_education_hs_bin, useNA = "always")
# education university
df_clean$ses_education_univ_bin <- ifelse(is.na(df_raw$ses_education), NA, ifelse(df_raw$ses_education %in% 7:9, 1, 0))
table(df_clean$ses_education_univ_bin, useNA = "always")

# income
attributes(df_raw$ses_income)
table(df_raw$ses_income, useNA = "always")
df_clean <- df_clean %>%
  mutate(
    ses_income_char = case_when(
      df_raw$ses_income == 1 ~ "none",
      df_raw$ses_income == 4 ~ "1_30000",
      df_raw$ses_income == 5 ~ "30001_60000",
      df_raw$ses_income == 6 ~ "60001_90000",
      df_raw$ses_income == 7 ~ "90001_110000",
      df_raw$ses_income == 8 ~ "110001_150000",
      df_raw$ses_income == 9 ~ "150001_200000",
      df_raw$ses_income == 10 ~ "200001_more",
    )
  )
table(df_clean$ses_income_char, useNA = "always")

# income low
df_clean$ses_income_low_bin <- ifelse(is.na(df_raw$ses_income), NA, ifelse(df_raw$ses_income %in% c(1, 4, 5), 1, 0))
table(df_clean$ses_income_low_bin, useNA = "always")

# income middle
df_clean$ses_income_mid_bin <- ifelse(is.na(df_raw$ses_income), NA, ifelse(df_raw$ses_income %in% c(6, 7), 1, 0))
table(df_clean$ses_income_mid_bin, useNA = "always")

# income high
df_clean$ses_income_high_bin <- ifelse(is.na(df_raw$ses_income), NA, ifelse(df_raw$ses_income %in% c(8, 9, 10), 1, 0))
table(df_clean$ses_income_high_bin, useNA = "always")
