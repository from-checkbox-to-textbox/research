library(dplyr)
library(haven)
library(tidyr)
library(ggplot2)

# Raw survey data (R/00_get_survey.R)
df_raw <- readRDS("data/raw/survey.rds") %>%
  filter(assignment == "closed") %>%
  filter(Finished == 1) %>%
  select(-contains("ope"))

df_clean <- data.frame(id = 1:nrow(df_raw))

# ses
source("R/cleaning_ses.R")

# env
source("R/cleaning_env.R")

# immi
source("R/cleaning_immi.R")

# vote
source("R/cleaning_vote.R")

# scale making
source("R/cleaning_scales.R")

# save data in RDS format
saveRDS(df_clean, "data/df_clean_closed.rds")
