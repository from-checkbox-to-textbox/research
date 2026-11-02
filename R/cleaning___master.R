library(dplyr)
library(tidyr)

source("R/cleaning__master_closed.R")
source("R/cleaning__master_open.R")

df_open <- readRDS("data/df_clean_open.rds") %>%
  mutate(treatment  = "open")

df_closed <- readRDS("data/df_clean_closed.rds") %>%
  mutate(treatment = "closed")

# Merge open and closed datasets with rowbind
df_clean <- bind_rows(df_open, df_closed)

# Save the merged data in RDS format
saveRDS(df_clean, "data/df_clean_merged.rds")


