# Semi-detached houses and the balance gap on dwelling type
# Script: sup_dwelling_semidetached.R
# Purpose: the closed-ended dwelling question has no semi-detached category.
#          Counts the open-ended answers that describe a semi-detached house
#          ("semi", "jumel" or "jumul" in the text, ignoring case), shows how
#          the consensus coded them, and recomputes the standardized gap
#          between conditions on the dwelling indicator used in the balance
#          table (condo, townhouse or detached house) with these answers
#          dropped and with these answers coded outside that category.
# Output:  results/tables/dwelling_semidetached.csv

library(dplyr)

consensus <- readRDS("data/df_clean_consensus_3providers_10_1.rds") %>%
  filter(assignment == "open")
merged <- readRDS("data/df_clean_merged.rds")

closed <- merged$ses_owner_bin[merged$treatment == "closed"]
open <- merged$ses_owner_bin[merged$treatment == "open"]
stopifnot(length(open) == nrow(consensus))

text <- tolower(consensus$ses_dwelling_open)
semi <- !is.na(text) & grepl("semi|jumel|jumul", text)
print(table(consensus$ses_dwelling_ai[semi], useNA = "ifany"))

# Standardized gap: difference in means over the pooled standard deviation,
# with all answers, without the semi-detached answers, and with them coded
# outside the category
open_dropped <- open[!semi]
open_outside <- open
open_outside[semi] <- 0

smd_all <- (mean(open, na.rm = TRUE) - mean(closed, na.rm = TRUE)) /
  sqrt((sd(closed, na.rm = TRUE)^2 + sd(open, na.rm = TRUE)^2) / 2)
smd_dropped <- (mean(open_dropped, na.rm = TRUE) - mean(closed, na.rm = TRUE)) /
  sqrt((sd(closed, na.rm = TRUE)^2 + sd(open_dropped, na.rm = TRUE)^2) / 2)
smd_outside <- (mean(open_outside, na.rm = TRUE) - mean(closed, na.rm = TRUE)) /
  sqrt((sd(closed, na.rm = TRUE)^2 + sd(open_outside, na.rm = TRUE)^2) / 2)

out <- tibble(
  statistic = c("semi-detached answers", "coded townhouse",
                "smd, all answers", "smd, semi-detached dropped",
                "smd, semi-detached coded outside the category"),
  value = c(sum(semi), sum(consensus$ses_dwelling_ai[semi] == 19, na.rm = TRUE),
            smd_all, smd_dropped, smd_outside)
)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "results/tables/dwelling_semidetached.csv", row.names = FALSE)
print(as.data.frame(out))
