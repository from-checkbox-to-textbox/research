# Questions and response categories of the questionnaire
# Script: sup_questionnaire.R
# Purpose: writes, for the appendix, the wording of each question and the
#          response categories of its closed-ended version, in the order
#          respondents saw them, as stored in the survey file (English
#          version of the questionnaire).
# Output:  results/tables/questionnaire.tex

library(dplyr)
library(haven)

survey <- readRDS("data/raw/survey.rds")

sections <- list(
  "Socio-Demographic Questions" = c("ses_age", "ses_gender", "ses_province", "ses_dwelling",
                                    "ses_language", "ses_education", "ses_income"),
  "Environment Questions" = c("env_spending", "env_regulation", "env_priority", "env_cause",
                              "env_oil", "env_exaggerated", "env_carbon_tax"),
  "Immigration Questions" = c("immi_integration", "immi_take_jobs", "immi_spending",
                              "immi_more", "immi_border"),
  "Vote Choice Question" = c("vote_federal")
)

lines <- c()
for (section in names(sections)) {
  lines <- c(lines, paste0("\\subsection{", section, "}"), "\\begin{itemize}")
  for (v in sections[[section]]) {
    # Escape the characters that LaTeX would read as commands ($, &, %, #, _)
    question <- gsub("([$&%#_])", "\\\\\\1", attr(survey[[v]], "label"))
    categories <- gsub("([$&%#_])", "\\\\\\1", names(attr(survey[[v]], "labels")))
    if (length(categories) == 0) {
      lines <- c(lines, paste0("  \\item ", question))
    } else {
      lines <- c(lines,
                 paste0("  \\item ", question, "\\\\"),
                 paste0("  \\textit{", paste(categories, collapse = " $\\cdot$ "), "}"))
    }
  }
  lines <- c(lines, "\\end{itemize}", "")
}

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
writeLines(lines, "results/tables/questionnaire.tex")
