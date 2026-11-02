# Raw survey data
# Script: 00_get_survey.R
# Purpose: the raw survey file is not included in this repository (see
#          DATA_AVAILABILITY.md). It holds the responses recorded before
#          June 23, 2025, the export coded by the models and analysed in the
#          article (3,514 responses). For anonymous review, the download from
#          the research group's data lake is withheld, since it identifies the
#          group. Every other script reads the file checked here.
# Output:  data/raw/survey.rds

if (!file.exists("data/raw/survey.rds")) {
  stop("data/raw/survey.rds is not included in this repository (see DATA_AVAILABILITY.md)")
}
