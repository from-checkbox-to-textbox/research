# Exploratory coding of strategic voting in the open-ended vote answers
# Script: sup_vote_strategic.R
# Purpose: sends each open-ended vote answer of the respondents who completed
#          the open-ended survey once to Gemini 3.5 Flash-Lite (through
#          OpenRouter) with a single prompt, which returns the party and
#          whether the respondent describes the vote as strategic. Reported
#          in the Discussion as an exploratory example; the prompt is in the
#          appendix.
# Usage:   OPENROUTER_API_KEY=... Rscript R/sup_vote_strategic.R
# Output:  data/vote_strategic.csv (kept in the repository, since make does
#          not rerun this API step)

library(haven)
library(dplyr)
library(httr)
library(jsonlite)
library(stringr)

api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (!nzchar(api_key)) {
  stop("Please set the OPENROUTER_API_KEY environment variable")
}

data <- readRDS("data/raw/survey.rds")
answers <- data %>%
  filter(assignment == "open", Finished == 1) %>%
  mutate(response_text = trimws(as.character(vote_federal_open))) %>%
  filter(!is.na(vote_federal_open), response_text != "") %>%
  select(response_text)

valid_parties <- c("lpc", "cpc", "bq", "ndp", "gpc", "ppc", "other")
answers$party <- NA_character_
answers$strategic <- NA_real_
answers$status <- NA_character_

for (i in seq_len(nrow(answers))) {
  prompt <- paste0(
    "Analyze this Canadian federal voting response and extract two pieces of information:\n",
    "1. Which federal party did the respondent vote for? Use ONLY these party codes: lpc, cpc, bq, ndp, gpc, ppc, or other\n",
    "2. Does the respondent seem to have voted strategically? (1 for strategic, 0 for not strategic)\n\n",
    "Response to analyze: \"", answers$response_text[i], "\"\n\n",
    "Return your answer in this exact JSON format:\n",
    "{\n",
    "  \"party\": \"[party code]\",\n",
    "  \"strategic\": [0 or 1]\n",
    "}\n\n",
    "Important guidelines:\n",
    "- Use ONLY these exact party codes (lowercase):\n",
    "  - lpc = Liberal Party of Canada / Parti libéral du Canada / Libéral / Liberal / PLC\n",
    "  - cpc = Conservative Party of Canada / Parti conservateur du Canada / Conservateur / Conservative / PCC\n",
    "  - bq = Bloc Québécois / Bloc québécois / Bloc / BQ\n",
    "  - ndp = New Democratic Party / Nouveau Parti démocratique / NPD / NDP\n",
    "  - gpc = Green Party of Canada / Parti vert du Canada / Vert / Green / PVC\n",
    "  - ppc = People's Party of Canada / Parti populaire du Canada / PPC\n",
    "  - other = any other party or unclear\n",
    "- Responses may be in French or English\n",
    "- Look for words like strategic, to block, to prevent, par stratégie, pour bloquer, pour empêcher for strategic voting\n",
    "- If unclear about strategic voting, default to 0 (not strategic)\n",
    "- If party is unclear, use 'other'"
  )

  # One call per answer, up to three attempts. An answer whose reply cannot be
  # read counts as not strategic, with party "other".
  answers$party[i] <- "other"
  answers$strategic[i] <- 0
  answers$status[i] <- "failed"
  for (attempt in 1:3) {
    response <- tryCatch(
      POST("https://openrouter.ai/api/v1/chat/completions",
           body = list(model = "google/gemini-3.5-flash-lite",
                       messages = list(list(role = "user", content = prompt))),
           encode = "json",
           add_headers("Authorization" = paste("Bearer", api_key)),
           timeout(30)),
      error = function(e) NULL
    )
    if (is.null(response) || status_code(response) != 200) next
    reply <- content(response, as = "parsed", simplifyVector = FALSE)$choices[[1]]$message$content
    json_str <- str_extract(reply, "\\{[^}]+\\}")
    if (is.na(json_str)) next
    result <- fromJSON(json_str)
    if (result$party %in% valid_parties && result$strategic %in% c(0, 1)) {
      answers$party[i] <- result$party
      answers$strategic[i] <- result$strategic
      answers$status[i] <- "coded"
      break
    }
  }
  if (i %% 100 == 0) cat(i, "of", nrow(answers), "answers\n")
}

# Share of strategic votes by party
summary_by_party <- answers %>%
  group_by(party) %>%
  summarise(answers = n(), share = mean(strategic), strategic = sum(strategic)) %>%
  arrange(desc(share))
print(table(answers$status))
print(summary_by_party)

write.csv(answers, "data/vote_strategic.csv", row.names = FALSE)
