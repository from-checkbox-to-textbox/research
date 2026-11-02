# Do the regression coefficients differ between conditions?
# For each scale and predictor, a pooled model scale ~ predictor * open:
# the interaction is the open-ended slope minus the closed-ended slope.
# The tests are run twice: on the scales as they are (0-1), and on each scale
# standardized within its condition, since the open-ended scales have less
# spread and a narrower scale yields smaller slopes.
# Output: results/tables/regression_interactions.csv

library(dplyr)

df <- readRDS("data/df_clean_merged.rds") |>
  mutate(open = as.integer(treatment == "open")) |>
  group_by(treatment) |>
  mutate(
    env_scale_std = as.numeric(scale(env_scale)),
    immi_scale_std = as.numeric(scale(immi_scale))
  ) |>
  ungroup()

predictors <- c(
  "Age 55+"         = "ses_age_55_plus_bin",
  "Female"          = "ses_female_bin",
  "Quebec"          = "ses_qc_bin",
  "Dwelling type"   = "ses_owner_bin",
  "English speaker" = "ses_english_bin",
  "University"      = "ses_education_univ_bin",
  "High income"     = "ses_income_high_bin",
  "Liberal"         = "vote_federal_lpc_bin",
  "Conservative"    = "vote_federal_cpc_bin",
  "NDP"             = "vote_federal_ndp_bin",
  "Bloc Québécois"  = "vote_federal_bq_bin"
)

# One pooled model per scale, predictor and unit: scale ~ predictor * open.
# The interaction is the open-ended slope minus the closed-ended slope.
results <- expand.grid(
  predictor = names(predictors),
  scale = c("env_scale", "immi_scale"),
  units = c("raw", "standardized"),
  stringsAsFactors = FALSE
)[, c("scale", "predictor", "units")]
results[c("closed", "open", "difference", "low", "high", "p", "n")] <- NA_real_
for (i in seq_len(nrow(results))) {
  dv <- if (results$units[i] == "raw") results$scale[i] else paste0(results$scale[i], "_std")
  x <- predictors[[results$predictor[i]]]
  m <- lm(as.formula(paste(dv, "~", x, "* open")), data = df)
  cs <- coef(summary(m))
  ci <- confint(m)[paste0(x, ":open"), ]
  results$closed[i] <- cs[x, 1]
  results$open[i] <- cs[x, 1] + cs[paste0(x, ":open"), 1]
  results$difference[i] <- cs[paste0(x, ":open"), 1]
  results$low[i] <- ci[1]
  results$high[i] <- ci[2]
  results$p[i] <- cs[paste0(x, ":open"), 4]
  results$n[i] <- nobs(m)
}
results <- as_tibble(results)

dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(results, "results/tables/regression_interactions.csv", row.names = FALSE)

print(results |> mutate(across(c(closed, open, difference), \(v) round(v, 3)), p = round(p, 3)), n = Inf)

results |>
  group_by(units) |>
  summarise(tests = n(), p_below_05 = sum(p < .05), expected_by_chance = n() * .05) |>
  print()

# LaTeX table for the appendix: one row per scale and predictor
wide <- results |>
  mutate(p_text = ifelse(p < .001, "<.001", sub("^0", "", sprintf("%.3f", p))),
         cell = sprintf("%.3f & %.3f & %.3f & %s", closed, open, difference, p_text),
         cell = ifelse(units == "standardized",
                       sprintf("%.2f & [%.2f, %.2f] & %s", difference, low, high, p_text), cell)) |>
  select(scale, predictor, units, cell) |>
  tidyr::pivot_wider(names_from = units, values_from = cell)
# Shade the rows where the standardized difference is significant
shade <- results |>
  filter(units == "standardized") |>
  mutate(shade = ifelse(p < .05, "\\rowcolor[gray]{0.86}", "")) |>
  select(scale, predictor, shade)
wide <- left_join(wide, shade, by = c("scale", "predictor"))
scale_names <- c(env_scale = "Pro-environment scale", immi_scale = "Anti-immigration scale")
rows <- c()
for (scale_name in names(scale_names)) {
  rows <- c(rows, sprintf("\\multicolumn{8}{l}{\\textit{%s}} \\\\", scale_names[[scale_name]]))
  w <- wide[wide$scale == scale_name, ]
  rows <- c(rows, sprintf("%s%s & %s & %s \\\\", w$shade, w$predictor, w$raw, w$standardized))
}
tex <- c(
  "\\begin{tabular}{lrrrrrcr}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Scales from 0 to 1} & \\multicolumn{3}{c}{Standardized scales} \\\\",
  "\\cmidrule(lr){2-5} \\cmidrule(lr){6-8}",
  "Predictor & Closed & Open & Difference & \\textit{p} & Difference & 95\\% CI & \\textit{p} \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tex, "results/tables/regression_interactions.tex")
