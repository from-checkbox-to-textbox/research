# Regression coefficients by condition.
# Coefficient of each predictor in a regression with that predictor alone,
# estimated separately in each condition, with each scale standardized within
# its condition, so the gap between the two coefficients is the difference
# tested in the pooled model scale ~ predictor * open. An asterisk on the
# predictor label marks the differences significant at p < .05, all of which
# are on the anti-immigration scale.
# Output: results/figures/fig_regression_difference.png

library(dplyr)
library(ggplot2)

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
vote_labels <- c("Liberal", "Conservative", "NDP", "Bloc Québécois")
scale_labels <- c(env_scale = "Pro-Environment Scale", immi_scale = "Anti-Immigration Scale")

# Coefficients by condition (standardized scales): one regression per scale,
# predictor and condition
coefs <- expand.grid(scale = names(scale_labels), predictor = names(predictors),
                     condition = c("closed", "open"), stringsAsFactors = FALSE)
coefs[c("estimate", "low", "high")] <- NA_real_
for (i in seq_len(nrow(coefs))) {
  m <- lm(as.formula(paste0(coefs$scale[i], "_std ~ ", predictors[[coefs$predictor[i]]])),
          data = filter(df, treatment == coefs$condition[i]))
  coefs$estimate[i] <- coef(m)[2]
  coefs$low[i] <- confint(m)[2, 1]
  coefs$high[i] <- confint(m)[2, 2]
}
coefs <- coefs |>
  mutate(condition = factor(condition, c("closed", "open"), c("Closed-ended", "Open-ended")))

# Difference between conditions (standardized scales): pooled model
# scale ~ predictor * open, the interaction is the difference
diffs <- expand.grid(scale = names(scale_labels), predictor = names(predictors),
                     stringsAsFactors = FALSE)
diffs$p <- NA_real_
for (i in seq_len(nrow(diffs))) {
  x <- predictors[[diffs$predictor[i]]]
  m <- lm(as.formula(paste0(diffs$scale[i], "_std ~ ", x, " * open")), data = df)
  diffs$p[i] <- coef(summary(m))[paste0(x, ":open"), 4]
}

# Predictors whose difference is significant get an asterisk on their label.
# The labels are shared by both scales, so the note names the scale, and the
# check below stops if a significant difference appears on the other scale
flags <- filter(diffs, p < .05)
stopifnot(all(flags$scale == "immi_scale"))
y_labels <- setNames(names(predictors), names(predictors))
y_labels[flags$predictor] <- paste(flags$predictor, "*")

# Labels and order of the panels and predictors
coefs <- coefs |> mutate(
  scale = factor(scale_labels[scale], scale_labels),
  group = factor(ifelse(predictor %in% vote_labels, "Vote choice", "Socio-demographics"),
                 c("Socio-demographics", "Vote choice")),
  predictor = factor(predictor, rev(names(predictors)))
)

p <- ggplot(coefs, aes(y = predictor)) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.3, linetype = "dashed") +
  geom_errorbar(aes(xmin = low, xmax = high, group = condition),
                width = 0.25, linewidth = 0.4, color = "gray40", orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_point(aes(x = estimate, shape = condition), size = 2.2, stroke = 0.8,
             color = "black", fill = "white", position = position_dodge(width = 0.6)) +
  facet_grid(group ~ scale, scales = "free_y", space = "free_y") +
  scale_y_discrete(labels = y_labels) +
  scale_shape_manual(values = c("Closed-ended" = 19, "Open-ended" = 21), name = NULL) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(
    x = "Standardized Coefficient", y = NULL,
    caption = paste0(
      "Note: OLS coefficient of each predictor entered alone, by condition, with each scale standardized within its condition,\n",
      "and 95% confidence intervals. * Difference between conditions significant at p < .05 on the anti-immigration scale,\n",
      "from a pooled model interacting the predictor with the condition."
    )
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 10, margin = margin(t = 8)),
    plot.caption = element_text(size = 9, hjust = 0, color = "black", margin = margin(t = 12),
                                lineheight = 1.2),
    strip.text = element_text(size = 9.5, face = "bold", color = "black"),
    strip.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.margin = margin(15, 15, 15, 15),
    panel.spacing = unit(1, "lines")
  )

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("results/figures/fig_regression_difference.png", plot = p,
       width = 11, height = 7.5, dpi = 600, bg = "white")
cat("Plot saved as: results/figures/fig_regression_difference.png\n")
