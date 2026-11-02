# Scree plot with parallel analysis, one panel per scale, closed-ended and
# open-ended conditions on the same panel.
# Eigenvalues of the item correlation matrix (the first one is the eigenvalue
# reported in fig_factor_analysis_dumbell), on the respondents who answered
# every item of the scale, as in the factor analysis. Parallel analysis:
# mean eigenvalues of 100 random data sets of the same size (psych::fa.parallel).
# Output: results/figures/fig_scree_by_scale.png

library(dplyr)
library(ggplot2)
library(psych)

set.seed(2025)
options(mc.cores = 1)  # parallel analysis on one core, so that the seed fixes its random draws

df <- readRDS("data/df_clean_merged.rds")

scales <- list(
  "Pro-Environment Scale" = c(
    "env_gov_spending_more_num", "env_regulation_stricter_num",
    "env_priority_over_jobs_num", "env_climate_cause_human_bin",
    "env_against_oil_support_num", "env_climate_change_not_exaggerated_num",
    "env_carbon_tax_maintained_num"
  ),
  "Anti-Immigration Scale" = c(
    "immi_dont_fit_in_num", "immi_take_jobs_num", "immi_gov_spending_less_num",
    "immi_admit_less_num", "immi_stricter_border_num"
  )
)

scree <- lapply(names(scales), function(s) {
  lapply(c("closed", "open"), function(cond) {
    d <- df |> filter(treatment == cond) |> select(all_of(scales[[s]])) |> tidyr::drop_na()
    pa <- fa.parallel(d, fa = "pc", n.iter = 100, plot = FALSE)
    k <- length(pa$pc.values)
    bind_rows(
      tibble(component = 1:k, eigenvalue = pa$pc.values, line = "Observed"),
      tibble(component = 1:k, eigenvalue = pa$pc.sim, line = "Random data")
    ) |>
      mutate(scale = s, condition = cond, n = nrow(d), retained = pa$ncomp)
  }) |> bind_rows()
}) |> bind_rows() |>
  mutate(
    scale = factor(scale, names(scales)),
    condition = factor(condition, c("closed", "open"), c("Closed-ended", "Open-ended"))
  )

scree |> distinct(scale, condition, n, retained) |> print()

p <- ggplot(scree, aes(x = component, y = eigenvalue, color = condition,
                       linetype = line, group = interaction(condition, line))) +
  geom_line(linewidth = 0.7) +
  geom_point(data = filter(scree, line == "Observed"), size = 2) +
  facet_wrap(~scale, scales = "free_x") +
  scale_color_manual(values = c("Closed-ended" = "black", "Open-ended" = "gray60"), name = NULL) +
  scale_linetype_manual(values = c("Observed" = "solid", "Random data" = "dashed"), name = NULL) +
  scale_x_continuous(breaks = 1:7) +
  # Fixed order of the two legends (otherwise ggplot may swap them between runs)
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2)) +
  labs(
    x = "Component", y = "Eigenvalue",
    caption = paste0(
      "Note: Eigenvalues of the item correlation matrix, on the respondents who answered every item of the scale.\n",
      "Dashed lines: mean eigenvalues of 100 random data sets of the same size (parallel analysis)."
    )
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10, color = "black"),
    plot.caption = element_text(size = 9, hjust = 0, color = "black", margin = margin(t = 12),
                                lineheight = 1.2),
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    strip.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.margin = margin(15, 15, 15, 15)
  )

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("results/figures/fig_scree_by_scale.png", plot = p, width = 10, height = 4.8,
       dpi = 600, bg = "white")
cat("Plot saved as: results/figures/fig_scree_by_scale.png\n")
