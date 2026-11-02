# Factor loadings, reliability and first eigenvalue by condition, pairwise correlations
# Script: fig_facanal_dumbell_12vars_pairwise.R
# Purpose: one-factor model (maximum likelihood) of each scale in each
#          condition, on pairwise correlations: every respondent who
#          answered both items of a pair enters that correlation (robustness
#          check of the Analysis footnote). Loadings of each item, Cronbach's alpha and
#          first eigenvalue (appendix figure).
# Output:  results/figures/fig_factor_analysis_dumbell_pairwise.png

library(tidyr)
library(dplyr)
library(ggplot2)
library(psych)
library(GPArotation)
library(scales)


df_merged <- readRDS("data/df_clean_merged.rds")

df_open <- df_merged %>%
  filter(treatment == "open") %>%
  select(-treatment)

df_closed <- df_merged %>%
  filter(treatment == "closed") %>%
  select(-treatment)

df_env_open <- df_open %>%
  select(
    env_gov_spending_more_num,
    env_regulation_stricter_num,
    env_priority_over_jobs_num,
    env_climate_cause_human_bin,
    env_against_oil_support_num,
    env_climate_change_not_exaggerated_num,
    env_carbon_tax_maintained_num
  ) %>% 
  filter(rowSums(!is.na(.)) > 0)  # pairwise: psych uses pairwise correlations with NA

df_env_closed <- df_closed %>%
  select(
    env_gov_spending_more_num,
    env_regulation_stricter_num,
    env_priority_over_jobs_num,
    env_climate_cause_human_bin,
    env_against_oil_support_num,
    env_climate_change_not_exaggerated_num,
    env_carbon_tax_maintained_num
  ) %>% 
  filter(rowSums(!is.na(.)) > 0)  # pairwise: psych uses pairwise correlations with NA

df_immi_open <- df_open %>%
  select(
    immi_dont_fit_in_num,
    immi_take_jobs_num,
    immi_gov_spending_less_num,
    immi_admit_less_num,
    immi_stricter_border_num
  ) %>% 
  filter(rowSums(!is.na(.)) > 0)  # pairwise: psych uses pairwise correlations with NA

df_immi_closed <- df_closed %>%
  select(
    immi_dont_fit_in_num,
    immi_take_jobs_num,
    immi_gov_spending_less_num,
    immi_admit_less_num,
    immi_stricter_border_num
  ) %>% 
  filter(rowSums(!is.na(.)) > 0)  # pairwise: psych uses pairwise correlations with NA

# One-factor model (maximum likelihood) and Cronbach's alpha for each scale
# and condition
fa_env_open <- fa(df_env_open, nfactors = 1, rotate = "oblimin", fm = "ml")
fa_env_closed <- fa(df_env_closed, nfactors = 1, rotate = "oblimin", fm = "ml")
fa_immi_open <- fa(df_immi_open, nfactors = 1, rotate = "oblimin", fm = "ml")
fa_immi_closed <- fa(df_immi_closed, nfactors = 1, rotate = "oblimin", fm = "ml")

alpha_env_open <- psych::alpha(df_env_open)$total$raw_alpha
alpha_env_closed <- psych::alpha(df_env_closed)$total$raw_alpha
alpha_immi_open <- psych::alpha(df_immi_open)$total$raw_alpha
alpha_immi_closed <- psych::alpha(df_immi_closed)$total$raw_alpha

# Labels of the items in the figure
item_labels <- c(
  env_gov_spending_more_num = "Gov Spending More",
  env_regulation_stricter_num = "Stricter Regulation",
  env_priority_over_jobs_num = "Priority Over Jobs",
  env_climate_cause_human_bin = "Climate Cause Human",
  env_against_oil_support_num = "Against Oil Support",
  env_climate_change_not_exaggerated_num = "Climate Not Exaggerated",
  env_carbon_tax_maintained_num = "Carbon Tax Maintained",
  immi_dont_fit_in_num = "Don't Fit In",
  immi_take_jobs_num = "Take Jobs",
  immi_gov_spending_less_num = "Gov Spending Less",
  immi_admit_less_num = "Admit Less",
  immi_stricter_border_num = "Stricter Border"
)

# Loadings of each item in both conditions, with the scale statistics
env_data <- tibble(
  variable = rownames(fa_env_open$loadings),
  open = as.numeric(fa_env_open$loadings[, 1]),
  closed = as.numeric(fa_env_closed$loadings[rownames(fa_env_open$loadings), 1]),
  domain = "Pro-Environment Scale",
  alpha_open = alpha_env_open,
  alpha_closed = alpha_env_closed,
  eigen_open = fa_env_open$e.values[1],
  eigen_closed = fa_env_closed$e.values[1]
)
immi_data <- tibble(
  variable = rownames(fa_immi_open$loadings),
  open = as.numeric(fa_immi_open$loadings[, 1]),
  closed = as.numeric(fa_immi_closed$loadings[rownames(fa_immi_open$loadings), 1]),
  domain = "Anti-Immigration Scale",
  alpha_open = alpha_immi_open,
  alpha_closed = alpha_immi_closed,
  eigen_open = fa_immi_open$e.values[1],
  eigen_closed = fa_immi_closed$e.values[1]
)
env_data$variable_clean <- unname(item_labels[env_data$variable])
immi_data$variable_clean <- unname(item_labels[immi_data$variable])

# Combine data (env_data first for top panel)
plot_data <- bind_rows(env_data, immi_data) %>%
  mutate(
    domain = factor(domain, levels = c("Pro-Environment Scale", "Anti-Immigration Scale")),
    variable_clean = factor(variable_clean, 
                           levels = c("Gov Spending More", "Stricter Regulation", "Priority Over Jobs", 
                                    "Climate Cause Human", "Against Oil Support", "Climate Not Exaggerated", 
                                    "Carbon Tax Maintained", "Don't Fit In", "Take Jobs", 
                                    "Gov Spending Less", "Admit Less", "Stricter Border"))
  )

# Create custom facet labels with statistics
env_stats <- paste0("Pro-Environment Scale\nα = ", formatC(unique(env_data$alpha_closed), format = "f", digits = 3), 
                   " (closed), ", formatC(unique(env_data$alpha_open), format = "f", digits = 3), " (open) | ",
                   "λ₁ = ", formatC(unique(env_data$eigen_closed), format = "f", digits = 2), 
                   " (closed), ", formatC(unique(env_data$eigen_open), format = "f", digits = 2), " (open)")

immi_stats <- paste0("Anti-Immigration Scale\nα = ", formatC(unique(immi_data$alpha_closed), format = "f", digits = 3),
                    " (closed), ", formatC(unique(immi_data$alpha_open), format = "f", digits = 3), " (open) | ",
                    "λ₁ = ", formatC(unique(immi_data$eigen_closed), format = "f", digits = 2),
                    " (closed), ", formatC(unique(immi_data$eigen_open), format = "f", digits = 2), " (open)")

# Create more compact labels for presentation using dot symbols
env_stats_pres <- paste0("Pro-Environment Scale\nα: ●", formatC(unique(env_data$alpha_closed), format = "f", digits = 3), 
                        ", ○", formatC(unique(env_data$alpha_open), format = "f", digits = 3), " | ",
                        "λ₁: ●", formatC(unique(env_data$eigen_closed), format = "f", digits = 2), 
                        ", ○", formatC(unique(env_data$eigen_open), format = "f", digits = 2))

immi_stats_pres <- paste0("Anti-Immigration Scale\nα: ●", formatC(unique(immi_data$alpha_closed), format = "f", digits = 3),
                         ", ○", formatC(unique(immi_data$alpha_open), format = "f", digits = 3), " | ",
                         "λ₁: ●", formatC(unique(immi_data$eigen_closed), format = "f", digits = 2),
                         ", ○", formatC(unique(immi_data$eigen_open), format = "f", digits = 2))

# Update plot_data with custom labels and label positioning
plot_data <- plot_data %>%
  mutate(
    domain_with_stats = case_when(
      domain == "Pro-Environment Scale" ~ env_stats,
      domain == "Anti-Immigration Scale" ~ immi_stats
    ),
    domain_with_stats_pres = case_when(
      domain == "Pro-Environment Scale" ~ env_stats_pres,
      domain == "Anti-Immigration Scale" ~ immi_stats_pres
    ),
    # Ensure proper ordering for facet panels (environment at top)
    domain_with_stats = factor(domain_with_stats, levels = c(env_stats, immi_stats)),
    domain_with_stats_pres = factor(domain_with_stats_pres, levels = c(env_stats_pres, immi_stats_pres)),
    # Determine which value is rightmost and leftmost
    rightmost_value = pmax(open, closed),
    leftmost_value = pmin(open, closed),
    rightmost_x = ifelse(open > closed, open, closed),
    leftmost_x = ifelse(open < closed, open, closed),
    rightmost_label = ifelse(open > closed, sprintf("%.2f", open), sprintf("%.2f", closed)),
    leftmost_label = ifelse(open < closed, sprintf("%.2f", open), sprintf("%.2f", closed))
  )

# Create academic journal-style dumbell plot
p <- ggplot(plot_data, aes(y = variable_clean)) +
  # Add threshold line at 0.3
  geom_vline(xintercept = 0.3, color = "black", linewidth = 0.6, linetype = "dotted") +
  # Add reference line at 0
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.3, linetype = "dashed") +
  # Connecting lines
  geom_segment(aes(x = closed, xend = open), 
               color = "gray30", linewidth = 0.8, alpha = 0.8) +
  # Filled dots for closed treatment
  geom_point(aes(x = closed), color = "black", fill = "black", 
             size = 2.5, shape = 19, stroke = 0.5) +
  # Empty dots for open treatment  
  geom_point(aes(x = open), color = "black", fill = "white", 
             size = 2.5, shape = 21, stroke = 0.8) +
  # Add labels for rightmost values (to the right)
  geom_text(aes(x = rightmost_x, label = rightmost_label),
            hjust = -0.5, vjust = 0.5, size = 2.8, color = "black") +
  # Add labels for leftmost values (to the left)  
  geom_text(aes(x = leftmost_x, label = leftmost_label),
            hjust = 1.5, vjust = 0.5, size = 2.8, color = "black") +
  facet_wrap(~domain_with_stats, scales = "free_y", ncol = 1) +
  labs(
    x = "Factor Loading",
    y = NULL,
    caption = "Note: ● = closed-ended condition; ○ = open-ended condition.\nThe dotted line marks a loading of 0.3."
  ) +
  theme_bw() +
  theme(
    # Typography
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5, 
                             margin = margin(b = 20)),
    axis.title.x = element_text(size = 11, face = "plain", 
                               margin = margin(t = 12)),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    plot.caption = element_text(size = 9, hjust = 0, color = "black",
                               margin = margin(t = 15), lineheight = 1.2),
    
    # Facet strips - clean, professional style with statistics
    strip.text = element_text(size = 9, face = "bold", color = "black",
                             margin = margin(8, 0, 8, 0), lineheight = 1.1),
    strip.background = element_blank(),
    strip.placement = "outside",
    
    # Panel and plot
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.25),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    # Spacing - adjust to give more room for dumbbells
    plot.margin = margin(20, 30, 20, 20),
    panel.spacing = unit(2, "lines"),
    
    # Remove legend
    legend.position = "none"
  ) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0.15, 0.15)))

print(p)

# Create presentation-friendly version (16:9)
p_pres <- ggplot(plot_data, aes(y = variable_clean)) +
  # Add threshold line at 0.3
  geom_vline(xintercept = 0.3, color = "black", linewidth = 0.8, linetype = "dotted") +
  # Add reference line at 0
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.5, linetype = "dashed") +
  # Connecting lines
  geom_segment(aes(x = closed, xend = open), 
               color = "gray30", linewidth = 1.2, alpha = 0.8) +
  # Filled dots for closed treatment
  geom_point(aes(x = closed), color = "black", fill = "black", 
             size = 4, shape = 19, stroke = 0.8) +
  # Empty dots for open treatment  
  geom_point(aes(x = open), color = "black", fill = "white", 
             size = 4, shape = 21, stroke = 1.2) +
  # Add labels for rightmost values (to the right)
  geom_text(aes(x = rightmost_x, label = rightmost_label),
            hjust = -0.4, vjust = 0.5, size = 4.5, color = "black", fontface = "bold") +
  # Add labels for leftmost values (to the left)  
  geom_text(aes(x = leftmost_x, label = leftmost_label),
            hjust = 1.4, vjust = 0.5, size = 4.5, color = "black", fontface = "bold") +
  facet_wrap(~domain_with_stats_pres, scales = "free_y", ncol = 2) +
  labs(
    title = "Factor Analysis by Condition",
    x = "Factor Loading",
    y = NULL,
    caption = "Note: ● = closed-ended condition; ○ = open-ended condition. The dotted line marks a loading of 0.3."
  ) +
  theme_bw() +
  theme(
    # Typography
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, 
                             margin = margin(b = 25)),
    axis.title.x = element_text(size = 16, face = "bold", 
                               margin = margin(t = 15)),
    axis.text.y = element_text(size = 13, color = "black", face = "bold"),
    axis.text.x = element_text(size = 13, color = "black"),
    plot.caption = element_text(size = 12, hjust = 0, color = "black",
                               margin = margin(t = 20), lineheight = 1.2),
    
    # Facet strips - clean, professional style with statistics
    strip.text = element_text(size = 11, face = "bold", color = "black",
                             margin = margin(8, 0, 8, 0), lineheight = 1.0),
    strip.background = element_blank(),
    strip.placement = "outside",
    
    # Panel and plot
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.4),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    
    # Spacing - adjust for 16:9 format
    plot.margin = margin(25, 40, 25, 25),
    panel.spacing = unit(3, "lines"),
    
    # Remove legend
    legend.position = "none"
  ) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                     breaks = scales::pretty_breaks(n = 6),
                     expand = expansion(mult = c(0.18, 0.18)))

# Save both versions
ggsave("results/figures/fig_factor_analysis_dumbell_pairwise.png", plot = p, 
       width = 8, height = 7, dpi = 600, bg = "white")

ggsave("results/figures/fig_factor_analysis_dumbell_pres_pairwise.png", plot = p_pres, 
       width = 16, height = 9, dpi = 300, bg = "white")

cat("\nPlot saved as: results/figures/fig_factor_analysis_dumbell_pairwise.png (600 DPI)\n")



