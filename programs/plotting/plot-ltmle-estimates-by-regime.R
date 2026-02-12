library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(viridis)
library(showtext)
library(stringr)

setwd("/home/rstudio-server/study")
tmle_df <- readRDS("data/processed/ltmle_by_regime_model_time_varying.rds")
unadjusted_df <- readRDS("data/processed/unadjusted_summaries_by_regime.rds")

# Label regimes
regime_labels <- c(
  "zeros_0" = "Disease at Baseline",
  "zeros_1" = "Disease at Year 5",
  "zeros_2" = "Disease at Year 10",
  "zeros_3" = "Disease at Year 15",
  "zeros_4" = "Disease at Year 20",
  "zeros_5" = "No Disease"
)

tmle_plot_df <- tmle_df %>%
  mutate(
    years = factor(regime, levels = names(regime_labels),
                   labels = regime_labels),
    group = factor(regime_labels[regime], levels = regime_labels)
  ) %>%
  select(years, group, estimate, CI_lower, CI_upper)

# Specifiy positivity violations
tmle_plot_df$positivity_violation <- c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)

# Recover unadjusted estimates from saved results
unadjusted_plot_df <- unadjusted_df %>%
  mutate(
    years = factor(regime, levels = names(regime_labels),
                   labels = regime_labels),
    group = factor(regime_labels[regime], levels = regime_labels),
    type = "Unadjusted"
  ) %>%
  filter(estimate > 0)  # skip zeros like zeros_4

# Specify positivity violations
unadjusted_plot_df$positivity_violation <- c(FALSE, FALSE, FALSE, TRUE, FALSE)

# Add for legend
tmle_plot_df$type <- "LTMLE"

# Combine for plotting
plot_df <- bind_rows(tmle_plot_df, unadjusted_plot_df)

# Font
showtext_auto()
font_add("Times New Roman", "programs/plotting/final_tables/tnr.ttf")

# Function to plot estimates by regime
plot_estimates_by_regime <- function(df, title) {
  
  ggplot(df, aes(x = years, y = estimate, color = type, shape = type)) +
    
    # LTMLE estimates + CIs
    geom_point(
      data = df %>% filter(type == "LTMLE"),
      aes(alpha = !positivity_violation),
      size = 1.8,
      position = position_dodge(width = 0.4)
    ) +
    geom_errorbar(
      data = df %>% filter(type == "LTMLE"),
      aes(ymin = CI_lower, ymax = CI_upper, alpha = !positivity_violation),
      color = "black",
      width = 0.20,
      size = 0.35,
      position = position_dodge(width = 0.4)
    ) +
    
    # Unadjusted points: hollow and red
    geom_point(
      data = df %>% filter(type == "Unadjusted"),
      aes(alpha = !positivity_violation),
      size = 2,
      position = position_dodge(width = 0.4)
    ) +
    
    scale_alpha_manual(values = c(0.3, 1), guide = "none") +
    
    scale_shape_manual(values = c("LTMLE" = 16, "Unadjusted" = 1)) +
    
    scale_color_manual(values = c("LTMLE" = "black", "Unadjusted" = "red")) +
    
    # Set Y-axis scale
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 0.1),
      limits = c(0, 0.10),
      breaks = seq(0, 0.10, 0.02),
      expand = expansion(mult = c(0, 0))
    ) +
    
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    
    # Labelling
    labs(
      title = title,
      x = "Exposure Regime",
      y = "Estimated Probability",
      color = "Estimate Type",
      shape = "Estimate Type",
      caption = "Faded or missing points indicate regimes with poor support; estimates should be interpreted with caution."
    ) +
    
    # Legend
    theme_pub(base_size = 10, base_family = "Times New Roman") +
    theme(
      legend.position = c(0.02, 0.98),       # top-left inset
      legend.justification = c(0, 1),
      #legend.background = element_rect(fill = "white", color = "black", size = 0.3),
      legend.key = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 7),
      legend.title = element_blank(),
      
      axis.title.y = element_text(angle = 90),
      axis.text.x = element_text(size = 8),
      plot.caption = element_text(size = 7, hjust = 0.5, color = "black", margin = margin(t = 5)),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.y = element_line(color = "black", size = 0.3),
      axis.line.x = element_line(color = "black", size = 0.3),
      panel.border = element_rect(color = "black", fill = NA, size = 0.35)
    )
}

# Generate plot
final_plot <- plot_estimates_by_regime(
  plot_df,
  "LTMLE Probability Estimates by Regime"
)

print(final_plot)

# Save
ggsave(
  "programs/plotting/final_tables/ltmle_summaries_by_regime.pdf",
  final_plot,
  width = 150/25.4,
  height = 105/25.4,
  units = "in",
  device = cairo_pdf
)
