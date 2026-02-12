library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)

setwd("/home/rstudio-server/study")
final_ltmle <- readRDS("data/processed/ltmle_by_regime_model_time_varying.rds")
all_gvals <- readRDS("data/processed/ltmle_by_regime_g_values_time_varying.rds")

# Font
showtext_auto()
font_add("Times New Roman", "programs/plotting/final_tables/tnr.ttf")

# Recover g-values from saved results
gvals_long <- all_gvals %>%
  pivot_longer(
    cols = everything(),
    names_to = "regime",
    values_to = "g"
  ) %>%
  filter(!is.na(g), g > 0)

# Label regimes
regime_labels <- c(
  "zeros_0" = "Disease at Baseline",
  "zeros_1" = "Disease at Year 5",
  "zeros_2" = "Disease at Year 10",
  "zeros_3" = "Disease at Year 15",
  "zeros_4" = "Disease at Year 20",
  "zeros_5" = "No Disease"
)

# Function to create a faceted density plot showing the distribution of g-values for each regime
ggplot(gvals_long, aes(x = g)) +
  
  geom_density(
    aes(y = ..count.. / sum(..count..) * 100),  # convert to percentage
    fill = "grey70",
    color = "black",
    linewidth = 0.2
  ) +
  
  # Include rug marks for density
  geom_rug(
    data = gvals_long %>% sample_frac(0.01),
    sides = "b",
    alpha = 0.2,
    linewidth = 0.2
  ) +
  
  # Include a positivity threshold at 0.01.
  geom_vline(
    xintercept = 0.01,
    linetype = "dashed",
    color = "red",
    linewidth = 0.2
  ) +
  
  scale_x_log10(
    limits = c(1e-6, 1),
    labels = scientific,
    breaks = c(1e-6, 1e-4, 1e-2, 1)
  ) +
  
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) + 
  
  coord_cartesian(
    xlim = c(1e-6, 1),
    ylim = c(0, 4)
  ) + 
  
  facet_wrap(
    ~ regime,
    ncol = 3,
    #scales = "free",
    axes = "all",
    labeller = labeller(regime = regime_labels)
  ) +
  
  labs(
    x = "Treatment Probabilities (log scale)",
    y = "Percentage of Observations",
    title = "Distribution of Cumulative Treatment Probabilities by Treatment Regime"
  ) +
  
  theme_pub(base_size = 10, base_family = "Times New Roman") +

  # Theming
  theme(
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10, angle = 90),
    
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    
    plot.caption = element_text(
      size = 7,
      hjust = 0.5,
      color = "black",
      margin = margin(t = 5)
    ),
    
    panel.grid.major.y = element_blank(),#element_line(color = "grey88", size = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.line.y = element_line(color = "black", size = 0.3),
    axis.line.x = element_line(color = "black", size = 0.3),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      size = 0.35
    ),
    
    strip.background = element_blank(),
    strip.text = element_text(size = 8, face = "bold")
  )

# Save
ggsave(
  filename = "programs/plotting/final_tables/ltmle_by_regime_by_g_plot.pdf",
  width = 150 / 25.4,
  height = 100 / 25.4,
  units = "in",
  device = cairo_pdf
)

