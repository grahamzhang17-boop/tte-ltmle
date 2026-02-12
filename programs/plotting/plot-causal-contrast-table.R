library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(viridis)
library(showtext)
library(kableExtra)
library(knitr)

setwd("/home/rstudio-server/study")
summary_df <- readRDS("data/processed/ltmle_by_regime_model_time_varying.rds")
unadjusted_df <- readRDS("data/processed/unadjusted_summaries_by_regime.rds")

# Label each exposure regime
regime_labels <- c(
  "zeros_0" = "Disease at Baseline",
  "zeros_1" = "Disease at Year 5",
  "zeros_2" = "Disease at Year 10",
  "zeros_3" = "Disease at Year 15",
  "zeros_4" = "Disease at Year 20",
  "zeros_5" = "No Disease"
)

# 1) Get standard errors from CIs and 2) rename labels
regime_estimates <- summary_df %>%
  mutate(
    regime_label = regime_labels[regime],
    se = (CI_upper - CI_lower) / (2 * 1.96)  # recover SE from CI
  ) %>%
  select(regime, regime_label, estimate, CI_lower, CI_upper, se)

# Regimes to contrast
contrast_definitions <- tibble::tribble(
  ~A,        ~B,
  "zeros_0", "zeros_5",
  "zeros_1", "zeros_5",
  "zeros_2", "zeros_5"
)

# Helper function to calculate risk ratios and risk differences for each causal contrast
compute_causal_contrasts <- function(regime_df, contrasts_df) {
  
  contrasts_df %>%
    left_join(regime_df, by = c("A" = "regime")) %>%
    rename_with(~ paste0(.x, "_A"),
                c(regime_label, estimate, CI_lower, CI_upper, se)) %>%
    
    left_join(regime_df, by = c("B" = "regime")) %>%
    rename_with(~ paste0(.x, "_B"),
                c(regime_label, estimate, CI_lower, CI_upper, se)) %>%
    
    mutate(
      # Risk Difference
      RD = estimate_A - estimate_B,
      RD_se = sqrt(se_A^2 + se_B^2),
      RD_lower = RD - 1.96 * RD_se,
      RD_upper = RD + 1.96 * RD_se,
      
      # Risk Ratio
      RR = estimate_A / estimate_B,
      logRR_se = sqrt((se_A / estimate_A)^2 + (se_B / estimate_B)^2),
      RR_lower = exp(log(RR) - 1.96 * logRR_se),
      RR_upper = exp(log(RR) + 1.96 * logRR_se),
      
      contrast_label = paste0(regime_label_A, " vs ", regime_label_B)
    )
}

# Run helper function, get table
contrast_table <- compute_causal_contrasts(
  regime_df   = regime_estimates,
  contrasts_df = contrast_definitions
)

# Assemble table, format columns and headers
final_table <- contrast_table %>%
  transmute(
    "Contrast (Regime A vs B)" = contrast_label,
    
    "Risk under A (%)" = paste0(
      percent(estimate_A, accuracy = 0.01), " (",
      percent(CI_lower_A, accuracy = 0.01), ", ",
      percent(CI_upper_A, accuracy = 0.01), ")"
    ),
    
    "Risk under B (%)" = paste0(
      percent(estimate_B, accuracy = 0.01), " (",
      percent(CI_lower_B, accuracy = 0.01), ", ",
      percent(CI_upper_B, accuracy = 0.01), ")"
    ),
    
    "Risk Difference (%)" =
      paste0(
        percent(RD, accuracy = 0.01), " (",
        percent(RD_lower, accuracy = 0.01), ", ",
        percent(RD_upper, accuracy = 0.01), ")"
      ),
    
    "Risk Ratio" =
      paste0(
        round(RR, 3), " (",
        round(RR_lower, 3), ", ",
        round(RR_upper, 3), ")"
      )
  )

rownames(final_table) <- NULL

# Theming
final_table %>%
  kbl(
    format = "html",
    caption = "Comparison of 25-year dementia probabilities across supported exposure regimes.",
    align = c("l", "r", "r", "r", "r")
  ) %>%
  theme_table_pub(
    full_width = FALSE,
    font_size = 14,
    font_family = "Times New Roman"
  ) %>%
  column_spec(
    1,
    extra_css = "white-space: nowrap;"
  ) %>%
  footnote(
    general = "95% Confidence intervals in parentheses. Exposure regimes with poor support were excluded.",
    general_title = "",
    footnote_as_chunk = TRUE
  ) %>%
  save_kable(
    "programs/plotting/final_tables/causal_contrast_table.html"
  )





