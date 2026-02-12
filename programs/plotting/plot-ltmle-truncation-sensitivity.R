library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tableone)
library(gtsummary)
library(table1)
library(gt)
library(knitr)
library(kableExtra)
library(tibble)
library(survival)

setwd("/home/rstudio-server/study")
ltmle_results <- readRDS("data/processed/ltmle_by_g_summary_time_varying.rds")

# Recover ltmle estimates by truncation bound used from saved results
final_ltmle_results <- ltmle_results %>%
  select(
    g_bound_lower, pct_truncated,
    mean_estimate_always, mean_CI_lower_always, mean_CI_upper_always,
    mean_estimate_never, mean_CI_lower_never, mean_CI_upper_never
  )


final_ltmle_results <- final_ltmle_results %>%
  rename(
    estimate_unexposed = mean_estimate_never,
    CI_lower_unexposed = mean_CI_lower_never,
    CI_upper_unexposed = mean_CI_upper_never,
    estimate_exposed = mean_estimate_always,
    CI_lower_exposed = mean_CI_lower_always,
    CI_upper_exposed = mean_CI_upper_always
  )

final_ltmle_results

# Calculate average treatment effects and risk ratios for each truncation bound used
make_effect_table <- function(df) {
  df %>%
    mutate(
      # Standard errors from CIs
      SE_unexposed = (CI_upper_unexposed - CI_lower_unexposed)/(2*1.96),
      SE_exposed   = (CI_upper_exposed - CI_lower_exposed)/(2*1.96),
      
      # ATE
      ATE = estimate_exposed - estimate_unexposed,
      SE_ATE = sqrt(SE_unexposed^2 + SE_exposed^2),
      CI_lower_ATE = ATE - 1.96*SE_ATE,
      CI_upper_ATE = ATE + 1.96*SE_ATE,
      
      # Risk ratio
      RR = estimate_exposed / estimate_unexposed,
      SE_log_RR = sqrt( (SE_exposed/estimate_exposed)^2 + (SE_unexposed/estimate_unexposed)^2 ),
      CI_lower_RR = exp(log(RR) - 1.96*SE_log_RR),
      CI_upper_RR = exp(log(RR) + 1.96*SE_log_RR)
    ) %>%
    transmute(
      "Truncation Bound" = g_bound_lower,
      "Percent Truncated (%)" = sprintf("%.0f%%", pct_truncated),
      "Disease at Baseline Risk (%)"     = sprintf("%.2f%% (%.2f – %.2f)", estimate_exposed*100, CI_lower_exposed*100, CI_upper_exposed*100),
      "No Disease Risk (%)"   = sprintf("%.2f%% (%.2f – %.2f)", estimate_unexposed*100, CI_lower_unexposed*100, CI_upper_unexposed*100),
      "Risk Difference (%)"   = sprintf("%.2f%% (%.2f – %.2f)", ATE*100, CI_lower_ATE*100, CI_upper_ATE*100),
      "Risk Ratio"         = sprintf("%.2f (%.2f – %.2f)", RR, CI_lower_RR, CI_upper_RR)
    )
}

ltmle_table <- make_effect_table(final_ltmle_results)

# Label headers
rownames(ltmle_table) <- NULL
ltmle_table <- ltmle_table %>%
  select("Truncation Bound", "Percent Truncated (%)", "Disease at Baseline Risk (%)", "No Disease Risk (%)", 
         "Risk Difference (%)", "Risk Ratio")

# Theming
ltmle_table %>%
  kbl(
    format = "html",
    caption = "Table 4: LTMLE Estimates by truncation bound Used",
    align = c("r", "r", "r", "r", "r", "r")
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
    general = "95% Confidence intervals in parentheses",
    general_title = "",
    footnote_as_chunk = TRUE) %>%
  save_kable("programs/plotting/final_tables/ltmle_by_g.html")







