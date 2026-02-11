library(dplyr)
library(tidyr)
library(ltmle)

#  Load data
setwd("/home/rstudio-server/study")
TMLE_data <- readRDS("data/processed/standardized_age_baseline_tmle_data.rds")
testing_data <- TMLE_data

num_bins <- 5

#  Remove unnecessary columns
testing_data <- testing_data %>%
  select(-ID, -age, -dementiadate, -mouthinfdate, -studystartage,
         -lostdate, -deathdate, -diabetesdate, -htdate,
         -cvddate, -obesitydate, -rfdate, -radate, -copddate, -studystartdate)

#  Define TMLE nodes
colnames <- names(testing_data)

Anodes <- grep("^mouthinf_", colnames, value = TRUE)
Cnodes <- grep("^censor_", colnames, value = TRUE)
Lnodes <- unlist(
  lapply(1:num_bins, function(t) {
    c(
      paste0("age_", t),
      paste0("diabetes_", t),
      paste0("ht_", t),
      paste0("cvd_", t),
      paste0("obesity_", t),
      paste0("rf_", t),
      paste0("ra_", t),
      paste0("copd_", t)
    )
  })
)
Ynodes <- grep("^dementia_", colnames, value = TRUE)

list(Anodes = Anodes, Cnodes = Cnodes, Lnodes = Lnodes, Ynodes = Ynodes)

# Always treated and never treated regimes
abar <- list(
  always = rep(1, num_bins),
  never  = rep(0, num_bins)
)

#  Ensure censoring variables are factors
testing_data <- testing_data %>%
  mutate(across(all_of(Cnodes), ~ factor(.x, levels = c("uncensored", "censored"))))

#  Collect data on truncation, percent truncated, causal estimands, CIs
summary_df <- data.frame(
  g_bound_lower = numeric(),
  g_bound_upper = numeric(),
  
  pct_truncated = numeric(),
  
  mean_estimate_always = numeric(),
  mean_se_always = numeric(),
  mean_CI_lower_always = numeric(),
  mean_CI_upper_always = numeric(),
  mean_pvalue_always = numeric(),
  
  mean_estimate_never = numeric(),
  mean_se_never = numeric(),
  mean_CI_lower_never = numeric(),
  mean_CI_upper_never = numeric(),
  mean_pvalue_never = numeric(),
  
  RR = numeric(),
  RR_se = numeric(),
  RR_CI_lower = numeric(),
  RR_CI_upper = numeric(),
  RR_pvalue = numeric(),
  
  OR = numeric(),
  OR_se = numeric(),
  OR_CI_lower = numeric(),
  OR_CI_upper = numeric(),
  OR_pvalue = numeric(),
  
  stringsAsFactors = FALSE
)

# Function to extract g-values, handles edge cases
safe_extract <- function(x, row = 1, col = 1) {
  if (is.null(x)) return(NA)
  if (is.matrix(x) && nrow(x) >= row && ncol(x) >= col) return(x[row, col])
  if (length(x) >= row) return(x[row])
  return(NA)
}

# Helper function to get the percent of truncated g-values
calculate_percent_truncated <- function(df_g_filtered, g_threshold) {
  num_truncated <- sum(df_g_filtered$g_value < g_threshold, na.rm = TRUE)
  total_values <- sum(!is.na(df_g_filtered$g_value))
  
  percent_truncated <- (num_truncated / total_values) * 100
  
  return(percent_truncated)
}

# Function to get percent truncated for a given regime and time point
get_pct_truncated <- function(result, node_of_interest, regime_of_interest, g_threshold) {
  g_array <- result$cum.g.unbounded
  
  # Collect data
  df_g <- as.data.frame(as.table(g_array))
  colnames(df_g) <- c("row", "node", "regime", "g_value")
  
  df_g <- df_g %>%
    mutate(
      row = as.integer(row),
      node = as.integer(node),
      regime = as.integer(regime)
    )
  
  # Filter for the node and regime of interest
  df_g_filtered <- df_g %>%
    filter(node == node_of_interest, regime == regime_of_interest)
  
  # Calculate the percent of truncated g-values
  percent_truncated <- calculate_percent_truncated(df_g_filtered, g_threshold)
  
  return(percent_truncated = percent_truncated)
}



g_bounds_list <- list(
  c(0.008, 1.000),
  c(0.010, 1.000),
  c(0.012, 1.000),
  c(0.014, 1.000)
)

# Loop over truncation values
for (gb in g_bounds_list) {
  
  cat("Running TMLE for g-bound = (", gb[1], ", ", gb[2], ")\n")
  
  result_gb <- ltmle(
    data = testing_data,
    Anodes = Anodes,
    Lnodes = Lnodes,
    Cnodes = Cnodes,
    Ynodes = Ynodes,
    survivalOutcome = TRUE,
    abar = abar,
    SL.library = "SL.glm",
    gbounds = gb,
    estimate.time = FALSE
  )
  
  sum_res <- summary(result_gb)$effect.measures
  
  summary_df <- rbind(summary_df, data.frame(
    g_bound_lower = gb[1],
    g_bound_upper = gb[2],
    
    pct_truncated = get_pct_truncated(result_gb, 5, 1, gb[1]),
    
    mean_estimate_always = safe_extract(sum_res$treatment$estimate),
    mean_se_always = safe_extract(sum_res$treatment$std.dev),
    mean_CI_lower_always = safe_extract(sum_res$treatment$CI, 1, 1),
    mean_CI_upper_always = safe_extract(sum_res$treatment$CI, 1, 2),
    mean_pvalue_always = safe_extract(sum_res$treatment$pvalue),
    
    mean_estimate_never = safe_extract(sum_res$control$estimate),
    mean_se_never = safe_extract(sum_res$control$std.dev),
    mean_CI_lower_never = safe_extract(sum_res$control$CI, 1, 1),
    mean_CI_upper_never = safe_extract(sum_res$control$CI, 1, 2),
    mean_pvalue_never = safe_extract(sum_res$control$pvalue),
    
    RR = safe_extract(sum_res$RR$estimate),
    RR_se = safe_extract(sum_res$RR$std.dev),
    RR_CI_lower = safe_extract(sum_res$RR$CI, 1, 1),
    RR_CI_upper = safe_extract(sum_res$RR$CI, 1, 2),
    RR_pvalue = safe_extract(sum_res$RR$pvalue),
    
    OR = safe_extract(sum_res$OR$estimate),
    OR_se = safe_extract(sum_res$OR$std.dev),
    OR_CI_lower = safe_extract(sum_res$OR$CI, 1, 1),
    OR_CI_upper = safe_extract(sum_res$OR$CI, 1, 2),
    OR_pvalue = safe_extract(sum_res$OR$pvalue)
  ))
}

summary(summary_df)
saveRDS(summary_df, file = "data/processed/ltmle_by_g_summary_time_varying.rds")
