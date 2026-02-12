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

#  Ensure censoring variables are factors
testing_data <- testing_data %>%
  mutate(across(all_of(Cnodes), ~ factor(.x, levels = c("uncensored", "censored"))))

#  Create exposure regimes corresponding to disease at Year 0, Year 5, Year 10, etc...
regime_list <- lapply(0:num_bins, function(s) {
  c(rep(0, s), rep(1, num_bins - s))
})
names(regime_list) <- paste0("zeros_", 0:num_bins)

# Function to extract data, handles edge cases
safe_extract <- function(x, row = 1, col = 1) {
  if (is.null(x)) return(NA)
  if (is.matrix(x)) {
    if (nrow(x) >= row && ncol(x) >= col) return(x[row, col])
  } else {
    if (length(x) >= row) return(x[row])
  }
  return(NA)
}

#  Function to compute mean + IQR of g-values
compute_g_summary <- function(result_reg) {
  g_array <- result_reg$cum.g.unbounded
  dims <- length(dim(g_array))
  df_g <- as.data.frame(as.table(g_array))

  # Handle edge case
  if (dims == 3) {
    colnames(df_g) <- c("row", "node", "regime", "g_value")
  } else if (dims == 2) {
    colnames(df_g) <- c("row", "node", "g_value")
    df_g$regime <- 1  # add regime column manually
  }
  
  df_g <- df_g %>%
    mutate(
      row = as.integer(row),
      node = as.integer(node),
      regime = as.integer(regime)
    )
  
  # Set last node (final time point)
  last_node <- max(df_g$node, na.rm = TRUE)
  df_g_last_node <- df_g %>% filter(node == last_node)
  
  # Summary statistics
  g_median <- median(df_g_last_node$g_value, na.rm = TRUE)
  Q1_g    <- quantile(df_g_last_node$g_value, 0.25, na.rm = TRUE)
  Q3_g    <- quantile(df_g_last_node$g_value, 0.75, na.rm = TRUE)
  min_g   <- min(df_g_last_node$g_value, na.rm = TRUE)
  max_g   <- max(df_g_last_node$g_value, na.rm = TRUE)
  
  return(list(g_median = g_median, Q1_g = Q1_g, Q3_g = Q3_g, min_g = min_g, max_g = max_g))
}

#  Function to extract all g-values from a given ltmle result call
extract_all_gvalues <- function(result_reg, regime_name, abar_used) {
  g_array <- result_reg$cum.g.unbounded
  dims <- length(dim(g_array))
  
  df_g <- as.data.frame(as.table(g_array))
  names(df_g)[ncol(df_g)] <- "g_value"

  # Edge case
  if (dims == 3) {
    colnames(df_g)[1:3] <- c("row", "node", "regime_index")
  } else if (dims == 2) {
    colnames(df_g)[1:2] <- c("row", "node")
    df_g$regime_index <- 1
  }
  
  df_g <- df_g %>%
    mutate(
      row = as.integer(row),
      node = as.integer(node),
      regime_index = as.integer(regime_index),
      g_value = as.numeric(g_value)
    )
  
  # Use last node (latest time point)
  last_node <- max(df_g$node, na.rm = TRUE)
  df_last_node <- df_g %>%
    filter(node == last_node) %>%
    mutate(
      regime_name = regime_name,
      abar = paste(abar_used, collapse = ",")
    )

  return(df_last_node)
}

#  Run ltmle for each exposure regime
summary_df <- data.frame()
all_gvals  <- data.frame()

for (regime_name in names(regime_list)) {

  # Set regime
  abar_vec <- regime_list[[regime_name]]
  cat("Running TMLE for regime:", regime_name, "=", paste(abar_vec, collapse=","), "\n")
  
  result_reg <- ltmle(
    data = testing_data,
    Anodes = Anodes,
    Lnodes = Lnodes,
    Cnodes = Cnodes,
    Ynodes = Ynodes,
    survivalOutcome = TRUE,
    abar = abar_vec,
    SL.library = "SL.glm",
    gbounds = c(0.01, 0.99),
    estimate.time = FALSE
  )
  
  # Get summary statistics
  sum_res <- summary(result_reg)
  g_stats <- compute_g_summary(result_reg)

  # Add summary statistics to final result
  summary_df <- rbind(summary_df, data.frame(
    regime = regime_name,
    abar = paste(abar_vec, collapse=","),
    estimate = safe_extract(sum_res$treatment$estimate),
    CI_lower = safe_extract(sum_res$treatment$CI, 1, 1),
    CI_upper = safe_extract(sum_res$treatment$CI, 1, 2),
    g_median = g_stats$g_median,
    g_Q1   = g_stats$Q1_g,
    g_Q3   = g_stats$Q3_g,
    g_min  = g_stats$min_g,
    g_max  = g_stats$max_g,
    stringsAsFactors = FALSE
  ))

  # Get all g-values using helper
  g_full_df <- extract_all_gvalues(result_reg, regime_name, abar_vec)
  g_values <- g_full_df$g_value
  
  # Add all g-values to final dataframe
  if (ncol(all_gvals) == 0) {
    all_gvals <- data.frame(g_values)
    colnames(all_gvals) <- regime_name
  } else {
    all_gvals[[regime_name]] <- g_values
  }
}

# Save results
row.names(summary_df) <- NULL
row.names(all_gvals) <- NULL

saveRDS(summary_df, "data/processed/ltmle_by_regime_model_time_varying.rds")
saveRDS(all_gvals,  "data/processed/ltmle_by_regime_g_values_time_varying.rds")


