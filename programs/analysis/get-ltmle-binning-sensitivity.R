library(dplyr)
library(tidyr)
library(ltmle)

# Helper function, runs ltmle analysis on a given dataset and returns estimates by exposure regime.
run_tmle_on_data <- function(data_path) {
  cat("Running TMLE for dataset = (", data_path, ")\n")
  dat <- readRDS(data_path)
  
  # Remove unnecessary columns
  dat <- dat %>%
    select(-ID, -age, -dementiadate, -mouthinfdate, -studystartage,
           -lostdate, -deathdate, -diabetesdate, -htdate,
           -cvddate, -obesitydate, -rfdate, -radate, -copddate, -studystartdate)
  
  # Get the number of bins from the file path
  num_bins <- max(as.numeric(gsub(".*_(\\d+)$", "\\1", grep("^dementia_", names(dat), value = TRUE))))
  
  dementia_cols <- paste0("dementia_", 1:num_bins)
  mouthinf_cols <- paste0("mouthinf_", 1:num_bins)
  censor_cols   <- paste0("censor_",   1:num_bins)

  # Name ltmle columns
  Anodes <- mouthinf_cols
  Cnodes <- censor_cols
  Ynodes <- dementia_cols
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
  
  # Ensure censoring variables are factors
  dat <- dat %>% mutate(across(all_of(Cnodes), ~ factor(.x, levels = c("uncensored", "censored"))))
  
  # Use exposed since baseline vs never exposed as exposure regimes
  abar <- list(always = rep(1, num_bins), never = rep(0, num_bins))
  
  # Run ltmle
  result <- ltmle(
    data = dat,
    Anodes = Anodes,
    Lnodes = Lnodes,
    Cnodes = Cnodes,
    Ynodes = Ynodes,
    survivalOutcome = TRUE,
    abar = abar,
    SL.library = "SL.glm",
    gbounds = c(0.01, 0.99),
    estimate.time = FALSE
  )
  
  # Helper function to extract ltmle estimates, handles edge cases
  safe_extract <- function(x, row = 1, col = 1) {
    if (is.null(x)) return(NA)
    if (is.matrix(x) && nrow(x) >= row && ncol(x) >= col) return(x[row, col])
    if (length(x) >= row) return(x[row])
    return(NA)
  }
  
  # Get ltmle causal estimates
  sum_res <- summary(result)$effect.measures

  # Get relevant information: mean + CIs, RR + CIs, OR + CIs between the two regimes
  df <- data.frame(
    dataset = basename(data_path),
    num_bins = num_bins,
    
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
    OR_pvalue = safe_extract(sum_res$OR$pvalue),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

# Run helper function on datasets of varying time discretization procedures (i.e. 8 intervals of 3 years each)
data_files <- list(
  "data/processed/tmle_data_3_by_8.rds",
  "data/processed/tmle_data_4_by_6.rds",
  "data/processed/tmle_data_5_by_5.rds",
  "data/processed/tmle_data_6_by_4.rds"
)

results_list <- lapply(data_files, run_tmle_on_data)
summary_df <- bind_rows(results_list)

summary(summary_df)
saveRDS(summary_df, file = "data/processed/ltmle_by_bin_size_summary_time_varying.rds")
