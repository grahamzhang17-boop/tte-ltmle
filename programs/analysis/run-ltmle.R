library(dplyr)
library(tidyr)
library(ltmle)

#  Load data
setwd("/home/rstudio-server/study")
TMLE_data <- readRDS("data/processed/standardized_age_baseline_tmle_data.rds")

testing_data <- TMLE_data
nrow(testing_data)

#testing_data <- testing_data %>% filter(studystartage > 60)
testing_data <- testing_data %>% distinct(ID, .keep_all = TRUE)
nrow(testing_data)

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
Lnodes <- grep("^age_", colnames, value = TRUE)
Ynodes <- grep("^dementia_", colnames, value = TRUE)

list(Anodes = Anodes, Cnodes = Cnodes, Lnodes = Lnodes, Ynodes = Ynodes)

#  Define treatment regimes
abar <- list(
  always = rep(1, num_bins),  # treated at all time points
  never  = rep(0, num_bins)   # never treated
)

#  Ensure censoring variables are factors
testing_data <- testing_data %>%
  mutate(across(all_of(Cnodes), ~ factor(.x, levels = c("uncensored", "censored"))))

#  Run LTMLE
result <- ltmle(
  data = testing_data,
  Anodes = Anodes,
  Cnodes = Cnodes,
  Lnodes = Lnodes,
  Ynodes = Ynodes,
  survivalOutcome = TRUE,
  abar = abar,
  SL.library = "SL.glm",
  gbounds = c(0.01, 0.99),
  estimate.time = FALSE
)

summary(result)
saveRDS(result, file = "data/processed/efficient_standardized_age_baseline_result.rds")
