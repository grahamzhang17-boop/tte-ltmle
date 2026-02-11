library(dplyr)
library(tidyr)
library(ltmle)

#  Load data
setwd("/home/rstudio-server/study")
TMLE_data <- readRDS("data/processed/standardized_age_baseline_tmle_data.rds")
testing_data <- TMLE_data


# Assign treatment regimes to oral disease exposure sequences
testing_data <- testing_data %>%
  mutate(
    mouthinf_seq = paste0(mouthinf_1, mouthinf_2, mouthinf_3, mouthinf_4, mouthinf_5),
    regime = case_when(
      mouthinf_seq == "11111" ~ "zeros_0",
      mouthinf_seq == "01111" ~ "zeros_1",
      mouthinf_seq == "00111" ~ "zeros_2",
      mouthinf_seq == "00011" ~ "zeros_3",
      mouthinf_seq == "00001" ~ "zeros_4",
      mouthinf_seq == "00000" ~ "zeros_5",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(regime))

# Get unadjusted dementia risk for each regime
unadjusted_df <- testing_data %>%
  mutate(dementia_by_bin5 = pmax(dementia_1, dementia_2, dementia_3, dementia_4, dementia_5)) %>%
  group_by(regime) %>%
  summarise(
    n = n(),
    dementia_cases = sum(dementia_by_bin5),
    estimate = dementia_cases / n
  ) %>%
  arrange(regime)

unadjusted_df

saveRDS(unadjusted_df, "data/processed/unadjusted_summaries_by_regime.rds")
