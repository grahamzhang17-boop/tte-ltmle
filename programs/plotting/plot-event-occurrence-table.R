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

setwd("/home/rstudio-server/study")
TMLE_data <- readRDS("data/processed/standardized_age_baseline_tmle_data.rds")
testing_data <- TMLE_data

# Parameters
bin_size_years <- 5
num_bins <- 5

# Convert censoring variables to numeric
censor_cols <- paste0("censor_", 1:num_bins)
testing_data <- testing_data %>%
  mutate(across(all_of(censor_cols), ~ ifelse(. == "censored", 1, 0)))

# Time-varying variables to include
timevarying_vars <- c(
  "censor", "mouthinf", "dementia",
  "diabetes", "ht", "cvd", "obesity",
  "rf", "ra", "copd"
)

# Get summary of each variable at each time-point
time_summary <- data.frame(Bin = 1:num_bins)

for (var in timevarying_vars) {
  counts <- sapply(1:num_bins, function(t) {
    colname <- paste0(var, "_", t)
    if (colname %in% names(testing_data)) {
      sum(testing_data[[colname]] == 1, na.rm = TRUE)
    } else {
      NA
    }
  })
  time_summary[[var]] <- counts
}

# Get the year of each bin (bin 1 = Year 5, bin 2 = 10, etc...)
years_mapping <- (1:num_bins) * bin_size_years
time_summary <- time_summary %>%
  mutate(Years = years_mapping) %>%
  select(Years, everything(), -Bin)

# Label variables
var_labels <- c(
  censor = "Censored",
  mouthinf = "Oral Infection",
  dementia = "Dementia",
  diabetes = "Diabetes",
  ht = "Hypertension",
  cvd = "CVD",
  obesity = "Obesity",
  rf = "RF",
  ra = "RA",
  copd = "COPD"
)

# Apply labels
colnames(time_summary) <- c(
  "Years",
  unname(var_labels[timevarying_vars])
)

# Theming
time_summary %>%
  kbl(
    format = "html",
    caption = "Cumulative Number of Subjects with Observed Event Over Study Period"
  ) %>%
  theme_table_pub(
    font_size = 14,
    font_family = "Times New Roman",
    full_width = FALSE
  ) %>%
  footnote(
    general = "Abbreviations: CVD = Cardiovascular Disease; RA = Rheumatoid Arthritis; RF = Rheumatic Fever; COPD = Chronic Obstructive Pulmonary Disease.",
    general_title = "",
    footnote_as_chunk = TRUE
  ) %>%
  save_kable("programs/plotting/final_tables/event_occurrence_table.html")


