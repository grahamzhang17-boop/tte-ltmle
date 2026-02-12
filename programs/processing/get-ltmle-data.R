library(dplyr)
library(tidyr)
library(lubridate)

# Numeric variables to include
vars <- c(
  "Participant.ID",
  "Any_all_dementia_diag_made",
  "Any_mouth_infections_diag_made",
  "Recruitment_age",
  "Sex",
  "Household_income_group_2",
  "BMI",
  "Disability_group",
  "Days_physical_activity_group_2",
  "Alchohol_intake_2",
  "Smoking_status"
)
# All variables to include (including categorical)
all_vars <- c(
  "Participant.ID",
  "Date.of.attending.assessment.centre...Instance.0",
  "Any_all_dementia_diag_made",
  "First_date_all_dementia_diag",
  "Any_mouth_infections_diag_made",
  "First_date_mouth_infections_diag",
  "Date.lost.to.follow.up",
  "Date.of.death",
  "Recruitment_age",
  "Sex",
  "Household_income_group_2",
  "BMI",
  "Disability_group",
  "Days_physical_activity_group_2",
  "Alchohol_intake_2",
  "Smoking_status",
  "diabetesdate",
  "htdate",
  "cvddate",
  "obesitydate",
  "rfdate",
  "radate",
  "copddate"
)

# Renaming to neater names
rename_map <- c(
  ID = "Participant.ID",
  baselinedate = "Date.of.attending.assessment.centre...Instance.0",
  dementia = "Any_all_dementia_diag_made",
  dementiadate = "First_date_all_dementia_diag",
  mouthinf = "Any_mouth_infections_diag_made",
  mouthinfdate = "First_date_mouth_infections_diag",
  lostdate = "Date.lost.to.follow.up",
  deathdate = "Date.of.death",
  age = "Recruitment_age",
  sex = "Sex",
  income = "Household_income_group_2",
  bmi = "BMI",
  disability = "Disability_group",
  physactivity = "Days_physical_activity_group_2",
  alcohol = "Alchohol_intake_2",
  smoking = "Smoking_status"
)

# Load and merge all data sources (both raw and processed)
setwd("/home/rstudio-server/study")

comorbidity_data <- readRDS("data/processed/comorbidity_data.rds")
study_data       <- readRDS("data/processed/study_data.rds")
follow_up_data   <- read.csv("data/raw/follow_up_data.csv")
data_death       <- read.csv("data/raw/data_death.csv")

testing_data <- study_data %>%
  left_join(follow_up_data, by = "Participant.ID") %>%
  left_join(data_death, by = "Participant.ID") %>%
  left_join(comorbidity_data, by = "Participant.ID") %>%
  select(all_of(all_vars)) %>%
  drop_na(all_of(vars)) %>%
  rename(!!!rename_map)

# Ensure correct variables are factors
testing_data <- testing_data %>%
  mutate(
    mouthinf = ifelse(mouthinf == "YES", 1, 0),
    dementia = ifelse(dementia == "Yes dementia diagnosis made", 1, 0),
    sex = as.factor(sex),
    lostdate = na_if(lostdate, "")
  ) %>%
  mutate(across(c(baselinedate, dementiadate, mouthinfdate,
                  lostdate, deathdate), as.Date))

# Set study start date (UK Biobank assessment date - 5 years)
testing_data <- testing_data %>%
  mutate(
    studystartdate = baselinedate - years(5),
    studystartage  = age - 5  # optional: age at study start
  )

# Remove events before study start
testing_data <- testing_data %>%
  filter(is.na(dementiadate) | dementiadate >= studystartdate) %>%
  filter(is.na(mouthinfdate) | mouthinfdate >= studystartdate)

# Apply administrative censoring
administrative_censoring <- as.Date("2026-01-01")

testing_data <- testing_data %>%
  mutate(
    censor_date = pmin(lostdate, deathdate, administrative_censoring, na.rm = TRUE),
    censor_date = ifelse(is.infinite(censor_date), NA, censor_date),
    censor_date = as.Date(censor_date, origin = "1970-01-01")
  )

# Time discretization, sort dates into correct bins
bin_size_years <- 5
num_bins <- 5

date_vars_to_bin <- c(
  "dementiadate", "mouthinfdate", "lostdate", "deathdate",
  "censor_date",
  "diabetesdate", "htdate", "cvddate",
  "obesitydate", "rfdate", "radate", "copddate"
)

get_time_bin_index <- function(date, start_date, bin_size_years) {
  date <- as.Date(date)
  start_date <- as.Date(start_date)
  
  years_elapsed <- as.numeric(difftime(date, start_date, units = "days")) / 365.25
  bin_index <- floor(years_elapsed / bin_size_years) + 1
  bin_index[bin_index < 1] <- 1
  return(bin_index)
}

binned_data <- testing_data %>%
  transmute(
    ID,
    age,
    across(
      all_of(date_vars_to_bin),
      ~ as.integer(get_time_bin_index(.x, studystartdate, bin_size_years)),
      .names = "{.col}"
    )
  )

# 8. Create matrices for ltmle analysis (time bin x individuals)
binned_data <- binned_data %>%
  left_join(
    testing_data %>% select(ID, studystartage) %>% distinct(ID, .keep_all = TRUE),
    by = "ID"
  )

# Age
age_bins <- binned_data %>%
  transmute(
    ID,
    !!!setNames(
      lapply(1:num_bins, function(i) binned_data$studystartage + bin_size_years * (i - 1)),
      paste0("age_", 1:num_bins)
    )
  )

# EHelper function to create matrix given a date variable
create_event_matrix <- function(event_bin, num_bins, prefix) {
  mat <- sapply(1:num_bins, function(t) as.integer(!is.na(event_bin) & t >= event_bin))
  colnames(mat) <- paste0(prefix, "_", 1:num_bins)
  as_tibble(mat)
}

# Outcome/exposure matrices
dementia_bins <- bind_cols(tibble(ID = binned_data$ID),
                           create_event_matrix(binned_data$dementiadate, num_bins, "dementia"))

mouthinf_bins <- bind_cols(tibble(ID = binned_data$ID),
                           create_event_matrix(binned_data$mouthinfdate, num_bins, "mouthinf"))

# Time-varying comorbidities matrix
timevarying_covars <- c("diabetesdate", "htdate", "cvddate",
                        "obesitydate", "rfdate", "radate", "copddate")

timevarying_bins_list <- lapply(timevarying_covars, function(var) {
  bind_cols(
    tibble(ID = binned_data$ID),
    create_event_matrix(binned_data[[var]], num_bins, gsub("date", "", var))
  )
})

# Censoring matrix (unique because we use strings "uncensored" vs "censored")
create_censor_matrix <- function(censor_bin, num_bins, prefix) {
  mat <- t(sapply(censor_bin, function(bin) {
    if (is.na(bin)) {
      rep(1L, num_bins)
    } else if (bin >= num_bins) {
      rep(1L, num_bins)
    } else {
      c(rep(1L, bin), rep(0L, num_bins - bin))
    }
  }))
  mat_chr <- ifelse(mat == 1L, "uncensored", "censored")
  colnames(mat_chr) <- paste0(prefix, "_", 1:num_bins)
  as_tibble(mat_chr)
}

censor_bins <- bind_cols(
  tibble(ID = binned_data$ID),
  create_censor_matrix(binned_data$censor_date, num_bins, "censor")
)

# Build final TMLE dataset
TMLE_data <- testing_data %>%
  select(
    ID, sex, income, bmi, disability, physactivity, alcohol, smoking,
    age, studystartdate, studystartage,
    dementiadate, mouthinfdate, lostdate, deathdate,
    diabetesdate, htdate, cvddate, obesitydate, rfdate, radate, copddate
  )

# Remove ID before combining
mouthinf_only <- mouthinf_bins %>% select(-ID)
censor_only   <- censor_bins   %>% select(-ID)
age_only      <- age_bins      %>% select(-ID)
dementia_only <- dementia_bins %>% select(-ID)
timevarying_flat <- dplyr::bind_cols(lapply(timevarying_bins_list, \(df) df %>% select(-ID)))

# Interleave matrices by bin
interleaved_cols <- lapply(1:num_bins, function(i) {
  idx <- seq(i, ncol(timevarying_flat), by = num_bins)
  
  tibble(
    mouthinf = mouthinf_only[[i]],
    censor   = censor_only[[i]],
    age      = age_only[[i]],
    !!!timevarying_flat[, idx, drop = FALSE],
    dementia = dementia_only[[i]]
  ) %>%
    rename_with(~ paste0(.x, "_", i), .cols = c("mouthinf", "censor", "age", "dementia"))
}) %>% bind_cols()

TMLE_data <- bind_cols(TMLE_data, interleaved_cols)

# Save
saveRDS(TMLE_data, "data/processed/standardized_age_baseline_tmle_data.rds")
