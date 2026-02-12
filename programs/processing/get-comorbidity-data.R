library(dplyr)
library(tidyr)
library(purrr)

# Data collection from UK Biobank's electronic medical history archive

# Define grouped lists of variable names by disease
comorbidity_data <- read.csv("comorbidities.csv", stringsAsFactors = FALSE)


diabetes_vars <- c(
  "Date.E10.first.reported..insulin.dependent.diabetes.mellitus.",
  "Date.E11.first.reported..non.insulin.dependent.diabetes.mellitus.",
  "Date.E12.first.reported..malnutrition.related.diabetes.mellitus.",
  "Date.E13.first.reported..other.specified.diabetes.mellitus.",
  "Date.E14.first.reported..unspecified.diabetes.mellitus."
)

hypertension_vars <- c(
  "Date.I10.first.reported..essential..primary..hypertension.",
  "Date.I15.first.reported..secondary.hypertension."
)

cardiovascular_vars <- c(
  "Date.I64.first.reported..stroke..not.specified.as.haemorrhage.or.infarction.",
  "Date.I21.first.reported..acute.myocardial.infarction.",
  "Date.I22.first.reported..subsequent.myocardial.infarction."
)

obesity_vars <- c(
  "Date.E66.first.reported..obesity."
)

renal_failure_vars <- c(
  "Date.N17.first.reported..acute.renal.failure.",
  "Date.N18.first.reported..chronic.renal.failure.",
  "Date.N19.first.reported..unspecified.renal.failure."
)

rheumatoid_arthritis_vars <- c(
  "Date.M05.first.reported..seropositive.rheumatoid.arthritis.",
  "Date.M06.first.reported..other.rheumatoid.arthritis."
)

copd_vars <- c(
  "Date.of.chronic.obstructive.pulmonary.disease.report"
)

# Define labels / naming conventions
comorbidity_groups <- list(
  diabetes = diabetes_vars,
  hypertension = hypertension_vars,
  cardiovascular = cardiovascular_vars,
  obesity = obesity_vars,
  renalfailure = renal_failure_vars,
  rheumatoidarthritis = rheumatoid_arthritis_vars,
  copd = copd_vars
)

naming_map <- c(
  diabetes = "diabetesdate",
  hypertension = "htdate",
  cardiovascular = "cvddate",
  obesity = "obesitydate",
  renalfailure = "rfdate",
  rheumatoidarthritis = "radate",
  copd = "copddate"
)

# Function to extract earliest date per row
get_earliest_date <- function(df, cols) {
  df_subset <- df %>% select(any_of(cols))
  
  # Convert character to Date
  df_subset <- df_subset %>%
    mutate(across(everything(), ~ as.Date(.x, format = "%Y-%m-%d")))
  
  apply(df_subset, 1, function(x) {
    if (all(is.na(x))) NA else min(x, na.rm = TRUE)
  }) %>% as.Date(origin = "1970-01-01")  # Ensure Date type
}

# Loop through groups and add new columns
for (group in names(comorbidity_groups)) {
  vars <- comorbidity_groups[[group]]
  new_col <- naming_map[[group]]
  
  comorbidity_data[[new_col]] <- get_earliest_date(comorbidity_data, vars)
}

# Save
comorbidity_data <- comorbidity_data %>%
  select(Participant.ID, all_of(unname(naming_map)))

head(comorbidity_data, 50)
summary(comorbidity_data)

saveRDS(comorbidity_data, file = "comorbidity_data.rds")





