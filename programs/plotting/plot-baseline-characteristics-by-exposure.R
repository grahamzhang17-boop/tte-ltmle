library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tableone)
library(gtsummary)
library(table1)
library(kableExtra)
library(knitr)

setwd("/home/rstudio-server/study")
TMLE_data <- readRDS("data/processed/standardized_age_baseline_tmle_data.rds")
testing_data <- TMLE_data

# Ensure mouthinf_5 is a factor and set labels
if (!is.factor(testing_data$mouthinf_5)) {
  testing_data <- testing_data %>%
    mutate(
      mouthinf_5 = factor(
        mouthinf_5,
        levels = c(0, 1),
        labels = c("No Incident Oral Infection", "Incident Oral Infection")
      )
    )
}

# Label variables
label(testing_data$age)         <- "Age at Recruitment (Mean [SD])"
label(testing_data$sex)         <- "Sex (%)"
label(testing_data$income)      <- "Household Income (%)"
label(testing_data$bmi)         <- "Body Mass Index (Mean [SD])"
label(testing_data$disability)  <- "Disability Status (%)"
label(testing_data$physactivity)<- "Physical Activity, Days/Wk (%)"
label(testing_data$alcohol)     <- "Alcohol Intake (%)"
label(testing_data$smoking)     <- "Smoking Status (%)"

label(testing_data$diabetes_1)     <- "Baseline Diabetes (%)"
label(testing_data$ht_1)     <- "Baseline Hypertension (%)"
label(testing_data$cvd_1)     <- "Baseline Cardiovascular Disease (%)"
label(testing_data$obesity_1)     <- "Baseline Obesity (%)"
label(testing_data$rf_1)     <- "Baseline Renal Failure (%)"
label(testing_data$ra_1)     <- "Baseline Rheumatoid Arthritis (%)"
label(testing_data$copd_1)     <- "Baseline Chronic Obstructive Pulmonary Disease (%)"

testing_data <- testing_data %>%
  mutate(
    diabetes_1 = factor(diabetes_1, levels = c(0, 1), labels = c("NO", "YES")),
    ht_1       = factor(ht_1,       levels = c(0, 1), labels = c("NO", "YES")),
    cvd_1      = factor(cvd_1,      levels = c(0, 1), labels = c("NO", "YES")),
    obesity_1  = factor(obesity_1,  levels = c(0, 1), labels = c("NO", "YES")),
    rf_1       = factor(rf_1,       levels = c(0, 1), labels = c("NO", "YES")),
    ra_1       = factor(ra_1,       levels = c(0, 1), labels = c("NO", "YES")),
    copd_1     = factor(copd_1,     levels = c(0, 1), labels = c("NO", "YES"))
  )

# Variables to summarize
vars <- c(
  "age", "sex", "income", "bmi", "disability",
  "physactivity", "alcohol", "smoking",
  "diabetes_1", "ht_1", "cvd_1", "obesity_1",
  "rf_1", "ra_1", "copd_1"
)

vars <- c(
  "age", "sex", "income", "bmi", "disability",
  "physactivity", "alcohol", "smoking"
)

# Calculate total group sizes
group_counts <- testing_data %>%
  group_by(mouthinf_5) %>%
  summarise(N = n(), .groups = "drop")

col_no_dementia <- paste0("No Incident Oral Infection<br>(N = ", 
                          group_counts$N[group_counts$mouthinf_5 == "No Incident Oral Infection"], ")")
col_inc_dementia <- paste0("Incident Oral Infection<br>(N = ", 
                           group_counts$N[group_counts$mouthinf_5 == "Incident Oral Infection"], ")")

# Function get summary statistics by group (oral disease vs no oral disease).
summarize_by_group <- function(df, var) {
  var_label <- label(df[[var]])
  if (is.null(var_label)) var_label <- var  # fallback

  # If numeric variable, get mean [SD]
  if (is.numeric(df[[var]])) {
    df %>%
      group_by(mouthinf_5) %>%
      summarise(Value = sprintf("%.2f (%.2f)", mean(.data[[var]], na.rm = TRUE),
                                sd(.data[[var]], na.rm = TRUE)),
                .groups = "drop") %>%
      pivot_wider(names_from = mouthinf_5, values_from = Value) %>%
      mutate(
        Variable = var_label,
        Level = ""
      ) %>%
      select(Variable, Level, everything())
  } 
  # If categorical, get percentage
  else { 
    df %>%
      group_by(.data[[var]], mouthinf_5) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(mouthinf_5) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(`N (%)` = paste0(n, " (", sprintf("%.1f", pct), "%)")) %>%
      select(Level = !!sym(var), mouthinf_5, `N (%)`) %>%
      pivot_wider(names_from = mouthinf_5, values_from = `N (%)`) %>%
      mutate(Variable = var_label) %>%
      select(Variable, Level, everything())
  }
}

# Apply function to all covariates
descriptives_list <- lapply(vars, function(v) summarize_by_group(testing_data, v))
descriptives_df <- bind_rows(descriptives_list)

# Indent categorical values
descriptives_df <- descriptives_df %>%
  mutate(Level = ifelse(Level == "", "", paste0("  ", Level)))

# Create a kable which has variables as main headers and categorical levels as sub-headers
kbl_obj <- kable(
  descriptives_df,
  format = "html",
  escape = FALSE,
  col.names = c("Variable", "Level", col_no_dementia, col_inc_dementia),
  caption = "Table 1: Descriptive Statistics Stratified by Oral Infection Status"
)

# Theming
kbl_html <- theme_table_pub(
  kable_input = kbl_obj,
  font_size = 14,
  font_family = "Times New Roman",
  stripe_color = "#f5f5f5",
  full_width = FALSE,
  position = "center"
) %>%
  collapse_rows(columns = 1, valign = "top")  # merge 'Variable' column

kbl_html

save_kable(kbl_html, "programs/plotting/final_tables/baseline_characteristics_by_exposure.html")

