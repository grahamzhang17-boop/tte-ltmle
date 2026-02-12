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

# Ensure dementia_5 is a factor and label values
if (!is.factor(testing_data$dementia_5)) {
  testing_data <- testing_data %>%
    mutate(
      dementia_5 = factor(
        dementia_5,
        levels = c(0, 1),
        labels = c("No Incident Dementia", "Incident Dementia")
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


# Variables to include
vars <- c("age", "sex", "income", "bmi", "disability", 
          "physactivity", "alcohol", "smoking")

# Get total N of each group size
group_counts <- testing_data %>%
  group_by(dementia_5) %>%
  summarise(N = n(), .groups = "drop")

col_no_dementia <- paste0("No Incident Dementia<br>(N = ", 
                          group_counts$N[group_counts$dementia_5 == "No Incident Dementia"], ")")
col_inc_dementia <- paste0("Incident Dementia<br>(N = ", 
                           group_counts$N[group_counts$dementia_5 == "Incident Dementia"], ")")

# Function to get summary statistics for a given variable
summarize_by_group <- function(df, var) {
  # If numeric, get mean [SD]
  if (is.numeric(df[[var]])) {
    df %>%
      group_by(dementia_5) %>%
      summarise(Value = sprintf("%.2f (%.2f)", mean(.data[[var]], na.rm = TRUE),
                                sd(.data[[var]], na.rm = TRUE)),
                .groups = "drop") %>%
      pivot_wider(names_from = dementia_5, values_from = Value) %>%
      mutate(
        Variable = label(df[[var]]),
        Level = ""
      ) %>%
      select(Variable, Level, everything())
  } 
  # If categorical, get total N [% of total population]
  else {
    df %>%
      group_by(.data[[var]], dementia_5) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(dementia_5) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(`N (%)` = paste0(n, " (", sprintf("%.1f", pct), "%)")) %>%
      select(Level = !!var, dementia_5, `N (%)`) %>%
      pivot_wider(names_from = dementia_5, values_from = `N (%)`) %>%
      mutate(Variable = label(df[[var]])) %>%
      select(Variable, Level, everything())
  }
}

# Apply function to all covariates
descriptives_list <- lapply(vars, function(v) summarize_by_group(testing_data, v))
descriptives_df <- bind_rows(descriptives_list)

# Indent categorical values
descriptives_df <- descriptives_df %>%
  mutate(Level = ifelse(Level == "", "", paste0("  ", Level)))

# Create kable with variables as a header and level as a sub-header
kbl_obj <- kable(
  descriptives_df,
  format = "html",
  escape = FALSE,
  col.names = c("Variable", "Level", col_no_dementia, col_inc_dementia),
  caption = "Table 1: Descriptive Statistics Stratified by Dementia Status"
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

save_kable(kbl_html, "outputs/all/baseline_characteristics.html")

