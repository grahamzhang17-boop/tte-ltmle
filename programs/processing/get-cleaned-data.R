library(dplyr)
library(tidyr)
library(purrr)
library(table1)
library(lubridate)

# Note: Many variables included here are exploratory and not included in the final analyses

# READ SAVED CSV FILES
data_participant1 <- read.csv("data_participant1.csv")
data_participant2 <- read.csv("data_participant2.csv")
data_participant3 <- read.csv("data_participant3.csv")
data_mouth_infections <- read.csv("data_mouth_infections.csv")

patient_exclude <- read.csv("patient_exclude.csv", header = FALSE)[[1]]


# MERGE DATAFRAMES
df_list <- list(data_participant1,
                data_participant2,
                data_participant3,
                data_mouth_infections
)
merged_data <- reduce(df_list, full_join, by = "Participant.ID")



# DIAGNOSIS COLUMNS - USE THIS TO SPECIFY ANY DIAGNOSES FIELDS - WILL ALWAYS TAKE EARLIEST DATE
get_diagnosis_info <- function(data, date_fields, diagnosis_name, track_source = FALSE) {
  first_date_col <- paste0("First_date_", diagnosis_name, "_diag")
  diag_made_col  <- paste0("Any_", diagnosis_name, "_diag_made")
  source_col     <- paste0("Source_", diagnosis_name, "_diag")
  
  results <- apply(data[, date_fields, drop = FALSE], 1, function(x) {
    x <- x[x != "" & !is.na(x)]
    if (length(x) > 0) {
      parsed_dates <- parse_date_time(
        x,
        orders = c("Y-m-d", "d/m/Y", "m/d/Y", "d-b-Y", "d-B-Y", "Ymd"),
        quiet = TRUE
      )
      parsed_dates <- parsed_dates[!is.na(parsed_dates)]
      if (length(parsed_dates) > 0) {
        min_date <- min(parsed_dates, na.rm = TRUE)
        # If tracking, also find which column it came from
        if (track_source) {
          idx <- which(parsed_dates == min_date)[1]  # first match if ties
          source_name <- names(x)[idx]
          return(list(date = min_date, source = source_name))
        } else {
          return(list(date = min_date, source = ""))
        }
      }
    }
    return(list(date = NA, source = ""))
  })
  
  data[[first_date_col]] <- sapply(results, function(r) if (!is.na(r$date)) as.Date(r$date) else NA)
  data[[diag_made_col]]  <- ifelse(!is.na(data[[first_date_col]]), "YES", "NO")
  if (track_source) {
    data[[source_col]] <- sapply(results, function(r) r$source)
  }
  return(data)
}



alz_date_fields <- c(
  "Date.of.alzheimer.s.disease.report",
  "Date.G30.first.reported..alzheimer.s.disease.",
  "Date.F00.first.reported..dementia.in.alzheimer.s.disease."
)
merged_data <- get_diagnosis_info(merged_data, alz_date_fields, "alzheimers", track_source = FALSE)

diabetes_date_fields <- c(
  "Date.E10.first.reported..insulin.dependent.diabetes.mellitus.",         
  "Date.E11.first.reported..non.insulin.dependent.diabetes.mellitus.",
  "Date.E12.first.reported..malnutrition.related.diabetes.mellitus.",
  "Date.E13.first.reported..other.specified.diabetes.mellitus.",
  "Date.E14.first.reported..unspecified.diabetes.mellitus."
)
merged_data <- get_diagnosis_info(merged_data, diabetes_date_fields, "diabetes", track_source = FALSE)

dementia_date_fields <- c(
  "Date.of.alzheimer.s.disease.report",                                        
  "Date.of.all.cause.dementia.report",                                      
  "Date.of.vascular.dementia.report",                                   
  "Date.of.frontotemporal.dementia.report",                                
  "Date.F00.first.reported..dementia.in.alzheimer.s.disease.",               
  "Date.F01.first.reported..vascular.dementia.",                           
  "Date.F02.first.reported..dementia.in.other.diseases.classified.elsewhere.",
  "Date.F03.first.reported..unspecified.dementia.",                    
  "Date.G30.first.reported..alzheimer.s.disease."
)
merged_data <- get_diagnosis_info(merged_data, dementia_date_fields, "all_dementia", track_source = FALSE)

mouth_infections_date_fields <- c(
  "Date.K02.first.reported..dental.caries.",                                   
  "Date.K03.first.reported..other.diseases.of.hard.tissues.of.teeth.",         
  "Date.K04.first.reported..diseases.of.pulp.and.periapical.tissues.",
  "Date.K05.first.reported..gingivitis.and.periodontal.diseases.",
  "Date.K12.first.reported..stomatitis.and.related.lesions.",                  
  "Date.K13.first.reported..other.diseases.of.lip.and.oral.mucosa.",           
  "Date.K11.first.reported..diseases.of.salivary.glands.",                     
  "Date.K14.first.reported..diseases.of.tongue."
)
merged_data <- get_diagnosis_info(merged_data, mouth_infections_date_fields,
                                  "mouth_infections", track_source = TRUE)

mouth_infections_reduced_date_fields <- c(
  "Date.K02.first.reported..dental.caries.",                                   
  "Date.K03.first.reported..other.diseases.of.hard.tissues.of.teeth.",         
  "Date.K04.first.reported..diseases.of.pulp.and.periapical.tissues.",
  "Date.K05.first.reported..gingivitis.and.periodontal.diseases.",
  "Date.K12.first.reported..stomatitis.and.related.lesions.",                  
  "Date.K13.first.reported..other.diseases.of.lip.and.oral.mucosa."
)
merged_data <- get_diagnosis_info(merged_data, mouth_infections_date_fields,
                                  "mouth_infections_reduced", track_source = TRUE)

periodontal_disease_date_fields <- c(
  "Date.K05.first.reported..gingivitis.and.periodontal.diseases."
)
merged_data <- get_diagnosis_info(merged_data, periodontal_disease_date_fields,
                                  "PD", track_source = TRUE)





# EXPLORATORY VARIABLES - OUTCOME AND EXPOSURE
dental_problems <- c("Mouth ulcers", "Painful gums", "Bleeding gums", 
                     "Loose teeth", "Toothache", "Dentures")
dental_cols <- grep("^Mouth\\.teeth\\.dental\\.problems\\.\\.\\.Instance\\.", 
                    names(merged_data), value = TRUE)
for (symptom in dental_problems) {
  col_base <- gsub(" ", "_", symptom)
  for (d_col in dental_cols) {
    instance_num <- sub(".*Instance\\.(\\d+)$", "\\1", d_col)
    new_col <- paste0(col_base, "_Instance_", instance_num)
    merged_data[[new_col]] <- ifelse(
      merged_data[[d_col]] %in% c("", "Prefer not to answer", "Do not know"),
      NA,
      ifelse(grepl(symptom, merged_data[[d_col]], fixed = TRUE), "YES", "NO")
    )
  }
}




# FILTER DATAFRAME BY INCLUSION CRITERIA
criteria <- list(
  min_age = 60
)
cat("Initial N:", nrow(merged_data), "\n")

# Step 1: Age filter
step1 <- merged_data %>%
  dplyr::filter(Age.at.recruitment >= criteria$min_age)
cat("Removed for age <", criteria$min_age, ":", 
    nrow(merged_data) - nrow(step1), "\n")
cat("Remaining after age filter:", nrow(step1), "\n\n")

# Step 2: Lost to follow-up filter
#step2 <- step1 %>%
#  dplyr::filter(Reason.lost.to.follow.up == "")
#cat("Removed for lost to follow-up:", 
#    nrow(step1) - nrow(step2), "\n")
#cat("Remaining after follow-up filter:", nrow(step2), "\n\n")

# Step 3: Exclude patient IDs
filtered_data <- step1 %>%
  dplyr::filter(!(Participant.ID %in% patient_exclude))
cat("Removed due to exclusion list:", 
    nrow(step1) - nrow(filtered_data), "\n")
cat("Final remaining:", nrow(filtered_data), "\n")



# CLEAN ALL VARIABLES (DICHOTOMIZE AND REMOVE NAs)
cleaned_data <- filtered_data
cleaned_data <- cleaned_data %>% mutate(Recruitment_age = as.numeric(Age.at.recruitment))
cleaned_data <- cleaned_data %>% mutate(BMI = as.numeric(Body.mass.index..BMI....Instance.0))


# CATEGORICAL ETHNICITY
cleaned_data <- cleaned_data %>%
  mutate(
    Ethnic_group = case_when(
      Ethnic.background...Instance.0 %in% c("African",
                                            "Any other Black background",
                                            "Black or Black British",
                                            "Caribbean") ~ "Black",
      Ethnic.background...Instance.0 %in% c("Any other Asian background",
                                            "Asian or Asian British",
                                            "Bangladeshi",
                                            "Chinese",
                                            "Indian",
                                            "Pakistani") ~ "Asian",
      Ethnic.background...Instance.0 %in% c("Any other white background",
                                            "British",
                                            "Irish",
                                            "White") ~ "White",
      Ethnic.background...Instance.0 %in% c("Any other mixed background",
                                            "Mixed",
                                            "White and Asian",
                                            "White and Black African",
                                            "White and Black Caribbean") ~ "Mixed",
      Ethnic.background...Instance.0 %in% c("Other ethnic group") ~ "Other",
      is.na(Ethnic.background...Instance.0) ~ NA_character_,
      TRUE ~ "Uncategorized"
    ),
    Ethnic_group = factor(Ethnic_group,
                          levels = c("White", "Black", "Asian", "Mixed", "Other", "Uncategorized"))
  )



# DICHOTOMIZED AGE TO COMPLETE FULL TIME EDUCATION
cleaned_data <- cleaned_data %>%
  mutate(
    Schooling_age_continuous = as.numeric(
      ifelse(Age.completed.full.time.education...Instance.0 == "Never went to school", 0,
             ifelse(Age.completed.full.time.education...Instance.0 %in% c("Do not know", "Prefer not to answer", ""), NA,
                    Age.completed.full.time.education...Instance.0))
    ),
    Schooling_age_group = case_when(
      Schooling_age_continuous < 15 ~ "<15",
      Schooling_age_continuous >= 15 & Schooling_age_continuous < 19 ~ "15-18",
      Schooling_age_continuous >= 19 & Schooling_age_continuous < 29 ~ "19-28",
      Schooling_age_continuous >= 29 ~ "29+"
    ),
    Schooling_age_group = factor(Schooling_age_group, levels = c("<15","15-18","19-28","29+"))
  )



# CATEGORICAL TOTAL HOUSEHOLD INCOME
cleaned_data <- cleaned_data %>%
  mutate(
    Household_income_group = as.factor(
      ifelse(Average.total.household.income.before.tax...Instance.0 %in% c("Do not know", "Prefer not to answer"), NA,
             Average.total.household.income.before.tax...Instance.0)
    ),
    Household_income_group = factor(Household_income_group, levels = c("Less than 18,000","18,000 to 30,999",
                                                                       "31,000 to 51,999","52,000 to 100,000",
                                                                       "Greater than 100,000"))
  )


# BINARY DISABILITY STATUS
cleaned_data <- cleaned_data %>%
  mutate(
    Disability_group = as.factor(
      ifelse(Attendance.disability.mobility.allowance...Instance.0 %in% c("Do not know", "Prefer not to answer", ""), NA,
             ifelse(Attendance.disability.mobility.allowance...Instance.0 == "None of the above", "NO", "YES"))
    )
  )



# BINARY LEISURE SOCIAL ACTIVITY
cleaned_data <- cleaned_data %>%
  mutate(
    Social_activity = as.factor(
      ifelse(Leisure.social.activities...Instance.0 %in% c("Do not know", "Prefer not to answer", ""), NA,
             ifelse(Leisure.social.activities...Instance.0 == "None of the above", "NO", "YES"))
    )
  )



# DICHOTOMIZED NUMBER OF DAYS OF PHYSICAL ACTIVITY PER WEEK
cleaned_data <- cleaned_data %>%
  mutate(
    Days_physical_activity_continuous =
      as.numeric(ifelse(Number.of.days.week.of.moderate.physical.activity.10..minutes...Instance.0 %in% c("Do not know", "Prefer not to answer", ""), NA,
                        Number.of.days.week.of.moderate.physical.activity.10..minutes...Instance.0)
      ),
    Days_physical_activity_group = case_when(
      Days_physical_activity_continuous < 2 ~ "0-1",
      Days_physical_activity_continuous >= 2 & Days_physical_activity_continuous < 5 ~ "2-4",
      Days_physical_activity_continuous >= 5 & Days_physical_activity_continuous < 8 ~ "5-7"
    ),
    Days_physical_activity_group = factor(Days_physical_activity_group, levels = c("0-1","2-4","5-7"))
  )



# DICHOTOMIZED ALCHOHOL INTAKE
cleaned_data <- cleaned_data %>%
  mutate(
    Alchohol_intake = case_when(
      Alcohol.intake.frequency....Instance.0 %in% c("Do not know", "Prefer not to answer", "") ~ NA,
      Alcohol.intake.frequency....Instance.0 %in% c("Never", "One to three times a month", "Special occasions only") ~ "Low",
      Alcohol.intake.frequency....Instance.0 %in% c("Once or twice a week") ~ "Moderate",
      Alcohol.intake.frequency....Instance.0 %in% c("Three or four times a week", "Daily or almost daily") ~ "High"
    ),
    Alchohol_intake = factor(Alchohol_intake, levels = c("Low","Moderate","High"))
  )



# DICHOTOMIZED SMOKING STATUS
cleaned_data <- cleaned_data %>%
  mutate(
    Smoking_status = case_when(
      as.character(Smoking.status...Instance.0) %in% c("Do not know", "Prefer not to answer", "") ~ NA_character_,
      as.character(Smoking.status...Instance.0) %in% c("Never") ~ "Never",
      as.character(Smoking.status...Instance.0) %in% c("Previous") ~ "Previous",
      as.character(Smoking.status...Instance.0) %in% c("Current") ~ "Current"
    ),
    Smoking_status = factor(Smoking_status, levels = c("Never","Previous","Current"))
  )

cleaned_data <- cleaned_data %>%
  mutate(
    Population_density = case_when(
      Home.area.population.density...urban.or.rural...Instance.0 %in% c("England/Wales - Hamlet and Isolated dwelling - sparse",
                                                                        "England/Wales - Town and Fringe - sparse",
                                                                        "England/Wales - Village - sparse",
                                                                        "England/Wales - Hamlet and Isolated Dwelling - less sparse",
                                                                        "England/Wales - Town and Fringe - less sparse",
                                                                        "England/Wales - Village - less sparse",
                                                                        "Scotland - Accessible Rural",
                                                                        "Scotland - Remote Rural",
                                                                        "Scotland - Very Remote Rural") ~ "Rural",
      Home.area.population.density...urban.or.rural...Instance.0 %in% c("England/Wales - Urban - sparse",
                                                                        "Scotland - Accessible Small Town",
                                                                        "Scotland - Other Urban Area",
                                                                        "Scotland - Remote Small Town",
                                                                        "England/Wales - Urban - less sparse",
                                                                        "Scotland - Large Urban Area") ~ "Urban",
      Home.area.population.density...urban.or.rural...Instance.0 %in% c("",
                                                                        "Prefer not to answer",
                                                                        "Do not know") ~ NA_character_
    ),
    Population_density = factor(Population_density,
                                levels = c("Rural", "Urban"))
  )

# RELABELED ALZHEIMERS DIAGNOSIS
cleaned_data$Any_alzheimers_diag_made <- factor(cleaned_data$Any_alzheimers_diag_made,
                                                levels = c("NO", "YES"),
                                                labels = c("No Alzheimer's diagnosis made", "Yes Alzheimer's diagnosis made"))
cleaned_data$Any_all_dementia_diag_made <- factor(cleaned_data$Any_all_dementia_diag_made,
                                                  levels = c("NO", "YES"),
                                                  labels = c("No dementia diagnosis made", "Yes dementia diagnosis made"))



# GENERATE EXPOSURE DEFINITIONS - Only subjects with exposure before outcome
cleaned_data <- cleaned_data %>%
  dplyr::mutate(
    Mouth_infections_exposure = dplyr::case_when(
      Any_mouth_infections_diag_made == "YES" &
        !is.na(First_date_mouth_infections_diag) &
        (is.na(First_date_alzheimers_diag) |
           First_date_mouth_infections_diag < First_date_alzheimers_diag) ~ "YES",
      TRUE ~ "NO"
    )
  )
cat("Initial mouth infection cases:", sum(cleaned_data$Any_mouth_infections_diag_made == "YES"), "\n")
cat("Mouth infection exposure cases:", sum(cleaned_data$Mouth_infections_exposure == "YES"), "\n")


# cleaned_data
write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)
cleaned_data <- read.csv("cleaned_data.csv")



