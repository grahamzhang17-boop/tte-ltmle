library(dplyr)
library(tidyr)
library(purrr)
library(table1)
library(lubridate)
library(stringr)
library(arsenal)

# Additional cleaning and data collection
# Note: Many variables included here are exploratory and were not included in the final analyses

road_data <- read.csv("nearest_major_road.csv")
cleaned_data <- read.csv("cleaned_data.csv")
testing_data <- cleaned_data

# IMPORT NEAREST MAJOR ROAD DATA
testing_data <- testing_data %>%
  left_join(road_data, by = "Participant.ID")

# RENAME ASSESSMENT CENTRE
testing_data <- testing_data %>%
  rename(Assessment_centre = UK.Biobank.assessment.centre...Instance.0)


# FURTHER CREATE VARIABLES FOR ANALYSIS
# LOG INVERSE NEAREST ROAD DISTANCE
testing_data$Log_inverse_nearest_road_distance <- as.numeric(testing_data$Inverse.distance.to.the.nearest.major.road)
testing_data$Log_inverse_nearest_road_distance <- log(testing_data$Log_inverse_nearest_road_distance)


# RECRUITMENT AGE GROUP
testing_data$Recruitment_age_group <- cut(
  testing_data$Age.at.recruitment,
  breaks = c(60, 65, Inf),     # 60–64, then 65+
  right = FALSE,               # include lower bound, exclude upper (so 60–64 works cleanly)
  include.lowest = TRUE,
  labels = c("60-64", "65+")
)
unique(testing_data$Recruitment_age_group)


# EDUCATION QUALIFICATION
qual_ranking <- c(
  "College or University degree",
  "Other professional qualifications eg: nursing, teaching",
  "NVQ or HND or HNC or equivalent",
  "A levels/AS levels or equivalent",
  "O levels/GCSEs or equivalent",
  "CSEs or equivalent"
)
get_highest_qual <- function(qual_string, priority_list) {
  if (qual_string %in% c("", "Prefer not to answer", "Do not know")) {
    return(NA)
  }
  if (qual_string == "None of the above") {
    return("None")
  }
  split_strings <- str_split(qual_string, "\\|")[[1]] %>% str_trim()
  for (item in priority_list) {
    if (item %in% split_strings) {
      return(item)
    }
  }
  return(NA)
}
testing_data <- testing_data %>%
  rowwise() %>%
  mutate(Highest_education = get_highest_qual(Qualifications...Instance.0, qual_ranking)) %>%
  ungroup() %>%
  mutate(
    Highest_education = factor(
      Highest_education,
      levels = c(
        "College or University degree",
        "Other professional qualifications eg: nursing, teaching",
        "NVQ or HND or HNC or equivalent",
        "A levels/AS levels or equivalent",
        "O levels/GCSEs or equivalent",
        "CSEs or equivalent",
        "None"
      ),
      labels = c(
        "4-year college degree",
        "Professional qualification (teaching, nursing, etc.)",
        "2-year college / vocational (HND/HNC/NVQ)",
        "High school (A-levels/AS)",
        "Some high school (O-levels/GCSEs)",
        "Lower secondary (CSE)",
        "None of the above"
      )
    )
  )
table(testing_data$Highest_education, useNA = "ifany")



# EDUCATION LEVEL
testing_data <- testing_data %>%
  mutate(
    Highest_education_2 = case_when(
      Highest_education %in% c("4-year college degree",
                               "Professional qualification (teaching, nursing, etc.)",
                               "2-year college / vocational (HND/HNC/NVQ)") ~ "College degree, other professional qualifications, or associates degree",
      Highest_education %in% c("High school (A-levels/AS)",
                               "Some high school (O-levels/GCSEs)") ~ "High school or some high school",
      Highest_education %in% c("Lower secondary (CSE)",
                               "None of the above") ~ "Less than high school or no formal education",
      is.na(Highest_education) ~ NA_character_,
      TRUE ~ "Other"
    ),
    Highest_education_2 = factor(Highest_education_2,
                                 levels = c("College degree, other professional qualifications, or associates degree",
                                            "High school or some high school",
                                            "Less than high school or no formal education"))
  )
table(testing_data$Highest_education_2, useNA = "ifany")



# ETHNICITY
testing_data <- testing_data %>%
  mutate(
    Ethnic_group_2 = case_when(
      Ethnic.background...Instance.0 %in% c("African",
                                            "Any other Black background",
                                            "Black or Black British",
                                            "White and Black Caribbean",
                                            "White and Black African") ~ "Black",
      Ethnic.background...Instance.0 %in% c("Any other white background",
                                            "British",
                                            "Irish",
                                            "White") ~ "White",
      Ethnic.background...Instance.0 %in% c("Other ethnic group") ~ "Other",
      Ethnic.background...Instance.0 %in% c("", "Prefer not to answer", "Do not know") ~ NA_character_,
      is.na(Ethnic.background...Instance.0) ~ NA_character_,
      TRUE ~ "Other"
    ),
    Ethnic_group_2 = factor(Ethnic_group,
                          levels = c("White", "Black", "Other"))
  )
# ETHNIC GROUP 3
testing_data <- testing_data %>%
  mutate(
    Ethnic_group_3 = case_when(
      Ethnic.background...Instance.0 %in% c("Any other white background",
                                            "British",
                                            "Irish",
                                            "White") ~ "White",
      Ethnic.background...Instance.0 %in% c("", "Prefer not to answer", "Do not know") ~ NA_character_,
      TRUE ~ "Non-White"  # everything else is non-white
    ),
    Ethnic_group_3 = factor(Ethnic_group_3, levels = c("White", "Non-White"))
  )


# BMI GROUP
testing_data <- testing_data %>%
  mutate(
    BMI_group = case_when(
      is.na(Body.mass.index..BMI....Instance.0) ~ NA_character_,
      Body.mass.index..BMI....Instance.0 < 25 ~ "Normal weight (<25)",
      Body.mass.index..BMI....Instance.0 >= 25 & Body.mass.index..BMI....Instance.0 < 30 ~ "Overweight (25-30)",
      Body.mass.index..BMI....Instance.0 >= 30 ~ "Obese (30+)"
    ),
    BMI_group = factor(BMI_group,
                       levels = c("Normal weight (<25)", "Overweight (25-30)", "Obese (30+)"))
  )

table(testing_data$BMI_group, useNA = "ifany")


# PHYSICAL ACTIVITY 2
testing_data <- testing_data %>%
  mutate(
    Days_physical_activity_continuous =
      as.numeric(ifelse(Number.of.days.week.of.moderate.physical.activity.10..minutes...Instance.0 %in% c("Do not know", "Prefer not to answer", ""), NA,
                        Number.of.days.week.of.moderate.physical.activity.10..minutes...Instance.0)
      ),
    Days_physical_activity_group_2 = case_when(
      Days_physical_activity_continuous < 1 ~ "0",
      Days_physical_activity_continuous >= 1 & Days_physical_activity_continuous < 3 ~ "1-2",
      Days_physical_activity_continuous >= 3 & Days_physical_activity_continuous < 5 ~ "3-4",
      Days_physical_activity_continuous >= 5 & Days_physical_activity_continuous < 8 ~ "5+",
      TRUE ~ NA
    ),
    Days_physical_activity_group_2 = factor(Days_physical_activity_group_2, levels = c("0","1-2","3-4", "5+"))
  )



# ALCHOHOL INTAKE 2
testing_data <- testing_data %>%
  mutate(
    Alchohol_intake_2 = case_when(
      Alcohol.intake.frequency....Instance.0 %in% c("Do not know", "Prefer not to answer", "") ~ NA,
      Alcohol.intake.frequency....Instance.0 %in% c("Never", "One to three times a month", "Special occasions only") ~ "Never",
      Alcohol.intake.frequency....Instance.0 %in% c("Daily or almost daily") ~ "Daily",
      Alcohol.intake.frequency....Instance.0 %in% c("Once or twice a week", "One to three times a month", "Special occasions only") ~ "Occasionally",
      Alcohol.intake.frequency....Instance.0 %in% c("Three or four times a week") ~ "Three or four times a week"
    ),
    Alchohol_intake_2 = factor(Alchohol_intake_2, levels = c("Never","Occasionally","Three or four times a week", "Daily"))
  )
# ALCHOHOL INTAKE 3
testing_data <- testing_data %>%
  mutate(
    Alchohol_intake_3 = case_when(
      Alcohol.intake.frequency....Instance.0 %in% c("Do not know", "Prefer not to answer", "") ~ NA,
      Alcohol.intake.frequency....Instance.0 %in% c("Never", "One to three times a month", "Special occasions only",
                                                    "Once or twice a week", "One to three times a month", "Special occasions only") ~ "Low (Never or occasionally)",
      Alcohol.intake.frequency....Instance.0 %in% c("Daily or almost daily", "Three or four times a week") ~ "High (Daily or three to four times a week)"
    ),
    Alchohol_intake_3 = factor(Alchohol_intake_3, levels = c("Low (Never or occasionally)", "High (Daily or three to four times a week)"))
  )



# SOCIAL ACTIVITIES
social_activities <- c(
  "Pub or social club",
  "Sports club or gym",
  "Religious group",
  "Adult education class",
  "Other group activity"
)
social_cols <- grep("^Leisure\\.social\\.activities\\.\\.\\.Instance\\.", 
                    names(testing_data), value = TRUE)

# Loop over activities and columns to create binary columns
for (activity in social_activities) {
  col_base <- gsub(" ", "_", activity)  # safe column name
  new_col <- col_base  # no instance number
  
  testing_data[[new_col]] <- apply(
    testing_data[, social_cols], 1,
    function(x) {
      x_clean <- x[!x %in% c("", "Prefer not to answer", "Do not know")]
      if (length(x_clean) == 0) {
        return(NA)              # all missing/non-informative
      } else if (any(grepl(activity, x_clean, fixed = TRUE))) {
        return("YES")           # activity present in at least one instance
      } else {
        return("NO")            # activity not present in any instance
      }
    }
  )
}
label(testing_data$Pub_or_social_club)       <- "Pub or Social Club"
label(testing_data$Sports_club_or_gym)      <- "Sports Club or Gym"
label(testing_data$Religious_group)         <- "Religious Group"
label(testing_data$Adult_education_class)  <- "Adult Education Class (Obsolete)"
label(testing_data$Other_group_activity)    <- "Other Group Activity"

# combine adult education class with other group activity
testing_data$Other_group_activity <- ifelse(
  testing_data$Adult_education_class == "YES" | testing_data$Other_group_activity == "YES",
  "YES",
  ifelse(testing_data$Adult_education_class == "NO" & testing_data$Other_group_activity == "NO", "NO", NA)
)

table(testing_data$Other_group_activity, useNA = "ifany")



# POPULATION DENSITY
testing_data <- testing_data %>%
  mutate(
    Population_density_2 = case_when(
      Home.area.population.density...urban.or.rural...Instance.0 %in% c("England/Wales - Hamlet and Isolated dwelling - sparse",
                                                                        "England/Wales - Town and Fringe - sparse",
                                                                        "England/Wales - Village - sparse",
                                                                        "England/Wales - Hamlet and Isolated Dwelling - less sparse",
                                                                        "England/Wales - Town and Fringe - less sparse",
                                                                        "England/Wales - Village - less sparse",
                                                                        "Scotland - Accessible Rural",
                                                                        "Scotland - Remote Rural",
                                                                        "Scotland - Very Remote Rural",
                                                                        "Scotland - Remote Small Town",
                                                                        "Scotland - Accessible Small Town") ~ "Rural",
      Home.area.population.density...urban.or.rural...Instance.0 %in% c("England/Wales - Urban - sparse",
                                                                        "Scotland - Other Urban Area",
                                                                        "England/Wales - Urban - less sparse",
                                                                        "Scotland - Large Urban Area") ~ "Urban",
      Home.area.population.density...urban.or.rural...Instance.0 %in% c("",
                                                                        "Prefer not to answer",
                                                                        "Do not know") ~ NA_character_
    ),
    Population_density_2 = factor(Population_density_2,
                                levels = c("Rural", "Urban"))
  )

testing_data <- testing_data %>%
  mutate(
    Population_density_3 = case_when(
      # Group 1 – Hamlets / Remote
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "England/Wales - Hamlet and Isolated dwelling - sparse",
        "England/Wales - Hamlet and Isolated Dwelling - less sparse",
        "England/Wales - Urban - sparse",
        "Scotland - Accessible Rural",
        "Scotland - Remote Rural",
        "Scotland - Very Remote Rural",
        "Postcode not linkable"
      ) ~ "Hamlets/Remote",
      
      # Group 2 – Villages
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "England/Wales - Village - sparse",
        "England/Wales - Village - less sparse"
      ) ~ "Villages",
      
      # Group 3 – Towns / Fringe
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "England/Wales - Town and Fringe - sparse",
        "England/Wales - Town and Fringe - less sparse",
        "Scotland - Accessible Small Town",
        "Scotland - Remote Small Town",
        "Scotland - Very Remote Small Town"
      ) ~ "Towns/Fringe",
      
      # Group 4 – Major Urban
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "England/Wales - Urban - less sparse",
        "Scotland - Large Urban Area",
        "Scotland - Other Urban Area"
      ) ~ "Major Urban",
      
      # Missing / not stated
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "",
        "Prefer not to answer",
        "Do not know"
      ) ~ NA_character_
    ),
    Population_density_3 = factor(
      Population_density_3,
      levels = c("Hamlets/Remote", "Villages", "Towns/Fringe", "Major Urban")
    )
  )
table(testing_data$Population_density_3)



# POPULATION DENSITY 4
testing_data <- testing_data %>%
  mutate(
    Population_density_4 = case_when(
      # Group 1 – England/Wales Urban
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "England/Wales - Urban - less sparse",
        "England/Wales - Urban - sparse"
      ) ~ "England/Wales – Urban",
      
      # Group 2 – England/Wales Town & Fringe
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "England/Wales - Town and Fringe - less sparse",
        "England/Wales - Town and Fringe - sparse"
      ) ~ "England/Wales – Town & Fringe",
      
      # Group 3 – England/Wales Village
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "England/Wales - Village - less sparse",
        "England/Wales - Village - sparse"
      ) ~ "England/Wales – Village",
      
      # Group 4 – England/Wales Hamlet & Isolated dwelling
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "England/Wales - Hamlet and Isolated Dwelling - less sparse",
        "England/Wales - Hamlet and Isolated dwelling - sparse"
      ) ~ "England/Wales – Hamlet & Isolated dwelling",
      
      # Group 5 – Scotland Urban
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "Scotland - Large Urban Area",
        "Scotland - Other Urban Area"
      ) ~ "Scotland – Urban",
      
      # Group 6 – Scotland Towns + Rural
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "Scotland - Accessible Small Town",
        "Scotland - Remote Small Town",
        "Scotland - Very Remote Small Town",
        "Scotland - Accessible Rural",
        "Scotland - Remote Rural",
        "Scotland - Very Remote Rural"
      ) ~ "Scotland – Towns & Rural",
      
      # Missing / not linkable
      Home.area.population.density...urban.or.rural...Instance.0 %in% c(
        "Postcode not linkable",
        "",
        "Prefer not to answer",
        "Do not know"
      ) ~ NA_character_
    ),
    Population_density_4 = factor(
      Population_density_4,
      levels = c(
        "England/Wales – Hamlet & Isolated dwelling",
        "England/Wales – Village",
        "England/Wales – Town & Fringe",
        "Scotland – Towns & Rural",
        "Scotland – Urban",
        "England/Wales – Urban"
      )
    )
  )
table(testing_data$Population_density_4, useNA = "ifany")




testing_data <- testing_data %>%
  mutate(
    Household_income_group_2 = case_when(
      Household_income_group %in% c("52,000 to 100,000", "Greater than 100,000") ~ "Greater than 52,000",
      TRUE ~ as.character(Household_income_group)
    ),
    Household_income_group_2 = factor(Household_income_group_2, 
                                      levels = c("Less than 18,000","18,000 to 30,999",
                                                 "31,000 to 51,999","Greater than 52,000"))
  )
table(testing_data$Household_income_group_2)


# GENERATE DENTAL CONDITION DIAGNOSES
cols <- c("Date.K02.first.reported..dental.caries.",                                   
          "Date.K03.first.reported..other.diseases.of.hard.tissues.of.teeth.",         
          "Date.K04.first.reported..diseases.of.pulp.and.periapical.tissues.",
          "Date.K05.first.reported..gingivitis.and.periodontal.diseases.",
          "Date.K12.first.reported..stomatitis.and.related.lesions.",                  
          "Date.K13.first.reported..other.diseases.of.lip.and.oral.mucosa.",           
          "Date.K11.first.reported..diseases.of.salivary.glands.",                     
          "Date.K14.first.reported..diseases.of.tongue.")
labels <- c("Dental caries",
            "Other diseases of hard tissues of teeth",
            "Diseases of pulp and periapical tissues",
            "Gingivitis and periodontal diseases",
            "Stomatitis and related lesions",
            "Other diseases of lip and oral mucosa",
            "Diseases of salviary glands",
            "Diseases of tongue")

for (i in seq_along(cols)) {
  var_name <- gsub(" ", "_", labels[i])   # variable name with underscores
  testing_data[[var_name]] <- ifelse(testing_data[[cols[i]]] == "", "NO", "YES")
  label(testing_data[[var_name]]) <- labels[i]  # assign human-readable label
}



# RELABEL CERTAIN VARIABLES
testing_data$Household_income_group <- factor(testing_data$Household_income_group,
                                              levels = c(
                                                "Less than 18,000",
                                                "18,000 to 30,999",
                                                "31,000 to 51,999",
                                                "52,000 to 100,000",
                                                "Greater than 100,000"
                                              ))
testing_data$Population_density <- factor(testing_data$Population_density,
                                              levels = c("Rural", "Urban"))
testing_data$Population_density_2 <- factor(testing_data$Population_density_2,
                                            levels = c("Rural", "Urban"))
testing_data$Alchohol_intake <- factor(testing_data$Alchohol_intake,
                                      levels = c("Low", "Moderate", "High"))
testing_data$Smoking_status <- factor(testing_data$Smoking_status,
                                            levels = c("Never", "Previous", "Current"))

testing_data$Any_all_dementia_diag_made <- factor(testing_data$Any_all_dementia_diag_made,
                                      levels = c("Yes dementia diagnosis made", "No dementia diagnosis made"))
testing_data$Disability_group <- factor(testing_data$Disability_group,
                                                  levels = c("YES", "NO"))
testing_data$Social_activity <- factor(testing_data$Social_activity,
                                         levels = c("YES", "NO"))
testing_data$Pub_or_social_club <- factor(testing_data$Pub_or_social_club,
                                         levels = c("YES", "NO"))
testing_data$Sports_club_or_gym <- factor(testing_data$Sports_club_or_gym,
                                         levels = c("YES", "NO"))
testing_data$Religious_group <- factor(testing_data$Religious_group,
                                         levels = c("YES", "NO"))
testing_data$Other_group_activity <- factor(testing_data$Other_group_activity,
                                         levels = c("YES", "NO"))
testing_data$Mouth_infections_exposure <- factor(testing_data$Mouth_infections_exposure,
                                         levels = c("YES", "NO"))
testing_data$Any_mouth_infections_diag_made <- factor(testing_data$Any_mouth_infections_diag_made,
                                         levels = c("YES", "NO"))
testing_data$Any_PD_diag_made <- factor(testing_data$Any_PD_diag_made,
                                         levels = c("YES", "NO"))
testing_data$Any_alzheimers_diag_made <- factor(testing_data$Any_alzheimers_diag_made,
                                         levels = c("Yes Alzheimer’s diagnosis made", "No Alzheimer’s diagnosis made"))


# GENERATE STUDY DATA
study_data <- testing_data %>%
  select(
    Participant.ID,
    Recruitment_age,
    Recruitment_age_group,
    Sex,
    Ethnic_group_2,
    Ethnic_group_3,
    Highest_education,
    Highest_education_2,
    Household_income_group,
    Household_income_group_2,
    Population_density,
    Population_density_2,
    Population_density_3,
    Population_density_4,
    Log_inverse_nearest_road_distance,
    Assessment_centre,
    Home.area.population.density...urban.or.rural...Instance.0,
    BMI,
    BMI_group,
    Disability_group,
    Social_activity,
    Pub_or_social_club,
    Sports_club_or_gym,
    Religious_group,
    Other_group_activity,
    Days_physical_activity_group_2,
    Alchohol_intake,
    Alchohol_intake_2,
    Alchohol_intake_3,
    Smoking_status,
    Dental_caries,
    Other_diseases_of_hard_tissues_of_teeth,
    Diseases_of_pulp_and_periapical_tissues,
    Gingivitis_and_periodontal_diseases,
    Stomatitis_and_related_lesions,
    Other_diseases_of_lip_and_oral_mucosa,
    Diseases_of_salviary_glands,
    Diseases_of_tongue,
    Mouth_infections_exposure,
    Any_mouth_infections_diag_made,
    Any_PD_diag_made,
    Any_alzheimers_diag_made,
    Any_all_dementia_diag_made,
    Mouth_ulcers_Instance_0,
    Painful_gums_Instance_0,
    Bleeding_gums_Instance_0,
    Loose_teeth_Instance_0,
    Toothache_Instance_0,
    Dentures_Instance_0,
    Any_diabetes_diag_made,
    First_date_all_dementia_diag,
    First_date_mouth_infections_diag
  )


# WRITE STUDY DATA TO FILE
write.csv(study_data, "study_data.csv", row.names = FALSE)
saveRDS(study_data, "study_data.rds")
study_data <- readRDS("study_data.rds")

  
