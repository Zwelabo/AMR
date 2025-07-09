# Load required libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)

# Load the AMU dataset
amu_path <- "test-data/AMU/Merged_Point Prevalence Survey_testing.xlsx"
amu_raw <- read_excel(amu_path, sheet = "Merged")

# Create the filtered AMU dataset
amu_dataset <- amu_raw %>%
  select(
    region,
    facility,
    level_of_care,
    PatientCode = `Patientform-CORE_Patientdemographics-PatientCode`,
    WardName = `Patientform-CORE_Patientdemographics-WardName`,
    AntibioticINNName,
    Other_Antibiotic,
    AdministrationRoute,
    IndicationType,
    Diagnosis,
    Gender = `Patientform-CORE_Patientdemographics-Gender`,
    AgeYears = `Patientform-CORE_Patientdemographics-age_years`
  ) %>%
  mutate(
    Antimicrobial_Name = if_else(
      tolower(trimws(AntibioticINNName)) == "other" & !is.na(Other_Antibiotic),
      Other_Antibiotic,
      AntibioticINNName
    )
  )

# Update IndicationType values
amu_dataset <- amu_dataset %>%
  mutate(
    IndicationType = case_when(
      IndicationType == "o" ~ "Other",
      IndicationType == "hia" ~ "HAI",
      IndicationType == "cia" ~ "CAI",
      TRUE ~ IndicationType  # keep all other values as they are
    )
  )

# Create age group variable
amu_dataset <- amu_dataset %>%
  mutate(
    AgeGroup = case_when(
      AgeYears >= 0       & AgeYears < 0.0767  ~ "0-27 days",        # ~27 days = 27/365
      AgeYears >= 0.0767  & AgeYears < 1       ~ "28-364 days",
      AgeYears >= 1       & AgeYears < 5       ~ "1-4 years",
      AgeYears >= 5       & AgeYears < 10      ~ "5-9 years",
      AgeYears >= 10      & AgeYears < 15      ~ "10-14 years",
      AgeYears >= 15      & AgeYears < 20      ~ "15 – 19 years",
      AgeYears >= 20      & AgeYears < 25      ~ "20-24 years",
      AgeYears >= 25      & AgeYears < 60      ~ "25-59 years",
      AgeYears >= 60      & AgeYears <= 99     ~ "60-99 years",
      AgeYears > 99                         ~ ">100 years",
      TRUE ~ NA_character_
    )
  )

# Create Patient_UniqueID by concatenating PatientCode, Region, and Facility
amu_dataset <- amu_dataset %>%
  unite("Patient_UniqueID", PatientCode, region, facility, sep = "_", remove = FALSE)

# Load AWaRe classification sheet
aware_path <- "test-data/AMU/ATC-DDD WHO core and optional antimicrobials.xlsx"
aware_data <- read_excel(aware_path, sheet = "2023 AWaRe classification") %>%
  select(Antibiotic, Class, Category)

# Join AMU dataset with AWaRe data using AntibioticINNName
amu_dataset <- amu_dataset %>%
  left_join(aware_data, by = c("AntibioticINNName" = "Antibiotic"))


############## Creating tables based on formulas for amu
##########################################################

# Demographics - % of surveyed patients by AgeGroup
demographics_age <- amu_dataset %>%
  distinct(facility, Patient_UniqueID, AgeGroup) %>%
  count(facility, AgeGroup) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

# Save to Excel
write_xlsx(demographics_age, path = "plots_AMU/demographics_by_agegroup.xlsx")

# Demographics - % of surveyed patients by Gender
demographics_gender <- amu_dataset %>%
  distinct(facility, Patient_UniqueID, Gender) %>%
  count(facility, Gender) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

# Save to Excel
write_xlsx(demographics_gender, path = "plots_AMU/demographics_by_gender.xlsx")
