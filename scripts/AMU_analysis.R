# Load required libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)
library(ggplot2)

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
      AgeYears > 99                         ~ "100+ years",
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

############### Demographics - % of surveyed patients by AgeGroup ######################
########################################################################################
demographics_age <- amu_dataset %>%
  distinct(facility, Patient_UniqueID, AgeGroup) %>%
  count(facility, AgeGroup) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(demographics_age, path = "plots_AMU/demographics_by_agegroup.xlsx") # Save to Excel

# Calculate average percentage of patients by age group across all facilities
avg_percent_by_age <- demographics_age %>%
  group_by(AgeGroup) %>%
  summarise(Avg_agegroup_Percent = mean(Percent, na.rm = TRUE))

# Bar chart of average percentage of patients by age group across all facilities
amu_agegroup_plot <- ggplot(avg_percent_by_age, aes(x = AgeGroup, y = Avg_agegroup_Percent)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "Percentage of patients by age group across all facilities",
    x = "AgeGroup",
    y = "Avg % of Patients"
  ) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = "plots_AMU/AMU_agegroup.png", plot = amu_agegroup_plot, width = 10, height = 6, dpi = 300)

#################### Demographics - % of surveyed patients by Gender #####################
##########################################################################################
demographics_gender <- amu_dataset %>%
  distinct(facility, Patient_UniqueID, Gender) %>%
  count(facility, Gender) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(demographics_gender, path = "plots_AMU/demographics_by_gender.xlsx") # Save to Excel

# Calculate average percentage of patients by gender across all facilities
avg_percent_by_gender <- demographics_gender %>%
  group_by(Gender) %>%
  summarise(Avg_gender_Percent = mean(Percent, na.rm = TRUE))

################## AMU Prevalence - overall, by age group, by gender #######################
############################################################################################
amu_flags <- amu_dataset %>%
  mutate(On_AM = !is.na(Antimicrobial_Name)) %>%
  distinct(facility, Patient_UniqueID, Gender, AgeGroup, On_AM)

###################################### overall prevalence
prevalence_overall <- amu_flags %>%
  group_by(facility) %>%
  summarise(AMU_Prev = round(sum(On_AM) / n() * 100, 2))

write_xlsx(prevalence_overall, path = "plots_AMU/prevalence_overall_by_facility.xlsx") # Save to Excel

# Bar chart of AMU prevalence by site
amu_plot <- ggplot(prevalence_overall, aes(x = facility, y = AMU_Prev, fill = facility)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Antimicrobial Use Prevalence by Site",
    x = "Site",
    y = "AMU Prevalence (%)"
  ) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical alignment)

# Save the plot to the plots_AMU folder
ggsave(filename = "plots_AMU/AMU_prevalence_by_site.png", plot = amu_plot, width = 10, height = 6, dpi = 300)

########################## Prevalence by age
prevalence_by_age <- amu_flags %>%
  group_by(facility, AgeGroup) %>%
  summarise(AMU_Prev = round(sum(On_AM) / n() * 100, 2))

write_xlsx(prevalence_by_age, path = "plots_AMU/prevalence__by_age.xlsx") # Save to Excel

# Calculate average percentage prevalence by age group across all facilities
avg_percent_prev_age <- prevalence_by_age %>%
  group_by(AgeGroup) %>%
  summarise(Avg_agegroup_Prev = mean(AMU_Prev, na.rm = TRUE))

# Bar chart of AMU prevalence by age
amu_prev_agegroup_plot <- ggplot(avg_percent_prev_age, aes(x = AgeGroup, y = Avg_agegroup_Prev)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "Prevalence by age group across all facilities",
    x = "AgeGroup",
    y = "Avg Prevalence (%)"
  ) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = "plots_AMU/AMU_agegroup_prev.png", plot = amu_prev_agegroup_plot, width = 10, height = 6, dpi = 300)

####################### prevalence by gender
prevalence_by_gender <- amu_flags %>%
  group_by(facility, Gender) %>%
  summarise(AMU_Prev = round(sum(On_AM) / n() * 100, 2))

write_xlsx(prevalence_by_gender, path = "plots_AMU/prevalence_by_gender.xlsx") # Save to Excel

##################### AMU Prevalence by Ward #########################################
######################################################################################
prevalence_by_ward <- amu_dataset %>%
  distinct(facility, Patient_UniqueID, WardName, Antimicrobial_Name) %>%
  mutate(On_AM = !is.na(Antimicrobial_Name)) %>%
  group_by(facility, WardName) %>%
  summarise(AMU_Prev = round(sum(On_AM) / n() * 100, 2))

# Calculate average percentage by ward across all facilities
avg_percent_by_ward <- prevalence_by_ward %>%
  group_by(WardName) %>%
  summarise(Avg_Ward_Percent = mean(AMU_Prev, na.rm = TRUE))

# Bar chart of AMU prevalence by Ward
amu_prev_ward_plot <- ggplot(avg_percent_by_ward, aes(x = reorder(WardName, Avg_Ward_Percent), y = Avg_Ward_Percent)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "AMU Prevalence by Hospital Ward",
    x = "Hospital Ward",
    y = "AMU Prevalence (%)"
  ) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = "plots_AMU/AMU_prevalence_by_Ward.png", plot = amu_prev_ward_plot, width = 10, height = 6, dpi = 300)

################### Avg number of antimicrobials per patient ####################
#################################################################################
avg_antimicrobials <- amu_dataset %>%
  filter(!is.na(Antimicrobial_Name)) %>%
  group_by(facility) %>%
  summarise(
    Total_AMs = n(),
    Total_Patients_on_AMs = n_distinct(Patient_UniqueID),
    Average_AMs = round(Total_AMs / Total_Patients_on_AMs, 2)
  )

write_xlsx(avg_antimicrobials, path = "plots_AMU/AMU_average_antimicrobialsr.xlsx") # Save to Excel

###################### AMU by ATC Class #########################################
#################################################################################
amu_by_class <- amu_dataset %>%
  filter(!is.na(Class)) %>%
  group_by(facility, Class) %>%
  summarise(n = n()) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(amu_by_class, path = "plots_AMU/AMU_by_class.xlsx") # Save to Excel

# Calculate average percentage by class across all facilities
avg_percent_by_class <- amu_by_class %>%
  group_by(Class) %>%
  summarise(Avg_Percent = mean(Percent, na.rm = TRUE))

# Bar chart of AMU by class
amu_class_plot <- ggplot(avg_percent_by_class, aes(x = reorder(Class, Avg_Percent), y = Avg_Percent)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "AMU by ATC classification",
    x = "ATC class",
    y = "% of total AMU"
  ) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = "plots_AMU/AMU_Class.png", plot = amu_class_plot, width = 10, height = 6, dpi = 300)

##################### AMU by Antimicrobial Molecule #############################
#################################################################################
amu_by_molecule <- amu_dataset %>%
  filter(!is.na(Antimicrobial_Name)) %>%
  group_by(facility, Antimicrobial_Name) %>%
  summarise(n = n()) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(amu_by_molecule, path = "plots_AMU/AMU_by_molecule.xlsx") # Save to Excel

# Calculate average percentage by Molecule across all facilities
avg_percent_by_molecule <- amu_by_molecule %>%
  group_by(Antimicrobial_Name) %>%
  summarise(Avg_molecule_Percent = mean(Percent, na.rm = TRUE))

###################### AMU by AWaRe Category ###################################
################################################################################
amu_by_category <- amu_dataset %>%
  filter(!is.na(Category)) %>%
  group_by(facility, Category) %>%
  summarise(n = n()) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(amu_by_category, path = "plots_AMU/AMU_by_category.xlsx") # Save to Excel

# Calculate average percentage by AWaRe Category across all facilities
avg_percent_by_category <- amu_by_category %>%
  group_by(Category) %>%
  summarise(Avg_category_Percent = mean(Percent, na.rm = TRUE))

# Bar chart of AMU by AWaRe Category
amu_category_plot <- ggplot(avg_percent_by_category, aes(x = reorder(Category, Avg_category_Percent), y = Avg_category_Percent)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "AMU by AWaRe Categorization",
    x = "AWaRe Category",
    y = "% of total AMU"
  ) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = "plots_AMU/AMU_Category.png", plot = amu_category_plot, width = 10, height = 6, dpi = 300)

######################## AMU by Route of Administration ##########################
##################################################################################
amu_by_route <- amu_dataset %>%
  filter(!is.na(AdministrationRoute)) %>%
  group_by(facility, AdministrationRoute) %>%
  summarise(n = n()) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(amu_by_route, path = "plots_AMU/AMU_by_adminroute.xlsx") # Save to Excel

# Calculate average percentage by route of Administration across all facilities
avg_percent_by_route <- amu_by_route %>%
  group_by(AdministrationRoute) %>%
  summarise(Avg_route_Percent = mean(Percent, na.rm = TRUE))

# Bar chart of AMU by Route of Administration
amu_route_plot <- ggplot(avg_percent_by_route, aes(x = reorder(AdministrationRoute, Avg_route_Percent), y = Avg_route_Percent)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "AMU by Route of Administration",
    x = "Route of Administration",
    y = "% of total AMU"
  ) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = "plots_AMU/AMU_route.png", plot = amu_route_plot, width = 10, height = 6, dpi = 300)

