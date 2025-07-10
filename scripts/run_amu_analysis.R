run_amu_analysis <- function() {
  # Load required libraries
  library(readxl)
  library(dplyr)
  library(tidyverse)
  library(writexl)
  library(ggplot2)

  # Create output directory if it doesn't exist
  if (!dir.exists("plots_AMU")) {
    dir.create("plots_AMU")
  }

  # Load and prepare data function
  load_and_prepare_data <- function() {
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

    # Standardize IndicationType values
    amu_dataset <- amu_dataset %>%
      mutate(
        IndicationType = case_when(
          IndicationType == "o" ~ "Other",
          IndicationType == "hia" ~ "HAI",
          IndicationType == "cia" ~ "CAI",
          TRUE ~ IndicationType
        )
      )

    # Create age groups
    amu_dataset <- amu_dataset %>%
      mutate(
        AgeGroup = case_when(
          AgeYears >= 0 & AgeYears < 0.0767 ~ "0-27 days",
          AgeYears >= 0.0767 & AgeYears < 1 ~ "28-364 days",
          AgeYears >= 1 & AgeYears < 5 ~ "1-4 years",
          AgeYears >= 5 & AgeYears < 10 ~ "5-9 years",
          AgeYears >= 10 & AgeYears < 15 ~ "10-14 years",
          AgeYears >= 15 & AgeYears < 20 ~ "15-19 years",
          AgeYears >= 20 & AgeYears < 25 ~ "20-24 years",
          AgeYears >= 25 & AgeYears < 60 ~ "25-59 years",
          AgeYears >= 60 & AgeYears <= 99 ~ "60-99 years",
          AgeYears > 99 ~ "100+ years",
          TRUE ~ NA_character_
        )
      )

    # Create unique patient IDs
    amu_dataset <- amu_dataset %>%
      unite("Patient_UniqueID", PatientCode, region, facility, sep = "_", remove = FALSE)

    # Load and merge AWaRe classification
    aware_path <- "test-data/AMU/ATC-DDD WHO core and optional antimicrobials.xlsx"
    aware_data <- read_excel(aware_path, sheet = "2023 AWaRe classification") %>%
      select(Antibiotic, Class, Category)

    amu_dataset <- amu_dataset %>%
      left_join(aware_data, by = c("AntibioticINNName" = "Antibiotic"))

    return(amu_dataset)
  }

  # Demographic analysis function
  analyze_demographics <- function(amu_dataset) {
    # Age group analysis
    demographics_age <- amu_dataset %>%
      distinct(facility, Patient_UniqueID, AgeGroup) %>%
      count(facility, AgeGroup) %>%
      group_by(facility) %>%
      mutate(Percent = round(n / sum(n) * 100, 2))

    write_xlsx(demographics_age, path = "plots_AMU/demographics_by_agegroup.xlsx")

    # Gender analysis
    demographics_gender <- amu_dataset %>%
      distinct(facility, Patient_UniqueID, Gender) %>%
      count(facility, Gender) %>%
      group_by(facility) %>%
      mutate(Percent = round(n / sum(n) * 100, 2))

    write_xlsx(demographics_gender, path = "plots_AMU/demographics_by_gender.xlsx")

    # Create and save demographic plots
    create_demographic_plots(demographics_age, demographics_gender)
  }

  # Create demographic plots
  create_demographic_plots <- function(demographics_age, demographics_gender) {
    # Age group plot
    avg_percent_by_age <- demographics_age %>%
      group_by(AgeGroup) %>%
      summarise(Avg_agegroup_Percent = mean(Percent, na.rm = TRUE))

    amu_agegroup_plot <- ggplot(avg_percent_by_age, aes(x = AgeGroup, y = Avg_agegroup_Percent)) +
      geom_bar(stat = "identity", fill = "#1E88E5") +
      labs(title = "Percentage of patients by age group", x = "Age Group", y = "Avg % of Patients") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

    ggsave("plots_AMU/AMU_agegroup.png", amu_agegroup_plot, width = 10, height = 6, dpi = 300)
  }

  # AMU prevalence analysis
  analyze_prevalence <- function(amu_dataset) {
    amu_flags <- amu_dataset %>%
      mutate(On_AM = !is.na(Antimicrobial_Name)) %>%
      distinct(facility, Patient_UniqueID, Gender, AgeGroup, On_AM)

    # Overall prevalence
    prevalence_overall <- amu_flags %>%
      group_by(facility) %>%
      summarise(AMU_Prev = round(sum(On_AM) / n() * 100, 2))

    write_xlsx(prevalence_overall, path = "plots_AMU/prevalence_overall_by_facility.xlsx")

    # Create and save prevalence plots
    create_prevalence_plots(amu_flags, prevalence_overall)
  }

  # Create prevalence plots
  create_prevalence_plots <- function(amu_flags, prevalence_overall) {
    # Overall prevalence plot
    amu_plot <- ggplot(prevalence_overall, aes(x = facility, y = AMU_Prev, fill = facility)) +
      geom_bar(stat = "identity") +
      labs(title = "Antimicrobial Use Prevalence by Site", x = "Site", y = "AMU Prevalence (%)") +
      theme_classic() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

    ggsave("plots_AMU/AMU_prevalence_by_site.png", amu_plot, width = 10, height = 6, dpi = 300)

    # Prevalence by ward
    prevalence_by_ward <- amu_dataset %>%
      distinct(facility, Patient_UniqueID, WardName, Antimicrobial_Name) %>%
      mutate(On_AM = !is.na(Antimicrobial_Name)) %>%
      group_by(facility, WardName) %>%
      summarise(AMU_Prev = round(sum(On_AM) / n() * 100, 2))

    avg_percent_by_ward <- prevalence_by_ward %>%
      group_by(WardName) %>%
      summarise(Avg_Ward_Percent = mean(AMU_Prev, na.rm = TRUE))

    amu_prev_ward_plot <- ggplot(avg_percent_by_ward,
                                 aes(x = reorder(WardName, Avg_Ward_Percent), y = Avg_Ward_Percent)) +
      geom_bar(stat = "identity", fill = "#1E88E5") +
      labs(title = "AMU Prevalence by Hospital Ward", x = "Hospital Ward", y = "AMU Prevalence (%)") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

    ggsave("plots_AMU/AMU_prevalence_by_Ward.png", amu_prev_ward_plot, width = 10, height = 6, dpi = 300)
  }

  # Antimicrobial utilization analysis
  analyze_utilization <- function(amu_dataset) {
    # Average antimicrobials per patient
    avg_antimicrobials <- amu_dataset %>%
      filter(!is.na(Antimicrobial_Name)) %>%
      group_by(facility) %>%
      summarise(
        Total_AMs = n(),
        Total_Patients_on_AMs = n_distinct(Patient_UniqueID),
        Average_AMs = round(Total_AMs / Total_Patients_on_AMs, 2)
      )

    write_xlsx(avg_antimicrobials, path = "plots_AMU/AMU_average_antimicrobials.xlsx")

    # Create and save utilization plots
    create_utilization_plots(amu_dataset)
  }

  # Create utilization plots
  create_utilization_plots <- function(amu_dataset) {
    # AMU by ATC Class
    amu_by_class <- amu_dataset %>%
      filter(!is.na(Class)) %>%
      group_by(facility, Class) %>%
      summarise(n = n()) %>%
      group_by(facility) %>%
      mutate(Percent = round(n / sum(n) * 100, 2))

    write_xlsx(amu_by_class, path = "plots_AMU/AMU_by_class.xlsx")

    avg_percent_by_class <- amu_by_class %>%
      group_by(Class) %>%
      summarise(Avg_Percent = mean(Percent, na.rm = TRUE))

    amu_class_plot <- ggplot(avg_percent_by_class,
                             aes(x = reorder(Class, Avg_Percent), y = Avg_Percent)) +
      geom_bar(stat = "identity", fill = "#1E88E5") +
      labs(title = "AMU by ATC classification", x = "ATC class", y = "% of total AMU") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

    ggsave("plots_AMU/AMU_Class.png", amu_class_plot, width = 10, height = 6, dpi = 300)

    # AMU by AWaRe Category
    amu_by_category <- amu_dataset %>%
      filter(!is.na(Category)) %>%
      group_by(facility, Category) %>%
      summarise(n = n()) %>%
      group_by(facility) %>%
      mutate(Percent = round(n / sum(n) * 100, 2))

    write_xlsx(amu_by_category, path = "plots_AMU/AMU_by_category.xlsx")

    avg_percent_by_category <- amu_by_category %>%
      group_by(Category) %>%
      summarise(Avg_category_Percent = mean(Percent, na.rm = TRUE))

    amu_category_plot <- ggplot(avg_percent_by_category,
                                aes(x = reorder(Category, Avg_category_Percent), y = Avg_category_Percent)) +
      geom_bar(stat = "identity", fill = "#1E88E5") +
      labs(title = "AMU by AWaRe Categorization", x = "AWaRe Category", y = "% of total AMU") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

    ggsave("plots_AMU/AMU_Category.png", amu_category_plot, width = 10, height = 6, dpi = 300)

    # AMU by Route of Administration
    amu_by_route <- amu_dataset %>%
      filter(!is.na(AdministrationRoute)) %>%
      group_by(facility, AdministrationRoute) %>%
      summarise(n = n()) %>%
      group_by(facility) %>%
      mutate(Percent = round(n / sum(n) * 100, 2))

    write_xlsx(amu_by_route, path = "plots_AMU/AMU_by_adminroute.xlsx")

    avg_percent_by_route <- amu_by_route %>%
      group_by(AdministrationRoute) %>%
      summarise(Avg_route_Percent = mean(Percent, na.rm = TRUE))

    amu_route_plot <- ggplot(avg_percent_by_route,
                             aes(x = reorder(AdministrationRoute, Avg_route_Percent), y = Avg_route_Percent)) +
      geom_bar(stat = "identity", fill = "#1E88E5") +
      labs(title = "AMU by Route of Administration", x = "Route", y = "% of total AMU") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

    ggsave("plots_AMU/AMU_route.png", amu_route_plot, width = 10, height = 6, dpi = 300)
  }

  # Main execution
  message("Starting AMU analysis...")

  # 1. Load and prepare data
  message("Loading and preparing data...")
  amu_dataset <- load_and_prepare_data()

  # 2. Analyze demographics
  message("Analyzing demographic data...")
  analyze_demographics(amu_dataset)

  # 3. Analyze AMU prevalence
  message("Analyzing AMU prevalence...")
  analyze_prevalence(amu_dataset)

  # 4. Analyze antimicrobial utilization
  message("Analyzing antimicrobial utilization patterns...")
  analyze_utilization(amu_dataset)

  message("AMU analysis complete! Results saved in the 'plots_AMU' folder.")
}
