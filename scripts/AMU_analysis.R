# Load required libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)
library(ggplot2)
library(stringdist)


#country
cntry='UG'   #input code

#create the results directory
amu_dir <- file.path("Results_AMU",cntry)

if(!dir.exists(amu_dir)){dir.create(amu_dir, recursive = T)}



# Load the AMU dataset
amu_path <- "test-data/AMU/Merged_Point Prevalence Survey_testing.xlsx"
amu_raw <- read_excel(amu_path, sheet = "Merged")

##run the AMU prep
source('functions/amu_prep.R')

amu_dataset <- working_df_molecule %>%
  distinct(uid, .keep_all = T) %>%
  arrange(AgeYears) %>%
  mutate(AgeGroup=factor(AgeGroup, levels=unique(AgeGroup)))  #sorting the x axis

############### Demographics - % of surveyed patients by AgeGroup ######################
########################################################################################
demographics_age <- amu_dataset %>%
  distinct(facility, Patient_UniqueID, AgeGroup) %>%
  count(facility, AgeGroup) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2),
         n_tot=sum(n))

write_xlsx(demographics_age, path = paste0(amu_dir,"/demographics_by_agegroup.xlsx") )# Save to Excel

# Calculate average percentage of patients by age group across all facilities
avg_percent_by_age <- demographics_age %>%
  group_by(AgeGroup) %>%
  summarise(n=sum(n)) %>%
  mutate(Avg_agegroup_Percent = round(n / sum(n) * 100, 2))

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
ggsave(filename = paste0(amu_dir,"/AMU_agegroup.png"), plot = amu_agegroup_plot, width = 10, height = 6, dpi = 300)

#################### Demographics - % of surveyed patients by Gender #####################
##########################################################################################
demographics_gender <- amu_dataset %>%
  distinct(facility, Patient_UniqueID, Gender) %>%
  count(facility, Gender) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(demographics_gender, path = paste0(amu_dir,"/demographics_by_gender.xlsx")) # Save to Excel

# Calculate average percentage of patients by gender across all facilities
avg_percent_by_gender <- demographics_gender %>%
  group_by(Gender) %>%
summarise(n=sum(n)) %>%
  mutate(Avg_gender_Percent = round(n / sum(n) * 100, 2))

#add plot

################## AMU Prevalence - overall, by age group, by gender #######################
############################################################################################
amu_flags <- amu_dataset %>%
  #mutate(On_AM = !is.na(Antimicrobial_Name)) %>%   #gilbert, no, some antimalarils are specified
  mutate(On_AM = ifelse(grepl('antibiotic', type),1,0)) %>%
  distinct(facility, Patient_UniqueID, .keep_all = T)

###################################### overall prevalence
prevalence_overall <- amu_flags %>%
  group_by(facility) %>%
  summarise(AMU_Prev = round(mean(On_AM) * 100, 2),
            n=n())

write_xlsx(prevalence_overall, path = paste0(amu_dir,"/prevalence_overall_by_facility.xlsx")) # Save to Excel

# Bar chart of AMU prevalence by site
amu_plot <- ggplot(prevalence_overall, aes(x = reorder(facility,-AMU_Prev), y = AMU_Prev)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Antimicrobial Use Prevalence by Site",
    x = "Site",
    y = "AMU Prevalence (%)"
  ) +
  theme_classic() +
  coord_flip()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))  # Vertical alignment)

# Save the plot to the plots_AMU folder
ggsave(filename = paste0(amu_dir,"/AMU_prevalence_by_site.png"), plot = amu_plot, width = 10, height = 11, dpi = 300)

########################## Prevalence by age
prevalence_by_age <- amu_flags %>%
  group_by(facility, AgeGroup) %>%
  summarise(AMU_Prev = round(mean(On_AM) * 100, 2))

write_xlsx(prevalence_by_age, path = paste0(amu_dir,"/prevalence__by_age.xlsx")) # Save to Excel

# Calculate average percentage prevalence by age group across all facilities
avg_percent_prev_age <- amu_flags %>%
  group_by(AgeGroup) %>%
  summarise(Avg_agegroup_Prev = round(mean(On_AM) * 100, 2))

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
ggsave(filename = paste0(amu_dir,"/AMU_agegroup_prev.png"), plot = amu_prev_agegroup_plot, width = 10, height = 6, dpi = 300)

####################### prevalence by gender
prevalence_by_gender <- amu_flags %>%
  group_by(facility, Gender) %>%
  summarise(AMU_Prev = round(mean(On_AM) * 100, 2))

write_xlsx(prevalence_by_gender, path = paste0(amu_dir,"/prevalence_by_gender.xlsx")) # Save to Excel

##################### AMU Prevalence by Ward #########################################
######################################################################################
prevalence_by_ward <- amu_flags %>%
 # distinct(facility, Patient_UniqueID, WardName, Antimicrobial_Name) %>%
  #mutate(On_AM = !is.na(Antimicrobial_Name)) %>%
  group_by(facility, WardName) %>%
  summarise(AMU_Prev = round(mean(On_AM) * 100, 2))

# Calculate average percentage by ward across all facilities
avg_percent_by_ward <- amu_flags %>%
  group_by(WardName) %>%
  summarise(Avg_Ward_Percent = round(mean(On_AM) * 100, 2),
            n=n())

# Bar chart of AMU prevalence by Ward
amu_prev_ward_plot <- ggplot(avg_percent_by_ward, aes(x = reorder(WardName, Avg_Ward_Percent), y = Avg_Ward_Percent)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "AMU Prevalence by Hospital Ward",
    x = "Hospital Ward",
    y = "AMU Prevalence (%)"
  ) +
  theme_classic() +
  coord_flip()+
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 0, vjust = 0, hjust = 1))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = paste0(amu_dir,"/AMU_prevalence_by_Ward.png"), plot = amu_prev_ward_plot, width = 10, height = 6, dpi = 300)

################### Avg number of antimicrobials per patient ####################
#################################################################################
avg_antimicrobials <- amu_dataset %>%
  filter(type=='antibiotic') %>%         ################start here
  group_by(facility) %>%
  summarise(
    Total_AMs = n(),
    Total_Patients_on_AMs = n_distinct(Patient_UniqueID),
    Average_AMs = round(Total_AMs / Total_Patients_on_AMs, 2)
  )

write_xlsx(avg_antimicrobials, path = paste0(amu_dir,"/AMU_average_antimicrobialsr.xlsx")) # Save to Excel

###################### AMU by ATC Class #########################################To continue working on this section-Gilbert
#################################################################################
# amu_by_class <- amu_dataset %>%
#   filter(!is.na(Class)) %>%
#   group_by(facility, Class) %>%
#   summarise(n = n()) %>%
#   group_by(facility) %>%
#   mutate(Percent = round(n / sum(n) * 100, 2))
#
# write_xlsx(amu_by_class, path = "plots_AMU/AMU_by_class.xlsx") # Save to Excel
#
# # Calculate average percentage by class across all facilities
# avg_percent_by_class <- amu_by_class %>%
#   group_by(Class) %>%
#   summarise(Avg_Percent = mean(Percent, na.rm = TRUE))
#
# # Bar chart of AMU by class
# amu_class_plot <- ggplot(avg_percent_by_class, aes(x = reorder(Class, Avg_Percent), y = Avg_Percent)) +
#   geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
#   labs(
#     title = "AMU by ATC classification",
#     x = "ATC class",
#     y = "% of total AMU"
#   ) +
#   theme_classic() +
#   theme(legend.position = 'none',
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Vertical alignment
#
# # Save the plot to the plots_AMU folder
# ggsave(filename = "plots_AMU/AMU_Class.png", plot = amu_class_plot, width = 10, height = 6, dpi = 300)

##################### AMU by Antimicrobial Molecule #############################
#################################################################################
amu_by_molecule <- working_df_molecule %>%
  filter(type=='antibiotic') %>%
  group_by(facility, xab_name) %>%
  summarise(n = n()) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(amu_by_molecule, path = paste0(amu_dir,"/AMU_by_molecule.xlsx")) # Save to Excel

# Calculate average percentage by Molecule across all facilities
avg_percent_by_molecule <- amu_by_molecule %>%ungroup() %>%
  group_by(xab_name) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  mutate(avg_percent_mol=round((n/sum(n))*100,2))

#add plot


###################### AMU by AWaRe Category ###################################
################################################################################
amu_by_category <- amu_dataset %>%
  filter(!is.na(aware_cat)) %>%
  group_by(facility, aware_cat) %>%
  summarise(n = n()) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(amu_by_category, path = paste0(amu_dir,"/AMU_by_category.xlsx")) # Save to Excel

# Calculate average percentage by AWaRe Category across all facilities
avg_percent_by_category <- amu_by_category %>%
  group_by(aware_cat) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  mutate(Avg_category_Percent=round(n*100/sum(n),2))

# Bar chart of AMU by AWaRe Category
amu_category_plot <- ggplot(avg_percent_by_category, aes(x = reorder(aware_cat, Avg_category_Percent), y = Avg_category_Percent)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "AMU by AWaRe Categorization",
    x = "AWaRe Category",
    y = "% of total AMU"
  ) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 0))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = paste0(amu_dir,"/AMU_aware_Category.png"), plot = amu_category_plot, width = 10, height = 6, dpi = 300)

######################## AMU by Route of Administration ##########################
##################################################################################
amu_by_route <- amu_dataset %>%
  filter(!is.na(AdministrationRoute) & type=='antibiotic') %>%
  group_by(facility, AdministrationRoute) %>%
  summarise(n = n()) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))

write_xlsx(amu_by_route, path = paste0(amu_dir,"/AMU_by_adminroute.xlsx")) # Save to Excel

# Calculate average percentage by route of Administration across all facilities
avg_percent_by_route <- amu_by_route %>%
  group_by(AdministrationRoute) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  mutate(Avg_route_Percent=round(n*100/sum(n),2))

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
        axis.text.x = element_text(angle = 0))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename =  paste0(amu_dir,"/AMU_route.png", plot = amu_route_plot, width = 10, height = 6, dpi = 300))

################################# AMU by Indication Type #################################
##########################################################################################
amu_by_indication <- amu_dataset %>%
  filter(!is.na(IndicationType) & type=='antibiotic') %>%
  group_by(facility, IndicationType) %>%
  summarise(n = n()) %>%
  group_by(facility) %>%
  mutate(Percent = round(n / sum(n) * 100, 2))     ##what aare mp and sp

# Calculate average percentage by indication type across all facilities
avg_percent_by_indication <- amu_by_indication %>%
  group_by(IndicationType) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  mutate(Avg_indication_Percent=round(n*100/sum(n),2))

# Bar chart of AMU by Indication Type
amu_indication_plot <- ggplot(avg_percent_by_indication, aes(x = reorder(IndicationType, Avg_indication_Percent), y = Avg_indication_Percent)) +
  geom_bar(stat = "identity", fill = "#1E88E5") +  # Moved fill outside aes()
  labs(
    title = "AMU by Indication Type",
    x = "Indication Type",
    y = "% of total AMU"
  ) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 0))  # Vertical alignment

# Save the plot to the plots_AMU folder
ggsave(filename = paste0(amu_dir,"/AMU_indication.png"), plot = amu_indication_plot, width = 10, height = 6, dpi = 300)
