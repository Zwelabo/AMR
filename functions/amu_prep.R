#AMU prep
#antibiotic inhibitors list
inhibitors <- c(
  "Clavulanic acid",
  "Sulbactam",
  "Tazobactam",
  "Avibactam",
  "Vaborbactam",
  "Relebactam",
  "Zidebactam",
  "Nacubactam",
  "PAβN",
  "D13-9001",
  "EDTA",
  'cilastatin',
  'betamipron',
  'Salbactam'
)

other_non_antibiotics <- c('diloxanide', 'praziquantel', 'antimalarial',
                           'ors', 'i.v', 'tabs', 'cap', 'syrup', 'acid', 'other'
                           )  #when the predicted name is not correct, the xab_name should be added in this list to ease analysis going forward



#atc molecules
atcs_cleaned <- read_excel('test-data/ab_molecules.xlsx') %>%
  mutate(original_entry=tolower(trimws(original_entry))) %>%
  distinct(original_entry, .keep_all = T)


##
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
      !is.na(Other_Antibiotic) &
        tolower(trimws(AntibioticINNName)) == "other",
      Other_Antibiotic,
      AntibioticINNName
    ),
    antibiotic_copy=gsub('[-+_/,&]','_s_',tolower(Antimicrobial_Name)),
    antibiotic_copy=gsub('ampiclox','ampicillin and cloxacillin and',tolower(antibiotic_copy)),  #may also need to do this for amoxiclav
    antibiotic_copy=gsub('and ','_s_',tolower(antibiotic_copy)),
    uid=1:nrow(.),                                    #adding an identifer to sort out the mixed molecules
    # Update IndicationType values
    IndicationType = case_when(
      IndicationType == "o" ~ "Other",
      IndicationType == "hia" ~ "HAI",
      IndicationType == "cia" ~ "CAI",
      TRUE ~ IndicationType),  # keep all other values as they are
    #Age
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
      AgeYears > 99   ~ "100+ years",
      TRUE ~ NA_character_
    ),
    # Create Patient_UniqueID by concatenating PatientCode, Region, and Facility
    Patient_UniqueID= paste(PatientCode, region, facility, sep = "_")
  )


#analysis scheme.
#how many antibiotics are in both datasets?
amu_dataset_match1 <- subset(amu_dataset, tolower(amu_dataset$Antimicrobial_Name) %in% atcs_cleaned$original_entry)


# Step 1: Determine the max number of parts after splitting
max_parts <- max(str_count(amu_dataset$antibiotic_copy, "_s_"), na.rm = T) + 1

# Step 2: Generate dynamic column names
col_names <- paste0("name", seq_len(max_parts))


amu_dataset_match_prep <- subset(amu_dataset, !amu_dataset$uid %in% amu_dataset_match1$uid) %>%
  dplyr::select(uid,Antimicrobial_Name, antibiotic_copy) %>%
  separate(antibiotic_copy, into = col_names, sep = "_s_", fill = "right") %>%
  pivot_longer(cols = starts_with('name'), values_to = 'xab_name') %>%
  drop_na()

amu_dataset_match2 <- subset(amu_dataset_match_prep, trimws(amu_dataset_match_prep$xab_name) %in% atcs_cleaned$original_entry)


amu_dataset_match_prep2 <- subset(amu_dataset_match_prep, !amu_dataset_match_prep$uid %in% amu_dataset_match2$uid) %>%
  dplyr::select(uid,Antimicrobial_Name, xab_name) %>%
  separate(xab_name,
           into =  paste0("name", seq_len(max(str_count(.$xab_name, " "), na.rm = T) + 1)),
           sep = " ", fill = "right") %>%
  pivot_longer(cols = starts_with('name'), values_to = 'xab_name') %>%
  drop_na()%>%
  filter(!xab_name %in% c('other', 'oral', 'acid')) #%>%     #update with new datasets
#
amu_dataset_match3 <- subset(amu_dataset_match_prep2, trimws(amu_dataset_match_prep2$xab_name) %in% atcs_cleaned$original_entry)


##getting names from the AMR package
amu_dataset_prep3 <- subset(amu_dataset_match_prep2, !amu_dataset_match_prep2$uid %in% amu_dataset_match3$uid) %>%
  filter(!(xab_name %in% tolower(inhibitors)) & nchar(xab_name)>2 ) %>%   #remove inhibitors and known antibiotics before ab_name step
  filter(!(xab_name %in% tolower(other_non_antibiotics)) ) %>%
  mutate( cleaned_ab_name=ab_name(xab_name),
          match_perc=100 * stringsim(xab_name, cleaned_ab_name)) %>%   #similarity threshold of >35% is applied for the generated ab names
  filter(match_perc>35)


#saving the looked up dataset
write_xlsx(amu_dataset_prep3 %>% dplyr::select(Antimicrobial_Name, reported_name=xab_name, cleaned_name=cleaned_ab_name),
           paste0(amu_dir,'/corrected entries_to be checked.xlsx'))

amu_dataset_match4<-amu_dataset_prep3%>%
  dplyr::select(uid,Antimicrobial_Name, cleaned_ab_name) %>%
  separate(cleaned_ab_name,
           into =  paste0("name", seq_len(max(str_count(.$cleaned_ab_name, "/"), na.rm = T) + 1)),
           sep = "/", fill = "right") %>%
  pivot_longer(cols = starts_with('name'), values_to = 'xab_name') %>%
  drop_na() %>%
  filter(tolower(xab_name) %in% atcs_cleaned$original_entry) %>%
  mutate(corrected='yes')  ##to work with only the reported data, this label can be used to filter



#re_attach the antibiotics back to the main dataset
#dataset now longer since it's on antibiotic molecule i.e., ampiclox gets 2 rows
working_df_molecule <- Reduce(bind_rows,
                     list(amu_dataset_match1 %>%
                            dplyr::select(uid,Antimicrobial_Name, xab_name=antibiotic_copy), amu_dataset_match2,
                          amu_dataset_match3, amu_dataset_match4)) %>%
  mutate(type='antibiotic',
         xab_name=tolower(trimws(xab_name))) %>%
  right_join(amu_dataset, by=c('uid', 'Antimicrobial_Name')) %>%
  left_join(atcs_cleaned, by=c('xab_name'='original_entry'))

#working_df_demographics <- working_df_molecule %>% distinct(uid, .keep_all = T)

##I will move on to the classes cleaning
#We might make an ML model for this classification
