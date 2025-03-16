# Load packages -----------------------------------------------------------

source(file.path("scripts","install_packages_packman.R"))


date_var <- as.Date(date(), format = "%a %b %d %H:%M:%S %Y")



# Load functions ----------------------------------------------------------

source(file.path("functions","amr_analysis_functions_01.R"))


# Load data sources and preprocess to remove NAs ---------------------------
amr <- readxl::read_excel("test-data/AMR data set.xlsx")


names(amr)[1:32]
amr[,names(amr)[82:96]]


# Specify mandatory columns i.e. columns that should not be NULL/NA
man_cols = c("Specimen date", # data specimen collected
             "Specimen type" , # type of specimemn
             "Organism") # organism identified


# Remove with NA values in any of the mandatory columns
amr <- amr %>% 
  filter(if_all(all_of(man_cols), ~ !is.na(.)))


# Split demographics and results ------------------------------------------


# get results first

pos_drugs <- c(33:81) # drugs
abx_vec <- names(amr)[c(33:81)]
abx_conformed <- as.ab(abx_vec)
pos_vec <- which(names(amr) %in% c("Identification number","Organism","Specimen type"))
amr_res <- amr[,names(amr)[c(pos_vec,pos_drugs)]] 


p1 <- which(names(amr_res) == "Identification number")
p2 <- which(names(amr_res) == "Organism")
p3 <- which(names(amr_res) == "Specimen type")

names(amr_res)[p1] <- "uid"
names(amr_res)[p2] <- "organism"
names(amr_res)[p3] <- "specimen_type"



amr_res <- amr_res %>%
  dplyr::filter(!is.na(organism)) #%>%



# get demographics
pos_demo <- c(1:32)



# Patient Linking Step - OPTIONAL -----------------------------------------








# Data preparation --------------------------------------------------------

amr_res <- amr_res %>%
  mutate(bacteria = as.mo(organism, info = TRUE)) %>%
  mutate(gramstain = mo_gramstain(bacteria)) %>%
  dplyr::select(uid, bacteria, organism, gramstain,everything()) %>%
  mutate(across(all_of(abx_vec), ~as.character(.)))


mo_failures()
mo_uncertainties()


# pivot drugs from wide into long format and map to standard drug codes
famr_long <- amr_res %>% 
  tidyr::pivot_longer(names_to = "drug_code",
                      cols = any_of(abx_vec), 
                      values_to = "vals") %>%
  mutate(drug_code = as.ab(drug_code))



# separate breakpoints from SIR interpretations

# Drugs with SIR interpretations already
famr_long_sir <- famr_long %>% 
  mutate(int_id=row_number()) %>%
  dplyr::filter(str_detect(vals,'R|I|S|SDD|NI')) %>% 
  mutate(test_type=ifelse(grepl('_NM|_EM', drug_code), 'mic','disk'),  
         # Use Gilbert's logic to determine if DISK or MIC
         guideline=ifelse(grepl('_N', drug_code), 'CLSI',
                          ifelse(grepl('_E', drug_code), 'EUCAST', 'CLSI 2022')),
         interpreted_res='',                    #to hold results for the next part
         intrinsic_res_status=''
         )

# Drugs with breakpoints (MIC or DISK)
famr_long_con <- famr_long %>% 
  mutate(int_id=row_number()) %>%
  dplyr::filter(!str_detect(vals,'R|I|S|SDD|NI')) %>%
  # Use Gilbert's logic to determine if DISK or MIC
  mutate(test_type=ifelse(grepl('_NM|_EM', drug_code), 'mic','disk'),  
         guideline=ifelse(grepl('_N', drug_code), 'CLSI',
                          ifelse(grepl('_E', drug_code), 'EUCAST', 'CLSI 2022')),
         interpreted_res='',                    #to hold results for the next part
         intrinsic_res_status=''
         )

# Convert breakpoints to SIR ----------------------------------------------

amr_con <- convert2sir_fun(famr_long_con)
amr_sir <- convert2sir_fun(famr_long_sir)

# combine results
sir_outcomes_df <- amr_con %>%
  dplyr::union(amr_sir)




# Start downstream analysis -----------------------------------------------




#opportunity to retrieve invalid tests
#quality check on the intrinsic res outcome

an_df <- r1_long %>%
  filter(interpreted_res!='NA') %>%
  filter(intrinsic_res_status=='FALSE') %>%
  filter(pathogen!='UNKNOWN') %>%
  select(-agent_code, -int_id,-test_type, -guideline, -res_values, -intrinsic_res_status) %>%
  pivot_wider(names_from = agent, values_from = interpreted_res) %>%
  ##cleanung Age cols
  mutate(Age_s=toupper(gsub("[[:digit:]]", "", Age)),
         Age_n= as.numeric(gsub("[^0-9.-]", "", Age)),
         Age_conv=ifelse(Age_s=='D',round(Age_n/365,0),
                         ifelse(Age_s=='W',round(Age_n/7,0),
                                ifelse(Age_s=='M',round(Age_n/12,0),
                                       ifelse(Age_s=='Y',round(Age_n/1,0),Age)))),
         Age_conv=as.numeric(Age_conv),
         #Age_g=replace_na(as.character(age_groups(Age_conv, split_at = c(1,5,20,50,65))),('N_a')),
         Age_g=as.character(age_groups(Age_conv, split_at = c(1,5,20,50,65))),
         Age_g=ifelse(Age_g=='0', '<1',Age_g),
         Age_g=factor(Age_g, levels=c('<1','1-4','5-19', '20-49','50-64','65+')))


#aNALYSIS BY PATHOGEN


#prepare the variables in the an_df to be used in the subgroup analysis

#E. COLI

amr_grp1_analysis(
org_name='B_ESCHR_COLI',
cntry='TZ',
par='Sex',            #update
par_var_name='Sex',   #update
abs_ref <- c("AMP", "CFR", "CTX", "CAZ", "CIP", "GEN", "TOB", "MEC", "MEM", "NIT", "TZP", "TMP", "SXT")
)







#notes

##processing the date and age columns
#when mixed resuls are availed

