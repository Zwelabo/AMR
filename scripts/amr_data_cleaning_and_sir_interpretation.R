
# Introduction ------------------------------------------------------------

# These scripts have been developed to simplify and standardize AMR/C/U data analysis
# through ASLM as part of the MAAP Phase II project funded through Fleming Fund.

# These scripts are largely based on the free and open-source AMR package implemented in R.



# Set-up and software installation ----------------------------------------

# First, you will need to download and install the R software and the latest free version
# of RStudio. To install R, navidate to [cran.r-project.org](https://cran.r-project.org/bin/windows/base/), and
# if you are on a Windows machine, click Download R-4.4.3 for Windows. This will download the installer to your Downloads foler.
# Once the download has been completed, click the installer to install R on your computer and follow the prompts without making any changes.
# Next, install RStudio by navigating to the [RStudio-desktop download page](https://posit.co/download/rstudio-desktop/).
# Click the blue tab under **2: Install RStudio** and click the installer to begin the installation once the downloda has completed.




# Clone the Country-specific MAAP Data Analysis repository from GitHub ----
# 1. Download GitHUb Desktop from the following link: (GitHub-Desktop download page)[https://desktop.github.com/download/], by clicking the **Download for Windows** tab.
# Follow the installation instructions and open the application once installation has completed successfully.
# 2. Go to File -> Clone repository -> URL and then paste the follwoing link https://github.com/ASLM-Fabebe/MAAP-Data-Analysis.git in the first box and click Clone
# 3. In RStudio go to File -> New Project -> Existing Project -> Browse (Navigate to your Documents -> GitHub -> MAAP-Data-Analysis)
# 4. Click MAAP-Data-Analysis.Rproj
# This will open the MAAP-Data-Analysis scripts within RStudio on your local computer.

# Load packages -----------------------------------------------------------



source(file.path("scripts","install_packages_packman.R"))


date_var <- as.Date(date(), format = "%a %b %d %H:%M:%S %Y")



# Load functions ----------------------------------------------------------

source(file.path("functions","amr_analysis_functions_01.R"))


# Load data sources and preprocess to remove NAs ---------------------------
amr <- readxl::read_excel("test-data/AMR data set.xlsx") %>%
  dplyr::mutate(rid=row_number()) # assign distinct RIDs


# Specify mandatory columns i.e. columns that should not be NULL/NA
man_cols = c("Specimen date", # data specimen collected
             "Specimen type" , # type of specimemn
             "Organism") # organism identified


# Remove with NA values in any of the mandatory columns
amr <- amr %>%
  filter(if_all(all_of(man_cols), ~ !is.na(.)))


# Split demographics and results ------------------------------------------


# get demographics
demo_vec <- c("rid","Identification number","First name","Last name",
  "Sex","Date of birth","Age","Age category","Date of admission","Reason")


lkp_demographics <- amr %>%
  dplyr::select(any_of(demo_vec))


# get lab and facility information
facility_vec <- c("rid","Identification number","Laboratory","Institution",
  "Location","Location type","Department","Origin","Country")

lkp_facility <- amr %>%
  dplyr::select(any_of(facility_vec)) %>%
  dplyr::select(where(~ !all(is.na(.))))  # remove columns with no values



# get specimens info
specimen_vec <- c("rid","Identification number","Specimen date","Specimen number","Specimen type","Specimen type (Numeric)")
lkp_specimens <-  amr %>%
  dplyr::select(any_of(specimen_vec))

# get test results
drugs_vec <- c("AMK_ND30","AMP_ND10", "AZM_ND15", "FEP_ND30", "CFM_ND5",
               "CTX_ND30","FOX_ND30", "CAZ_ND30", "CRO_ND30", "CIP_ND5",
               "COL_ND10","DOR_ND10", "ETP_ND10", "GEN_ND10", "IPM_ND10",
               "LVX_ND5", "MEM_ND10", "MNO_ND30", "OXA_ND1", "PEN_ND10",
               "SPT_ND100","TGC_ND15","SXT_ND1.2","AMK_NM","AMP_NM","AZM_NM",
               "FEP_NM","CFM_NM","CTX_NM","FOX_NM","CAZ_NM","CRO_NM","CIP_NM",
               "COL_NM","DOR_NM","ETP_NM","GEN_NM","IPM_NM","LVX_NM","MEM_NM",
               "MNO_NM","OXA_NM","PEN_NM","SPT_NM","TGC_NM","SXT_NM","CTX_NE",
               "CRO_NE","PEN_NE")

# map antibiotics
abx_conformed <- as.ab(drugs_vec)


# get antimicrobial results
main_vars <- c("rid","Identification number","Organism","Specimen type")
amr_res <- amr %>%
  dplyr::select(any_of(c(main_vars,drugs_vec)))


p1 <- which(names(amr_res) == "Identification number")
p2 <- which(names(amr_res) == "Organism")
p3 <- which(names(amr_res) == "Specimen type")

names(amr_res)[p1] <- "uid"
names(amr_res)[p2] <- "organism"
names(amr_res)[p3] <- "specimen_type"



amr_res <- amr_res %>%
  dplyr::filter(!is.na(organism)) #%>%



# Patient Linking Step - OPTIONAL -----------------------------------------








# Antibiotic results preparation ------------------------------------------

amr_res <- amr_res %>%
  mutate(bacteria = as.mo(organism, info = TRUE)) %>%
  mutate(gramstain = mo_gramstain(bacteria)) %>%
  dplyr::select(rid, uid, specimen_type,bacteria, organism, gramstain,everything()) %>%
  mutate(across(all_of(abx_vec), ~as.character(.)))


mo_failures()
mo_uncertainties()


# pivot drugs from wide into long format and map to standard drug codes
famr_long <- amr_res %>%
  tidyr::pivot_longer(names_to = "drug_code",
                      cols = any_of(abx_vec),
                      values_to = "vals") %>%
  mutate(ab = as.ab(drug_code))



# separate breakpoints and SIR interpretations

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


# get organism full names
lkp_organisms <- AMR::microorganisms %>% dplyr::select(mo,fullname)

# pivot wide
sir_outcomes_df_wide <- sir_outcomes_df %>%
  dplyr::select(-c(drug_code,int_id, vals, intrinsic_res_status)) %>%
  pivot_wider(names_from = "ab",
              values_from = "interpreted_res") %>%
  left_join(lkp_organisms, by=join_by("bacteria"=="mo")) %>%
  dplyr::select(rid,uid,specimen_type,mo_organism=fullname,
                gramstain,test_type,guideline, everything())

instrinsic_res_status_df_wide <- sir_outcomes_df %>%
  dplyr::select(-c(drug_code,int_id, vals, interpreted_res)) %>%
  pivot_wider(names_from = "ab",
              values_from = "intrinsic_res_status") %>%
  left_join(lkp_organisms, by=join_by("bacteria"=="mo")) %>%
  dplyr::select(rid,uid,specimen_type,mo_organism=fullname,
                gramstain,test_type,guideline, everything())

# Start downstream analysis -----------------------------------------------

# Look-up tables
lkp_specimens
lkp_demographics
lkp_facility

# Results tables
sir_outcomes_df_wide          # Antimicrobial results (SIR Interpretations)
instrinsic_res_status_df_wide # intrinsic_res_status results







# End ---------------------------------------------------------------------



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

