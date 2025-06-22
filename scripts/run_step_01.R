## Insert your country code ------------------------------------------------


#cntry='TZ' # TZ for Tanzania


# Specify organisms of interest -------------------------------------------

#eskape_pathogens <- c(
#  "Enterococcus faecium",
#  "Staphylococcus aureus",
#  "Klebsiella pneumoniae",
#  "Acinetobacter baumannii",
#  "Pseudomonas aeruginosa",
#  "Escherichia coli",
#  "Enterobacter cloacae complex",
#  "Enterobacter aerogenes",
#  "Enterobacter hormaechei"
#)

#print(eskape_pathogens)


# Create output folder: Results -------------------------------------------


res_dir <- file.path("Results")

if (!dir.exists(res_dir)){

  dir.create(res_dir, recursive = T)

}

date_var <- as.Date(date(), format = "%a %b %d %H:%M:%S %Y")


# Load functions ----------------------------------------------------------

source(file.path("functions","amr_analysis_functions_01.R"))
source(file.path("functions","amr_analysis_functions_02.R"))

amr <- get_inputs_and_prelim_cleanup()

# Begin analysis here -----------------------------------------------------

# setwd(res_dir)




lkp_demographics <- get_demographics(df=amr)

lkp_facility <- get_facilities_data(df=amr)

lkp_specimens <- get_specimen_info(df=amr)

amr_res <- get_test_results(df=amr)

famr_long <- pivot_abx_results(df=amr_res)

# separate breakpoints and SIR interpretations
famr_long_sir <- get_sir_interpr(df=famr_long)
famr_long_con <- get_con_interp(df=famr_long)


# Convert breakpoints to SIR ----------------------------------------------

amr_con <- convert2sir_fun(famr_long_con)

amr_sir <- convert2sir_fun(famr_long_sir)

# combine results

sir_outcomes_df <- amr_con %>%

  dplyr::union(amr_sir) %>%

  dplyr::filter(intrinsic_res_status=='FALSE') %>%   #drop the intrinsically resistant bug-drugs to not skew results

  dplyr::filter(interpreted_res!='NA')  #drop the UNINTERPRETABLE COMBOS FROM GUIDELINES


excluded_rec <- amr_con %>%

  dplyr::union(amr_sir) %>%

  dplyr::filter(intrinsic_res_status=='TRUE'| interpreted_res=='NA')

# get organism full names

lkp_organisms <- AMR::microorganisms %>% dplyr::select(mo,fullname)  #puls the entire list

# pivot wide

sir_outcomes_df_wide <- sir_outcomes_df %>%

  dplyr::select(-c(drug_code,int_id, vals, intrinsic_res_status)) %>%

  pivot_wider(names_from = "ab",

              values_from = "interpreted_res") %>%

  left_join(lkp_organisms, by=join_by("bacteria"=="mo")) %>%

  dplyr::select(rid,uid,specimen_type,mo_organism=fullname,

                gramstain,test_type,guideline, everything())



# Start the downstream analysis -------------------------------------------

# Please note Rates are only shown when n >=30 according to GLASS reccommendations

#create an analysis dataframe

an_df <- sir_outcomes_df_wide %>%
  left_join(lkp_demographics %>% select(Age, Sex,rid), by='rid') %>%
  ##cleanung Age cols
  mutate(Age_s=toupper(gsub("[[:digit:]]", "", Age)),
         Age_n= as.numeric(gsub("[^0-9.-]", "", Age)),
         Age_conv=ifelse(Age_s=='D',round(Age_n/365,0),
                         ifelse(Age_s=='W',round(Age_n/7,0),
                                ifelse(Age_s=='M',round(Age_n/12,0),
                                       ifelse(Age_s=='Y',round(Age_n/1,0),Age)))),
         Age_conv=as.numeric(Age_conv),
         Age_g=as.character(age_groups(Age_conv, split_at = c(1,5,20,50,65))),
         Age_g=ifelse(Age_g=='0', '<1',Age_g),
         Age_g=factor(Age_g, levels=c('<1','1-4','5-19', '20-49','50-64','65+')))




ab_cols <- unique(sir_outcomes_df$ab)  #antibiotic columns

if(file.exists('test-data/ab_class_list.csv')){
  ab_class_list <- read.csv('test-data/ab_class_list.csv')
}else{
  stop("Error: The file ab_class_list.csv does not exist. Please add file to test-data and try again.")
}

an_df_long <- an_df %>%
  pivot_longer(cols=ab_cols, names_to = 'ab', values_to = 'interpreted_res') %>%
  filter(!is.na(interpreted_res)) %>%
  mutate(R=ifelse(interpreted_res=='R',1,0),       ##resistance column
         genus=str_split_i(mo_organism, ' ',1)) %>%
  mutate(interpreted_res=as.sir(interpreted_res, clean = TRUE)) %>%
  left_join(ab_class_list, by='ab', relationship = "many-to-many")


# Write data to file ------------------------------------------------------

openxlsx::write.xlsx(lkp_organisms,file = file.path("Results",paste0("Organisms.",date_var,".xlsx")))
openxlsx::write.xlsx(lkp_demographics,file = file.path("Results",paste0("Demographics.",date_var,".xlsx")))
openxlsx::write.xlsx(lkp_facility,file = file.path("Results",paste0("Facilities.",date_var,".xlsx")))
openxlsx::write.xlsx(sir_outcomes_df_wide,file = file.path("Results",paste0("AST.results.",date_var,".xlsx")))
openxlsx::write.xlsx(excluded_rec,file = file.path("Results",paste0("Intrinsic.noguidelines.results.",date_var,".xlsx")))


#openxlsx::write.xlsx(lkp_organisms,file = paste0("Organisms.",date_var,".xlsx"))
#openxlsx::write.xlsx(lkp_demographics,file = paste0("Demographics.",date_var,".xlsx"))
#openxlsx::write.xlsx(lkp_facility,file = paste0("Facilities.",date_var,".xlsx"))
#openxlsx::write.xlsx(sir_outcomes_df_wide,file = paste0("AST.results.",date_var,".xlsx"))
#openxlsx::write.xlsx(excluded_rec,file = paste0("Intrinsic.noguidelines.results.",date_var,".xlsx"))



# Get bug-drug combinations  ----------------------------------------------
# where at least 30 (default) isolates are available per species

bug_drug_combos <- format(AMR::bug_drug_combinations(an_df_long))
openxlsx::write.xlsx(bug_drug_combos,file = file.path("Results",paste0("Bug_drug_combinations.results.",date_var,".xlsx")))

# National antibiogram ----------------------------------------------------


abg_df <- an_df %>%

  #filter(mo_organism==org_name) %>%

  mutate_if(is_sir_eligible, as.sir) %>%

  antibiogram()

openxlsx::write.xlsx(abg_df,file = file.path("Results",paste0("National.antibiogram.results.",date_var,".xlsx")))


## 2025-06-22: S. Kwenda: Commenting out this part because the
## amr_grp1_analysis() function doesn't exist in the current repo
#
## Define parameters for subgroup analyses
#par_df <- tibble(param=c('Age', 'Sex', 'Specimen Type'), var_name=c('Age_g', 'Sex', 'specimen_type')) %>%
#
#  mutate(id=paste0(param,var_name))
#
## Add/change pathogen and antibiotics of interests to obtain prevalance
## rates and plots from the 2nd function (amr_grp1_analysis)
#
#for (i in par_df$id) {
#
#  par=par_df$param[par_df$id==i]
#
#  par_var_name=par_df$var_name[par_df$id==i]
#
#
#  #E. COLI
#  amr_grp1_analysis(
#
#    cntry = cntry,
#
#    par=par,
#
#    par_var_name=par_var_name,
#
#    org_name='Escherichia coli',
#
#    abs_ref <- c("AMP", "CFR", "CTX", "CAZ", "CIP", "GEN", "TOB", "MEC", "MEM", "NIT", "TZP", "TMP", "SXT")
#
#  )
#
#  #other bug-drug combinations to follow
#
#}
## E. coli
#
#abs_ref <- c("AMP", "CFR", "CTX", "CAZ", "CIP", "GEN", "TOB", "MEC", "MEM", "NIT", "TZP", "TMP", "SXT")
#amr_individual_pathogens(org_name, abs_ref, cntry, par, par_var_name)


# Detailed analysis -------------------------------------------------------
orgs_vec <- lkp_organisms %>% dplyr::pull(fullname)

if (exists("eskape_pathogens")) {
  eskape_vec <- orgs_vec[orgs_vec %in% eskape_pathogens]
} else {
  print("eskape_pathogens vector does NOT exist. Using default ESKAPE pathogens list")

  eskape_pathogens <- c(
    "Enterococcus faecium",
    "Staphylococcus aureus",
    "Klebsiella pneumoniae",
    "Acinetobacter baumannii",
    "Pseudomonas aeruginosa",
    "Escherichia coli",
    "Enterobacter cloacae complex",
    "Enterobacter aerogenes",
    "Enterobacter hormaechei"
  )


  eskape_vec <- orgs_vec[orgs_vec %in% eskape_pathogens]

}




abs_ref <- unique(an_df_long$ab)

for(i in seq_along(eskape_vec)){
  org_name <- eskape_vec[i]
  org_name <-str_replace_all(org_name," ","_")
  org_name <-str_replace_all(org_name,"\\(|\\)","_")
  org_res_dir <- file.path("Results",org_name)

  if(!dir.exists(org_res_dir)){dir.create(org_res_dir, recursive = T)}

  amr_individual_pathogens(an_df_long,org_res_dir,org_name, abs_ref, cntry, par, par_var_name)
}



org_name = "Staphylococcus aureus"
mrsa_analysis(an_df_long,org_name, abs_ref, cntry, par, par_var_name)



for(i in seq_along(eskape_vec)){
  org_name <- eskape_vec[i]
  org_name <-str_replace_all(org_name," ","_")
  org_name <-str_replace_all(org_name,"\\(|\\)","_")
  org_res_dir <- file.path("Results",org_name,"indiv")

  if(!dir.exists(org_res_dir)){dir.create(org_res_dir, recursive = T)}

  amr_pathogen_groups(an_df_long,org_res_dir,org_name, abs_ref, cntry, par, par_var_name)

  }
