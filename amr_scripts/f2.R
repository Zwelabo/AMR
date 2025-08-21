
message('Loading functions ....')
cat('Loading functions ....')
# Load functions ----------------------------------------------------------

source(file.path("functions","amr_analysis_functions_01.R"))
source(file.path("functions","amr_analysis_functions_02.R"))


# Create output folder: Results -------------------------------------------


res_dir <- file.path(paste0(cntry,"/Results_AMR"))

if (!dir.exists(res_dir)){

  dir.create(res_dir, recursive = T)

}

date_var <- as.Date(date(), format = "%a %b %d %H:%M:%S %Y")



amr <- get_inputs_and_prelim_cleanup()

# Begin analysis here -----------------------------------------------------
message('Beginning analysis ....')
cat('Beginning analysis here ....')


lkp_demographics <- get_demographics(df=amr)

lkp_facility <- get_facilities_data(df=amr)

lkp_specimens <- get_specimen_info(df=amr)

amr_res <- get_test_results(df=amr)

famr_long <- pivot_abx_results(df=amr_res)

# separate breakpoints and SIR interpretations
famr_long_sir <- get_sir_interpr(df=famr_long)
famr_long_con <- get_con_interp(df=famr_long)


# Convert breakpoints to SIR ----------------------------------------------
message('Converting breakpoints to SIR ....')
cat('Beginning analysis here ....\n')



amr_con <- convert2sir_fun(famr_long_con)

amr_sir <- convert2sir_fun(famr_long_sir)

# combine results

sir_outcomes_df <- dplyr::bind_rows(amr_con, amr_sir) %>%

  dplyr::filter(intrinsic_res_status=='FALSE') %>%   #drop the intrinsically resistant bug-drugs to not skew results

  dplyr::filter(interpreted_res!='NA')  #drop the UNINTERPRETABLE COMBOS FROM GUIDELINES


excluded_rec <- bind_rows(amr_con, amr_sir) %>%

  dplyr::filter(intrinsic_res_status=='TRUE'| interpreted_res=='NA')


# get organism full names

lkp_organisms <- AMR::microorganisms %>% dplyr::select(mo,fullname)  #puls the entire list


# pivot wide

sir_outcomes_df_wide <- sir_outcomes_df %>%

  dplyr::select(-c(drug_code,int_id, vals, intrinsic_res_status)) %>%

  pivot_wider(names_from = "ab",

              values_from = "interpreted_res") %>%

  left_join(lkp_organisms, by=join_by("bacteria"=="mo")) %>%

  dplyr::select(r_id,uid,specimen_type,mo_organism=fullname,

                gramstain,test_type,guideline, everything())



# Start the downstream analysis -------------------------------------------

message('Beginning downstream analysis ....')
cat('Beginning downstream analysis ....\n')

# Please note Rates are only shown when n >=30 according to GLASS reccommendations

#create an analysis dataframe

an_df <- sir_outcomes_df_wide %>%
  left_join(lkp_demographics %>% select(Age, Sex,r_id), by='r_id') %>%
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




ab_cols <- as.character(unique(sir_outcomes_df$ab))  #antibiotic columns

if(file.exists('amr_resources/ab_class_list.csv')){
  ab_class_list <- read.csv('amr_resources/ab_class_list.csv')
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

openxlsx::write.xlsx(lkp_organisms,file = paste0(cntry,"/Results_AMR/Organisms.",date_var,".xlsx"))
openxlsx::write.xlsx(lkp_demographics,file = paste0(cntry,"/Results_AMR/Demographics.",date_var,".xlsx"))
openxlsx::write.xlsx(lkp_facility,file = paste0(cntry,"/Results_AMR/Facilities.",date_var,".xlsx"))
openxlsx::write.xlsx(sir_outcomes_df_wide,file = paste0(cntry,"/Results_AMR/AST.results.",date_var,".xlsx"))
openxlsx::write.xlsx(excluded_rec,file = paste0(cntry,"/Results_AMR/Intrinsic.noguidelines.results.",date_var,".xlsx"))


#openxlsx::write.xlsx(lkp_organisms,file = paste0("Organisms.",date_var,".xlsx"))
#openxlsx::write.xlsx(lkp_demographics,file = paste0("Demographics.",date_var,".xlsx"))
#openxlsx::write.xlsx(lkp_facility,file = paste0("Facilities.",date_var,".xlsx"))
#openxlsx::write.xlsx(sir_outcomes_df_wide,file = paste0("AST.results.",date_var,".xlsx"))
#openxlsx::write.xlsx(excluded_rec,file = paste0("Intrinsic.noguidelines.results.",date_var,".xlsx"))



# Get bug-drug combinations  ----------------------------------------------
# where at least 30 (default) isolates are available per species

#-Gilbeert: commenting this one out
#bug_drug_combos <- format(AMR::bug_drug_combinations(an_df_long))
#openxlsx::write.xlsx(bug_drug_combos,file = file.path("Results",paste0("Bug_drug_combinations.results.",date_var,".xlsx")))

# National antibiogram ----------------------------------------------------
message('Generating the antibiogram ....')
cat('Generating the antibiogram ....\n')

abg_df <- an_df %>%

  #filter(mo_organism==org_name) %>%

  mutate_if(is_sir_eligible, as.sir) %>%

  antibiogram()

# Creating a workbook and worksheet
wb <- createWorkbook()
addWorksheet(wb, "antibiogram")

# Write data to Excel
writeData(wb, sheet = "antibiogram", x = abg_df)

# Apply color scale to 'Score' column (column B)
conditionalFormatting(
  wb,
  sheet = "antibiogram",
  cols = 2:length(names(abg_df)),
  rows = 2:(nrow(abg_df)+1),  # +1 to account for header
  style = c("red", "yellow", "green"),
  type = "colourScale"
)

# Save to file
saveWorkbook(wb, file.path(paste0(cntry,"/Results_AMR/National.antibiogram.results.",date_var,".xlsx")), overwrite = TRUE)


# Detailed analysis -------------------------------------------------------

message('Profile analysis ....')
cat('Profile analysis ....\n')

## Define parameters for subgroup analyses
par_df <- tibble(param=c('Age', 'Sex', 'Specimen Type'), var_name=c('Age_g', 'Sex', 'specimen_type')) %>%
  mutate(id=paste0(param,var_name))

orgs_vec <- lkp_organisms %>% dplyr::pull(fullname)


if (exists("priority_bact_pathogens")) {
  priority_bact_vec <- orgs_vec[orgs_vec %in% priority_bact_pathogens]
} else {
  print("priority_bact_pathogens vector does NOT exist. Using universal priority pathogens list")

  priority_bact_pathogens <- c(
    "Acinetobacter baumannii",
    "Citrobacter",
    "Enterobacter",
    "Enterococcus faecalis",
    "Enterococcus faecium",
    "Escherichia coli",
    "Klebsiella pneumoniae",
    "Morganella",
    "Neisseria gonorrhoeae",
    "Salmonella",
    "Proteus",
    "Pseudomonas aeruginosa",
    "Serratia",
    "Shigella",
    "Staphylococcus aureus",
    "Streptococcus agalactiae",
    "Streptococcus pneumoniae",
    "Streptococcus pyogenes",
    "Helicobacter pylori",
    "Campylobacter",
    "Mycobacterium tuberculosis"
  )


  priority_bact_vec <- orgs_vec[orgs_vec %in% priority_bact_pathogens]

}

if (exists("priority_fungi_pathogens")) {
  priority_fungal_vec <- orgs_vec[orgs_vec %in% priority_fungal_pathogens]
} else {
  print("priority_fungal_pathogens vector does NOT exist. Using universal priority pathogens list")

  priority_fungal_pathogens <-c(
    "Cryptococcus neoformans",
    "Candida auris",
    "Aspergillus fumigatus",
    "Candida albicans",
    "Nakaseomyces glabrata",
    "Histoplasma",
    "Eumycetoma",
    "Mucorales",
    "Fusarium",
    "Candida tropicalis",
    "Candida parapsilosis",
    "Pneumocystis jirovecii",
    "Talaromyces marneffei",
    "Coccidioides",
    "Paracoccidioides",
    "Scedosporium",
    "Lomentospora prolificans",
    "Cladosporium"
  )

  priority_fungi_vec <- orgs_vec[orgs_vec %in% priority_fungal_pathogens]

}



for (i in par_df$id) {

par=par_df$param[par_df$id==i]

par_var_name=par_df$var_name[par_df$id==i]


  cntry = cntry

  par=par

  par_var_name=par_var_name

  # org_name='Escherichia coli',
  # abs_ref <- c("AMP", "CFR", "CTX", "CAZ", "CIP", "GEN", "TOB", "MEC", "MEM", "NIT", "TZP", "TMP", "SXT")



  abs_ref <- unique(an_df_long$ab)

  #Bacterial segment

  for(i in seq_along(priority_bact_vec)){
    org_name <- priority_bact_vec[i]
    org_name_dir <-str_replace_all(org_name," ","_")
    org_name_dir <-str_replace_all(org_name_dir,"\\(|\\)","_")
    org_res_dir <- file.path(paste0(cntry,"/Results_AMR/Bacteria"),org_name_dir, 'indiv')

    if(!dir.exists(org_res_dir)){dir.create(org_res_dir, recursive = T)}

    amr_individual_pathogens(an_df_long,org_res_dir,org_name, abs_ref, cntry, par, par_var_name)
  }



  org_name = "Staphylococcus aureus"
  mrsa_analysis(an_df_long,org_name, abs_ref, cntry, par, par_var_name)



  for(i in seq_along(priority_bact_vec)){
    org_name <- priority_bact_vec[i] ##extracting the genus name
    org_name_dir <-str_replace_all(org_name," ","_")
    org_name_dir <-str_replace_all(org_name_dir,"\\(|\\)","_")
    org_res_dir <- file.path(paste0(cntry,"/Results_AMR/Bacteria"),org_name_dir,"pathogen_grp")

    if(!dir.exists(org_res_dir)){dir.create(org_res_dir, recursive = T)}

    amr_pathogen_groups(an_df_long,org_res_dir,org_name, abs_ref, cntry, par, par_var_name)

  }

  #Fungal
  for(i in seq_along(priority_fungi_vec)){
    org_name <- priority_fungi_vec[i]
    org_name_dir <-str_replace_all(org_name," ","_")
    org_name_dir <-str_replace_all(org_name_dir,"\\(|\\)","_")
    org_res_dir <- file.path(paste0(cntry,"/Results_AMR/Fungi"),org_name_dir, 'indiv')

    if(!dir.exists(org_res_dir)){dir.create(org_res_dir, recursive = T)}

    amr_individual_pathogens(an_df_long,org_res_dir,org_name, abs_ref, cntry, par, par_var_name)
  }


  for(i in seq_along(priority_fungi_vec)){
    org_name <- priority_fungi_vec[i]   ##extracting the genus name
    org_name_dir <-str_replace_all(org_name," ","_")
    org_name_dir <-str_replace_all(org_name_dir,"\\(|\\)","_")
    org_res_dir <- file.path(paste0(cntry,"/Results_AMR/Fungi"),org_name_dir,"pathogen_grp")

    if(!dir.exists(org_res_dir)){dir.create(org_res_dir, recursive = T)}

    amr_pathogen_groups(an_df_long,org_res_dir,org_name, abs_ref, cntry, par, par_var_name)

  }


}

message('Analysis successfully completed ....')
cat('Analysis successfully completed ....\n')









