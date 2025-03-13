library(AMR)
library(tidyverse)
library(ggplot2)
library(readxl)

#importing the dataset
r_data <- read_excel('data/AMR data set.xlsx') %>%
  select(where(~ any(!is.na(.))))  #dropping all empty columns


#retrieving the antibiotic columns
ab_codes <- names(r_data)[grepl('_N|_E', names(r_data))]


#valid records with mandatory columns
man_cols = c("Specimen date", # data specimen collected
             "Specimen type" , # type of specimemn
             "Organism") # organism identified


r1_data <- r_data %>% drop_na(man_cols)    #dropping all records without the mandatory columns



#I want to reshape the data to obtain the antibiotic codes, check ast standards adherence and re_shape back

#standardize antibiotic columns

r1_data[ab_codes]=sapply(r1_data[ab_codes], as.character)

r1_long <- r1_data %>% mutate(pathogen=as.mo(Organism),
                         id=1:nrow(.)) %>%
  pivot_longer(names_to = 'agent_code', values_to = 'res_values', cols = (ab_codes)) %>%
  mutate(agent=as.ab(agent_code),
         test_type=ifelse(grepl('_NM|_EM', agent_code), 'mic',
                          'disk'),
         guideline=ifelse(grepl('_N', agent_code), 'CLSI',
                          ifelse(grepl('_E', agent_code), 'EUCAST',
                                 'CLSI 2022')),
         interpreted_res='',                    #to hold results for the next part
         intrinsic_res_status='',
         rownames_to_column(., 'int_id')) %>%   #Debatable
  filter(!is.na(res_values))


##populating/updating the interpretations
#will await for scenarios when there is mixed reporting i.e diameters and RIS

for (i in r1_long$int_id){
  isol= r1_long %>% mutate() %>%
    filter(int_id==i)

  id=isol$id
  pathogen=isol$pathogen[1]
  agent=isol$agent[1]
  test=isol$test_type[1]
  guideline=isol$guideline[1]
  value= as.numeric(isol$res_values[1])

  ifelse(test == 'mic',
         interpreted <- as.character(as.sir(as.mic(value), mo=paste0(pathogen), guideline = paste0(guideline), ab=paste0(agent))),
         interpreted <- as.sir(as.disk(value), mo=paste0(pathogen), guideline = paste0(guideline), ab=paste0(agent))
  )

  intrinsic_status <- as.character(mo_is_intrinsic_resistant( paste0(pathogen),ab=paste0(agent)))

  #update the df
  r1_long$interpreted_res[r1_long$int_id==i]=paste0(interpreted)
  r1_long$intrinsic_res_status[r1_long$int_id==i]=paste0(intrinsic_status)
}
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

