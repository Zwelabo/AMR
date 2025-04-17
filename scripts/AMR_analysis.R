source(file.path("scripts","amr_data_cleaning_and_sir_interpretation.R"))

#ANALYSIS BY PATHOGEN
par_df <- tibble(param=c('Age', 'Sex', 'Specimen Type'),           #variables of interest
                 var_name=c('Age_g', 'Sex', 'specimen_type')) %>%  #corresponding column names
  mutate(id=paste0(param,var_name))
cntry='TZ'                           #input your country


for (i in par_df$id) {

  par=par_df$param[par_df$id==i]
  par_var_name=par_df$var_name[par_df$id==i]

  #E. COLI
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Escherichia coli',
    abs_ref <- c("AMP", "CFR", "CTX", "CAZ", "CIP", "GEN", "TOB", "MEC", "MEM", "NIT", "TZP", "TMP", "SXT")
  )

  #Klebsiella pneumoniae
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Klebsiella pneumoniae',
    abs_ref <- c('AMC', 'CXM', 'CTX', 'CRO', 'CAZ', 'CIP', 'GEN', 'TOB', 'TMP', 'SXT', 'MEM', 'TZP', 'TMP', 'LVX', 'ERY')
  )

  #S. aureus
  mrsa_analysis(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name="Staphylococcus aureus",
    abs_ref <- c('FOX', 'CLI', 'ERY', 'GEN', 'TOB', 'FUS', 'LNZ', 'RIF', 'TMP')
  )

  #Proteus mirabilis
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Proteus mirabilis',
    abs_ref <- c('AMX', 'AMP', 'AMC', 'CXM', 'CTX', 'CRO', 'CAZ', 'CIP', 'GEN', 'TOB', 'TMP', 'SXT')
  )

  #P. aeruginosa
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Pseudomonas aeruginosa',
    abs_ref <- c('TZP', 'CAZ', 'MEM', 'IPM', 'CIP', 'TOB')
  )

  #S. pneumoniae
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Streptococcus pneumoniae',
    abs_ref <- c('PEN', 'CIP', 'PHN', 'TCY', 'SXT')
  )

  #S. pyogenes
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Streptococcus pyogenes',
    abs_ref <- c('PEN', 'CLI', 'TCY', 'SXT', 'ERY')
  )

  #Enterococcus faecalis/ faecium
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Enterococcus faecalis',
    abs_ref <- c('AMP', 'GEN', 'LNZ', 'TZP', 'VAN')
  )

  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Enterococcus faecium',
    abs_ref <- c('AMP', 'GEN', 'LNZ', 'TZP', 'VAN')
  )

  #Haemophilus influenzae
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Haemophilus influenzae',
    abs_ref <- c('AMP', 'AMX', 'AMC', 'CTX', 'PEN', 'TCY', 'TMP', 'CIP')
  )

  #Acinetobacter baumannii

  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Acinetobacter baumannii',
    abs_ref <- c('MEM', 'CIP', 'TMP' ,'GEN', 'TOB', 'AMK')
  )

  #Neisseria gonorrhoeae
  amr_individual_pathogens(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Neisseria gonorrhoeae',
    abs_ref <- c('CFM', 'CRO', 'AZM', 'CIP', 'SPT')
  )


  #Pathogen groups
  # #Streptococcus spp
  amr_pathogen_groups(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Streptococcus',
    abs_ref <- c('ERY', 'DOX', 'TCY', 'SXT', 'CIP')
  )

  #Acinetobacter spp
  amr_pathogen_groups(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Acinetobacter',
    abs_ref <- c('MEM', 'CIP', 'TMP' ,'GEN', 'TOB', 'AMK')
  )

  # #shigella
  amr_pathogen_groups(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Shigella',
    abs_ref <- c('CIP', 'TMP', 'CTX', 'CAZ', 'MEM', 'AZM', 'TZP')
  )

  # #Salmonella
  amr_pathogen_groups(
    cntry = cntry,
    par=par,
    par_var_name=par_var_name,
    org_name='Shigella',
    abs_ref <-  c('AZM', 'CTX', 'CAZ', 'MEM', 'CIP', 'MEM', 'TZP', 'TMP')
  )
}



#antibiogram
abg_df <- an_df %>%
  mutate_if(is_sir_eligible, as.sir) %>%
  antibiogram()

write.csv(abg_df, paste0('Results/',cntry,'_antibiogram.csv'))
