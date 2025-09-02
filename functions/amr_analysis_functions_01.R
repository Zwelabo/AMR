


# Functions ---------------------------------------------------------------

# Function 1: Convert breakpoints to SIR

convert2sir_fun <- function(df){

  if (is.null(df) || nrow(df) == 0) {    #if there are no sir reported or no zones/mics
    x <- NULL
  } else{

  res_list <- list()

  id_vec <- df %>% pull(int_id)

  for (i in id_vec){

    res_list[[i]] <- df %>%
      mutate() %>%
      dplyr::filter(int_id==i)

    id=res_list[[i]][["int_id"]]
    bacteria=res_list[[i]][["bacteria"]]
    drug_code=as.ab(res_list[[i]][["drug_code"]])
    test=res_list[[i]][["test_type"]]
    guideline=res_list[[i]][["guideline"]]
    value= suppressWarnings(as.numeric(res_list[[i]][["vals"]]))



    if(test == 'mic'){
      if(is.na(value)){
        value=res_list[[i]][["vals"]]
        interpreted <- as.character(as.sir(value, mo=paste0(bacteria), guideline = paste0(guideline), ab=paste0(drug_code))) #the SIR
      }else{
        interpreted <- as.character(as.sir(as.mic(value), mo=paste0(bacteria), guideline = paste0(guideline), ab=paste0(drug_code)))
      }
    }else{
      if(is.na(value)){
        value=res_list[[i]][["vals"]]
        interpreted <- as.sir(value, mo=paste0(bacteria), guideline = paste0(guideline), ab=paste0(drug_code))
      }else{
        interpreted <- as.sir(as.disk(value), mo=paste0(bacteria), guideline = paste0(guideline), ab=paste0(drug_code))
      }
    }


    intrinsic_status <- as.character(mo_is_intrinsic_resistant( paste0(bacteria),ab=paste0(drug_code)))

    #update the df
    #res_list[[i]][["drug_code"]]=paste0(drug_code) #keep unconformed drug for join back to demographics
    res_list[[i]][["interpreted_res"]]=paste0(interpreted)
    res_list[[i]][["intrinsic_res_status"]]=paste0(intrinsic_status)

  }


  x<-dplyr::bind_rows(res_list)

  return(x)

}
}


#------------------------------------------------------------------------------------------------

##Resistance calculation functions
indiv_ab_resistance <- function(df,path){
  hold_df <- df %>%
    filter(mo_organism==org_name) %>%
    group_by(mo_organism, get(par_var_name), ab) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name=ab_name(ab),
           var_name=`get(par_var_name)`)

  write.csv(hold_df, file.path(org_res_dir,paste0(cntry,'_',org_name,'_',par,'.csv')))

  #plots
  ##preseving the empty cols for plotting equal bars
  hold_df <-  hold_df %>% filter(n>29) %>%
    filter(!is.na(var_name)) %>%
    ungroup() %>%
    complete(ab_name, var_name, fill = list(total_R = 0))%>%
    mutate(.is_missing = total_R == 0)   # flagging the ghosts

  ggsave(file.path(org_res_dir,paste0(cntry,'_',org_name,'_individual_abs','.png')), indiv_ab_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)
}


overall_ab_resistance <- function(df,path){
  hold_df <- df %>%
    filter(mo_organism==org_name) %>%
    filter(ab %in% sel_abs) %>%
    group_by( ab) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name=ab_name(ab))

  write.csv(hold_df, file.path(org_res_dir,paste0(cntry,'_',org_name,'_overall_abs.csv')))
#plot
  ggsave(file.path(org_res_dir,paste0(cntry,'_',org_name,'_overall_abs','.png')), overall_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)
}


indiv_ab_resistance_sau <- function(df){
  hold_df_mrsa <- df %>%
    filter(mo_organism==org_name) %>%
    filter(ab %in% mrsa_abs) %>%
    group_by(mo_organism, get(par_var_name)) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name='MRSA',
           var_name=`get(par_var_name)`)


  write.csv(hold_df_mrsa, paste0(cntry, '/Results_AMR/Bacteria/Staphylococcus_aureus/',cntry,'_',org_name,'_mrsa',par,'.csv'))


  #other combos
  hold_df_a <- df %>%
    filter(mo_organism==org_name) %>%
    filter(ab %in% sel_abs) %>%
    group_by(mo_organism, get(par_var_name), ab) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name=ab_name(ab),
           var_name=`get(par_var_name)`)

  write.csv(hold_df_a, paste0(cntry, '/Results_AMR/Bacteria/Staphylococcus_aureus/',cntry,'_',org_name,'_',par,'.csv'))

  hold_df <- bind_rows(hold_df_a, hold_df_mrsa)

  #plots
  ##preseving the empty cols for plotting equal bars
  hold_df <-  hold_df %>% filter(n>29) %>%
    filter(!is.na(var_name)) %>%
    ungroup() %>%
    complete(ab_name, var_name, fill = list(total_R = 0))%>%
    mutate(.is_missing = total_R == 0)   # flagging the ghosts

  ggsave(paste0(cntry, '/Results_AMR/Bacteria/Staphylococcus_aureus/',cntry,'_',org_name,'_overall_abs','.png'), indiv_ab_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)
}

overall_ab_resistance_sau <- function(df){
  hold_df_mrsa <- df %>%
    filter(mo_organism==org_name) %>%
    filter(ab %in% mrsa_abs) %>%
    group_by( mo_organism) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name='MRSA')


  write.csv(hold_df_mrsa, paste0(cntry, '/Results_AMR/Bacteria/Staphylococcus_aureus/',cntry,'_',org_name,'mrsa_overall_abs.csv'))

  #other combos
  hold_df_a <- df %>%
    filter(mo_organism==org_name) %>%
    group_by( ab) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name=ab_name(ab))


  write.csv(hold_df_a, paste0(cntry, '/Results_AMR/',cntry,'_',org_name,'_overall_abs.csv'))

  hold_df <- bind_rows(hold_df_a, hold_df_mrsa)

  #plots
  ggsave(paste0(cntry, '/Results_AMR/Bacteria/Staphylococcus_aureus/',cntry,'_',org_name,'_overall_abs','.png'), overall_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)
}

  #pathogen groups
indiv_ab_resistance_genus <- function(df,path){
    hold_df <- df %>%
      mutate(mo_organism=genus) %>%
      filter(ab %in% sel_abs) %>%
      filter(mo_organism==org_name) %>%
      group_by(mo_organism, get(par_var_name), ab) %>%
      summarise(n=n(),
                r=sum(R),
                total_R = r/n  ) %>%
      mutate(ab_name=ab_name(ab),
             var_name=`get(par_var_name)`)

    write.csv(hold_df, file.path(org_res_dir,paste0(cntry,'_',org_name,'_',par,'.csv')))

    #plots
    ##preseving the empty cols for plotting equal bars
    hold_df <-  hold_df %>% filter(n>29) %>%
      filter(!is.na(var_name)) %>%
      ungroup() %>%
      complete(ab_name, var_name, fill = list(total_R = 0))%>%
      mutate(.is_missing = total_R == 0)   # flagging the ghosts

    ggsave(file.path(org_res_dir,paste0(cntry,'_',org_name,'_overall_abs','.png')), indiv_ab_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)
  }


overall_ab_resistance_genus <- function(df,path){
    hold_df <- df %>%
      mutate(mo_organism=genus) %>%
      filter(mo_organism==org_name) %>%
      filter(ab %in% sel_abs) %>%
      group_by( ab) %>%
      summarise(n=n(),
                r=sum(R),
                total_R = r/n  ) %>%
      mutate(ab_name=ab_name(ab))

    write.csv(hold_df, file.path(org_res_dir,paste0(cntry,'_',org_name,'_overall_abs.csv')))

  #plots
  ggsave(file.path(org_res_dir,paste0(cntry,'_',org_name,'_overall_abs','.png')),overall_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)
}




# Antibiotic class resistance ---------------------------------------------



#Individual pathogens
antibiotic_classes_res_indiv <- function(df,path) {

  abx_classes <- unique(ab_group(abs_ref)) #returns the antibiotic classes

  hold_df <- df %>% filter(ab_class %in% abx_classes) %>%
    filter(mo_organism==org_name) %>%
    arrange(desc(R)) %>%
    distinct(uid,mo_organism,ab_class, .keep_all = TRUE) %>% #remove duplicates for the Ab classes, depending on how vast the dataset is, could be updated to country, lab, etc
    group_by(mo_organism, get(par_var_name), ab_class) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name=ab_class,
           var_name=`get(par_var_name)`)

  write.csv(hold_df, file.path(org_res_dir,paste0(cntry,'_',org_name,'_',par,'ab_classes.csv')))

  #plot
  ##preseving the empty cols for plotting equal bars
  hold_df <-  hold_df %>% filter(n>29) %>%
    filter(!is.na(var_name)) %>%
    ungroup() %>%
    complete(ab_name, var_name, fill = list(total_R = 0))%>%
    mutate(.is_missing = total_R == 0)   # flagging the ghosts

  ggsave(file.path(org_res_dir,paste0(cntry,'_',org_name,'_',par,'ab_classes.png')), indiv_ab_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)


  #calculating overall resistance
  hold_df <- df %>%
    filter(mo_organism==org_name) %>%
    filter(ab_class %in% abx_classes) %>%
    arrange(desc(R)) %>%
    distinct(uid,mo_organism,ab_class, .keep_all = TRUE) %>% #remove duplicates for the Ab classes, depending on how vast the dataset is, could be updated to country, lab, etc
    group_by(mo_organism, ab_class) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name=ab_class)

  write.csv(hold_df, file.path(org_res_dir,paste0(cntry,'_',org_name,'_overall_ab_classes.csv')))

  #plot
  ggsave(file.path(org_res_dir,paste0(cntry,'_',org_name,'_overall_ab_classes','.png')), overall_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)
}


#Grouped pathogens
antibiotic_classes_res_grp <- function(df,path) {

  abx_classes <- unique(ab_group(abs_ref)) #returns the antibiotic classes

  hold_df <- df %>% filter(ab_class %in% abx_classes) %>%
    mutate(mo_organism=genus) %>%
    filter(mo_organism==org_name) %>%
    arrange(desc(R)) %>%
    distinct(uid,mo_organism,ab_class, .keep_all = TRUE) %>% #remove duplicates for the Ab classes, depending on how vast the dataset is, could be updated to country, lab, etc
    group_by(mo_organism, get(par_var_name), ab_class) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name=ab_class,
           var_name=`get(par_var_name)`)

  write.csv(hold_df, file.path(org_res_dir,paste0(cntry,'_',org_name,'_',par,'ab_classes.csv')))

  #plot
  ##preseving the empty cols for plotting equal bars
  hold_df <-  hold_df %>% filter(n>29) %>%
    filter(!is.na(var_name)) %>%
    ungroup() %>%
    complete(ab_name, var_name, fill = list(total_R = 0))%>%
    mutate(.is_missing = total_R == 0)   # flagging the ghosts

  ggsave(file.path(org_res_dir,paste0(cntry,'_',org_name,'_',par,'ab_classes.png')), indiv_ab_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)


  #calculating overall resistance
  hold_df <- df %>%
    mutate(mo_organism=genus) %>%
    filter(mo_organism==org_name) %>%
    filter(ab_class %in% abx_classes) %>%
    arrange(desc(R)) %>%
    distinct(uid,mo_organism,ab_class, .keep_all = TRUE) %>% #remove duplicates for the Ab classes, depending on how vast the dataset is, could be updated to country, lab, etc
    group_by(mo_organism, ab_class) %>%
    summarise(n=n(),
              r=sum(R),
              total_R = r/n  ) %>%
    mutate(ab_name=ab_class)

  write.csv(hold_df, file.path(org_res_dir,paste0(cntry,'_',org_name,'_overall_ab_classes.csv')))

  #plot
  ggsave(file.path(org_res_dir,paste0(cntry,'_',org_name,cntry,'_overall_ab_classes','.png')), overall_resistance_plot(hold_df), width=8, height=8, units="in", dpi=300)
}


# Plotting functions ------------------------------------------------------

overall_resistance_plot <- function(df) {
  plt_hold <- ggplot(df  %>% filter(n>29), aes(x=reorder(ab_name,total_R), y=total_R*100, group=ab_name))+
    geom_col(position = 'dodge', fill='dodgerblue')+
    labs(x='Antibiotic', y='Percent resistance')+
    theme_bw()+
    theme(axis.text.x = element_text(size=11,angle=90,hjust=1,vjust=0),axis.text.y = element_text(size=10),axis.title = element_text(size=12),
          axis.title.x = element_text(size=11),
          plot.title = element_text(hjust = 0.5, face='bold'),legend.key.size = unit(0.7, "cm"),legend.spacing.x = unit(0.5,"cm"),
          strip.text = element_text(size=11),legend.text=element_text(size=8),legend.title = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),legend.position='right')
 plt_hold
}


indiv_ab_resistance_plot <- function(df) {
  plt_hold <- ggplot(df, aes(x=ab_name, y=total_R*100, group=var_name, fill=var_name))+
    geom_col( position = position_dodge(width = 0.9))+
    #geom_text(aes( y=total_R*100,label=round(total_R*100,1)), position=position_dodge(width = 0.9), vjust=-.5)+
    labs(x=par, y='Percent resistance')+
    scale_fill_brewer(palette='Dark2',direction = -1)+
    theme_bw()+
    theme(axis.text.x = element_text(size=11,angle=90,hjust=1,vjust=0),axis.text.y = element_text(size=10),axis.title = element_text(size=12),
          axis.title.x = element_text(size=11),
          #panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, face='bold'),legend.key.size = unit(0.7, "cm"),legend.spacing.x = unit(0.5,"cm"),
          strip.text = element_text(size=11),legend.text=element_text(size=8),legend.title = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),legend.position='bottom')
  plt_hold
}



# Analysis functions ------------------------------------------------------


amr_individual_pathogens <-  function(xdf, org_res_dir,org_name, abs_ref, cntry, par, par_var_name){
  sel_abs <- subset(abs_ref,  abs_ref %in% xdf$ab)

  #subgroups
  environment(indiv_ab_resistance) <- environment()
  indiv_ab_resistance(xdf,org_res_dir)

  #calculating overall resistance
  environment(overall_ab_resistance) <- environment()
  overall_ab_resistance(xdf,org_res_dir)

  #class resistance
  environment(antibiotic_classes_res_indiv) <- environment()
  antibiotic_classes_res_indiv(xdf,org_res_dir)
}


#MRSA
mrsa_analysis <-  function(xdf,org_name, abs_ref, cntry, par, par_var_name){
  mrsa_abs <- c('FOX', 'MET', 'OXA')
  sel_abs_mrsa <- subset(mrsa_abs,  mrsa_abs %in% names(an_df))
  sel_abs <- subset(abs_ref,  abs_ref %in% names(an_df))

  #MRSA
  #subgroups
  environment(indiv_ab_resistance_sau) <- environment()
  indiv_ab_resistance_sau(xdf)

  #calculating overall resistance
  #MRSA
  environment(overall_ab_resistance_sau) <- environment()
  overall_ab_resistance_sau(xdf)

  #class resistance
  environment(antibiotic_classes_res_indiv) <- environment()
  antibiotic_classes_res_indiv(xdf)
}

##
amr_pathogen_groups <-  function(xdf,org_res_dir,org_name, abs_ref, cntry, par, par_var_name){
  sel_abs <- subset(abs_ref,  abs_ref %in% xdf$ab)

  #subgroups
  environment(indiv_ab_resistance_genus) <- environment()
  indiv_ab_resistance_genus(xdf,org_res_dir)

  #calculating overall resistance
  environment(overall_ab_resistance_genus) <- environment()
  overall_ab_resistance_genus(xdf,org_res_dir)

  #class resistance
  environment(antibiotic_classes_res_grp) <- environment()
  antibiotic_classes_res_grp(xdf,org_res_dir)
}

#------------------------------------------------------------------------------------------------------


