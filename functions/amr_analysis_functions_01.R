# Functions ---------------------------------------------------------------

# Function 1: Convert breakpoints to SIR

convert2sir_fun <- function(df){
  
  df_len <- nrow(df)
  if(df_len == 0){
    stop("df seems empty.")
  }
  
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
    value= as.numeric(res_list[[i]][["vals"]])
    
    
    
    if(test == 'mic'){
      if(is.na(value)){
        value=res_list[[i]][["vals"]]
        interpreted <- as.character(as.sir(value, mo=paste0(bacteria), guideline = paste0(guideline), ab=paste0(drug_code)))
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





# Function 2: 
amr_grp1_analysis= function(org_name, abs_ref, cntry, par, par_var_name){

  sel_abs <- subset(abs_ref,  abs_ref %in% names(an_df))

    hold_df <- an_df %>%
    filter(pathogen==org_name) %>%
    group_by(across(all_of(c('pathogen', par_var_name)))) %>%
    summarise(across(c(sel_abs),  #create a vector for each pathogen of interest
                     list(n=n_sir,
                          r=count_R,
                          total_R = resistance#,
                          #conf_int = function(x) sir_confidence_interval(x, collapse = "-")
                     )))

  write.csv(hold_df, paste0('outputs/',cntry,'_',org_name,'_',par,'.csv'))

  hold_df_plot <- hold_df%>%
    pivot_longer(names_to = 'variable', values_to = 'values',
                 cols = names(hold_df)[3:length(hold_df)]) %>%                  #from col 3 onwards
    mutate(ab=ab_name(ifelse(grepl('total', variable), str_split_i(variable, '_', 1), '')))

  plt_hold <- ggplot(hold_df_plot %>% filter(ab!='' ), aes(x=.data[[paste0(par_var_name)]], y=values*100, group=ab, fill=ab))+
    geom_col( position = 'dodge')+
    labs(x=par, y='Percent resistance')+
    theme_bw()+
    theme(axis.text.x = element_text(size=11,angle=0,hjust=0.5,vjust=0.5),axis.text.y = element_text(size=10),axis.title = element_text(size=12),
          axis.title.x = element_text(size=11),
          #panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, face='bold'),legend.key.size = unit(0.7, "cm"),legend.spacing.x = unit(0.5,"cm"),
          strip.text = element_text(size=11),legend.text=element_text(size=8),legend.title = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),legend.position='bottom')


  ggsave(paste0('plots/',cntry,'_',org_name,'_',par,'.png'), plt_hold, width=8, height=8, units="in", dpi=300)



}
