##functions

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
