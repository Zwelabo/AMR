library(readxl)
library(tidyverse)
library(AMR)
library(ggplot2)
library(zoo)


#
cntry='TZ'
pop=67000000  #hypothetical population estimate

#importing the reference file
who_atc_ref <- read_excel('test-data/AMC/ATC-DDD WHO core and optional antimicrobials.xlsx') %>%
  filter(`Core or Optional`=='Core') %>%
  mutate(name_route=paste0(`ATC level name`, '_',tolower(Adm.R))) %>%
  rename(aware_cats=`2023 AWaRe categorization (for J01)`) %>%
  distinct(name_route, .keep_all = T)


#create the results directory
amc_dir <- file.path("Results_AMC",cntry)

if(!dir.exists(amc_dir)){dir.create(amc_dir, recursive = T)}

#importing the test dataset (focus is on non-combinational drugs)
amc_test <- read_excel('test-data/AMC/AMC_test_data.xlsx') %>%
  tidyr::extract("product", c("product", "strength"), "(\\D*)(\\d.*)") %>%   #separating product names from strength
  tidyr::extract("pack_size", c("pack_size_unit", "pack_size"), "(\\D*)(\\d.*)") %>%   #separating product names from strength
  mutate(reported_route=route,
         route=ifelse(str_detect(tolower(route), 'oral'),'o',           #administration routes
                      ifelse(str_detect( tolower(route), 'parenteral'),'p',
                             tolower(route))),
         name_route=paste0(trimws(product), '_', route),
         strength_val=str_split_i(strength , "([A-Za-z]+)",1),   #strength integer
         strength_unit_r=substr(strength , nchar(strength_val)+1, 50), #strength unit
         strength_unit=str_split_i(strength_unit_r , "(/)", 1), #strength unit
         strength_div_factor=str_split_i(strength_unit , "(/)",2),
         strength_div_factor_val=str_split_i(strength_div_factor , "([A-Za-z]+)",1),
         strength_div_factor_val=ifelse(is.na(strength_div_factor_val),1 , strength_div_factor_val),
         strength_div_factor_unit=substr(strength_div_factor , nchar(strength_div_factor_val)+1, 50),   #strength unit
         pack_size_val=str_split_i(pack_size , "([A-Za-z]+)",1),   #packsize integer)
         pack_unit=substr(pack_size , nchar(pack_size_val)+1, 50))


#calculating the consumption (DiD)
amc <- amc_test %>% left_join(who_atc_ref, by='name_route') %>%
  filter(!is.na(`ATC code`)) %>%
  mutate(total=quantity*as.numeric(strength_val)*as.numeric(pack_size_val)/as.numeric(strength_div_factor_val),
         total_g=ifelse(strength_unit=='mg', total/1000,
                        ifelse(strength_unit=='g', total/1,
                               NA)),
         ddd_equivalent=total_g/as.numeric(DDD),
         did=ddd_equivalent*1000/365/pop,
         year=as.numeric(format(date,'%Y')),
         y_month=format(as.Date(date), "%Y-%m"),
         y_month_date=as.Date(as.yearmon(y_month))) %>%
  filter(ddd_equivalent>0)


##by molecule

amc_dir_molecule <- file.path("Results_AMC",cntry,'molecules')

if(!dir.exists(amc_dir_molecule)){dir.create(amc_dir_molecule, recursive = T)}


amc %>% group_by(year, `ATC level name`) %>%
  summarise(tot_did=sum(did),
            tot_ddd=sum(ddd_equivalent)) %>%
  ungroup() %>%
  arrange(desc(tot_did)) %>%
  #  mutate(did=tot_ddd*1000/365/pop) %>%
  group_by(year) %>%
  mutate(annual_totals=sum(tot_ddd),
         ddd_dist=round(tot_ddd*100/annual_totals,2),
         molecule=factor(`ATC level name`, levels=unique(.$`ATC level name`))) %>%
  arrange(desc(ddd_dist)) -> molecule_temp


amc_cats_molecule <- Reduce( rbind, list(molecule_temp %>% filter(ddd_dist >= 1),
                                         molecule_temp %>% filter(ddd_dist < 1) %>%   ##creating Others if less than 1%
                                           summarise(ddd_dist=sum(ddd_dist),
                                                     tot_did=sum(tot_did),
                                                     tot_ddd=sum(tot_ddd)) %>%
                                           mutate(molecule='Others')) ) %>%
  arrange(desc(ddd_dist)) %>%
  mutate( molecule=factor(molecule, levels=unique(.$molecule)))




for (y in unique(amc_cats_molecule$year)) {


  plt_molecule_dist <- ggplot(amc_cats_molecule %>% filter(year==y), aes(x=as.factor(year), y=ddd_dist, fill=molecule))+
    geom_bar(stat = 'identity', width = 0.5)+
    scale_fill_brewer(palette = "Set1") +
    labs(x='Year', y='DDD Distribution (%)')+
    theme_classic()+
    theme(legend.title = element_blank())

  ggsave(paste0(amc_dir_molecule,'/',y,'AMC_molecules_dist_DDD.png'),plt_molecule_dist, width=8, height=8, units="in", dpi=300)

  #next the barplots for did

  plt_molecule_tot <- ggplot(amc_cats_molecule%>% filter(year==y), aes(x=reorder(molecule,tot_did), y=tot_did))+
    geom_bar(stat = 'identity', fill='dodgerblue', width=.5)+
    labs(x='', y='DDD/1000 Inhabitants/day')+
    theme_classic()+
    theme(legend.title = element_blank())+
    coord_flip()
  ggsave(paste0(amc_dir_molecule,'/',y,'AMC_molecules_DID.png'),plt_molecule_tot, width=8, height=8, units="in", dpi=300)


}



#s_routees distribution

amc_dir_route <- file.path("Results_AMC",cntry,'routes')

if(!dir.exists(amc_dir_route)){dir.create(amc_dir_route, recursive = T)}



amc %>% group_by(year, route) %>%
  summarise(tot_did=sum(did),
            tot_ddd=sum(ddd_equivalent)) %>%
  ungroup() %>%
  arrange(desc(tot_did)) %>%
#  mutate(did=tot_ddd*1000/365/pop) %>%
group_by(year) %>%
  mutate(annual_totals=sum(tot_ddd),
         ddd_dist=round(tot_ddd*100/annual_totals,2),
         s_route=factor(route, levels=unique(.$route))) %>%
  arrange(desc(ddd_dist)) -> s_route_temp


amc_cats_s_route <- Reduce( rbind, list(s_route_temp %>% filter(ddd_dist >= 1),
                        s_route_temp %>% filter(ddd_dist < 1) %>%   ##creating Others if less than 1%
                                summarise(ddd_dist=sum(ddd_dist),
                                 tot_did=sum(tot_did),
                                 tot_ddd=sum(tot_ddd)) %>%
                          mutate(s_route='Others')) ) %>%
  arrange(desc(ddd_dist)) %>%
  mutate( s_route=factor(s_route, levels=unique(.$s_route)))




for (y in unique(amc_cats_s_route$year)) {


plt_s_route_dist <- ggplot(amc_cats_s_route %>% filter(year==y), aes(x=as.factor(year), y=ddd_dist, fill=s_route))+
  geom_bar(stat = 'identity', width = 0.5)+
  scale_fill_brewer(palette = "Set1") +
  labs(x='Year', y='DDD Distribution (%)')+
  theme_classic()+
  theme(legend.title = element_blank())

ggsave(paste0(amc_dir_route,'/',y,'AMC_s_routes_dist_DDD.png'),plt_s_route_dist, width=8, height=8, units="in", dpi=300)

#next the barplots for did

plt_s_route_tot <- ggplot(amc_cats_s_route%>% filter(year==y), aes(x=s_route, y=tot_did))+
  geom_bar(stat = 'identity', fill='dodgerblue', width=.5)+
  labs(x='', y='DDD/1000 Inhabitants/day')+
  theme_classic()+
  theme(legend.title = element_blank())
ggsave(paste0(amc_dir_route,'/',y,'AMC_s_routes_DID.png'),plt_s_route_tot, width=8, height=8, units="in", dpi=300)

}



##Consumption trend
amc_dir_trend <- file.path("Results_AMC",cntry,'Trend')

if(!dir.exists(amc_dir_trend)){dir.create(amc_dir_trend, recursive = T)}


amc_trend <- amc %>% group_by(y_month_date, Class, year) %>%
  summarise(tot_did=sum(did),
            tot_ddd=sum(ddd_equivalent)) %>%
  arrange(desc(tot_did))


#Visuals
#classes distribution
amc_dir_class <- file.path("Results_AMC",cntry,'class')

if(!dir.exists(amc_dir_class)){dir.create(amc_dir_class, recursive = T)}


amc %>% group_by(year, Class) %>%
  summarise(tot_did=sum(did),
            tot_ddd=sum(ddd_equivalent)) %>%
  ungroup() %>%
  arrange(desc(tot_did)) %>%
#  mutate(did=tot_ddd*1000/365/pop) %>%
group_by(year) %>%
  mutate(annual_totals=sum(tot_ddd),
         ddd_dist=round(tot_ddd*100/annual_totals,2),
         class=factor(Class, levels=unique(.$Class))) %>%
  arrange((ddd_dist)) %>%
  mutate( cum=cumsum(ddd_dist),
          midpoint=ddd_dist/2+lag(cum))-> class_temp


amc_cats_class <- Reduce( rbind, list(class_temp %>% filter(ddd_dist >= 1),
                        class_temp %>% filter(ddd_dist < 1) %>%   ##creating Others if less than 1%
                                summarise(ddd_dist=sum(ddd_dist),
                                 tot_did=sum(tot_did),
                                 tot_ddd=sum(tot_ddd)) %>%
                          mutate(class='Others',
                        midpoint=0)) ) %>%
  arrange(desc(ddd_dist)) %>%
  mutate( class=factor(class, levels=unique(.$class)))

#AWARe
amc_cats_aware <- amc %>% group_by(year, aware_cats) %>%
  summarise(tot_did=sum(did),
            tot_ddd=sum(ddd_equivalent)) %>%
  ungroup() %>%
  arrange(desc(tot_ddd)) %>%
  #  mutate(did=tot_ddd*1000/365/pop) %>%
  group_by(year) %>%
  mutate(annual_totals=sum(tot_ddd),
         dist=round(tot_ddd*100/annual_totals,2),
         aware_cats=factor(aware_cats, levels=unique(.$aware_cats))) %>%
  arrange(desc(dist))


for (y in unique(amc_cats_class$year)) {


plt_class_dist <- ggplot(amc_cats_class %>% filter(year==y), aes(x=as.factor(year), y=ddd_dist, fill=class))+
  geom_bar(stat = 'identity', width = 0.5)+
  geom_text_repel(aes(label=round(ddd_dist,1), y=midpoint), nudge_x = .3,
                 # size = 8,
                  direction = "y",
                  segment.size = .7,
                  segment.alpha = .5,
                  segment.linetype = "dotted",
                  box.padding = .4,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20)+

  scale_fill_brewer(palette = "Set1") +
  labs(x='Year', y='DDD Distribution (%)')+
  theme_classic()+
  theme(legend.title = element_blank())

ggsave(paste0(amc_dir_class,'/',y,'AMC_classes_dist.png'),plt_class_dist, width=8, height=8, units="in", dpi=300)

#next the barplots for did

plt_class_tot <- ggplot(amc_cats_class%>% filter(year==y), aes(x=reorder(class, tot_did), y=tot_did))+
  geom_bar(stat = 'identity', fill='dodgerblue', width=.5)+
  labs(x='', y='DDD/1000 Inhabitants/day')+
  theme_classic()+
  theme(legend.title = element_blank())+
  coord_flip()
ggsave(paste0(amc_dir_class,'/',y,'AMC_classes.png'),plt_class_tot, width=8, height=8, units="in", dpi=300)



plt_aware_dist <- ggplot(amc_cats_aware %>% drop_na(aware_cats)%>% filter(year==y), aes(x=as.factor(year), y=dist, fill=aware_cats))+
  geom_bar(stat = 'identity', width=0.5)+
  labs(x='Year', y='DDD Distribution (%)')+
  scale_fill_brewer(palette = "Set2") +
  theme_classic()+
  theme(legend.title = element_blank())

ggsave(paste0(amc_dir,'/',y,'AMC_aware.png'),plt_aware_dist, width=8, height=8, units="in", dpi=300)


plt_trend <- ggplot(amc_trend %>% filter(year==y), aes(x=y_month_date, y=log10(tot_ddd), color=Class))+
  geom_line()+
  labs(x='Year', y='DDDs (Log10)')+
  theme_classic()+
  theme(legend.title = element_blank(), legend.position = 'bottom')

ggsave(paste0(amc_dir_trend,'/',y,'AMC_trends.png'),plt_trend, width=12, height=8, units="in", dpi=300)

}





