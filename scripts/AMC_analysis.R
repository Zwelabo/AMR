library(readxl)
library(tidyverse)
library(AMR)
library(ggplot2)
library(zoo)

#importing the reference file
who_atc_ref <- read_excel('test-data/AMC/ATC-DDD WHO core and optional antimicrobials.xlsx') %>%
  filter(`Core/Optional`=='Core') %>%
  mutate(name_route=paste0(`ATC level name`, '_',tolower(Adm.R))) %>%
  rename(aware_cats=`2023 AWaRe categorization (for J01)`)

#importing the test dataset (focus is on non-combinational drugs)
amc_test <- read_excel('test-data/AMC/AMC_test_data.xlsx') %>%
    extract("product", c("product", "strength"), "(\\D*)(\\d.*)") %>%   #separating product names from strength
  extract("pack_size", c("pack_size_unit", "pack_size"), "(\\D*)(\\d.*)") %>%   #separating product names from strength
  mutate(route=ifelse(str_detect(tolower(route), 'oral'),'o',           #administration routes
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

pop=20000000  #hypothetical population estimate


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



#Visuals
#classes distribution
amc_cats_b <- amc %>% group_by(year, Class) %>%
  summarise(tot_did=sum(did)) %>%
  ungroup() %>%
  arrange(desc(tot_did)) %>%
#  mutate(did=tot_ddd*1000/365/pop) %>%
group_by(year) %>%
  mutate(annual_totals=sum(tot_did),
         dist=round(tot_did*100/annual_totals,2),
         class=factor(Class, levels=unique(.$Class))) %>%
  arrange(desc(dist))


plt_class_dist <- ggplot(amc_cats_b, aes(x=year, y=dist, fill=class))+
  geom_bar(stat = 'identity')+
  labs(x='Year', y='DiD (%)')+
  theme_classic()+
  theme(legend.title = element_blank())

ggsave(paste0('plots_AMC/AMC_classes_dist.png'),plt_class_dist, width=8, height=8, units="in", dpi=300)


plt_class_tot <- ggplot(amc_cats_b, aes(x=year, y=tot_did, fill=class))+
  geom_bar(stat = 'identity')+
  labs(x='Year', y='DiD')+
  theme_classic()+
  theme(legend.title = element_blank())
ggsave(paste0('plots_AMC/AMC_classes.png'),plt_class_tot, width=8, height=8, units="in", dpi=300)


#AWARe
amc_cats_aware <- amc %>% group_by(year, aware_cats) %>%
  summarise(tot_did=sum(did)) %>%
  ungroup() %>%
  arrange(desc(tot_did)) %>%
  #  mutate(did=tot_ddd*1000/365/pop) %>%
  group_by(year) %>%
  mutate(annual_totals=sum(tot_did),
         dist=round(tot_did*100/annual_totals,2),
         aware_cats=factor(aware_cats, levels=unique(.$aware_cats))) %>%
  arrange(desc(dist))


plt_aware_dist <- ggplot(amc_cats_aware %>% drop_na(aware_cats), aes(x=year, y=dist, fill=aware_cats))+
  geom_bar(stat = 'identity')+
  labs(x='Year', y='DiD (%)')+
  theme_classic()+
  theme(legend.title = element_blank())

ggsave(paste0('plots_AMC/AMC_aware.png'),plt_aware_dist, width=8, height=8, units="in", dpi=300)


##Consumption trend
amc_trend <- amc %>% group_by(y_month_date, Class) %>%
  summarise(tot_did=sum(did)) %>%
  ungroup() %>%
  arrange(desc(tot_did))

plt_trend <- ggplot(amc_trend, aes(x=y_month_date, y=tot_did, color=Class))+
  geom_line()+
  labs(x='Year', y='DiD')+
  theme_classic()+
  theme(legend.title = element_blank(), legend.position = 'bottom')

ggsave(paste0('plots_AMC/AMC_trends_holder.png'),plt_trend, width=12, height=8, units="in", dpi=300)

