who_atc_ref <- read_excel('test-data/AMC/ATC-DDD WHO core and optional antimicrobials.xlsx') %>%
  filter(`Core or Optional`=='Core') %>%
  mutate(name_route=paste0(`ATC level name`, '_',tolower(Adm.R))) %>%
  rename(aware_cats=`2023 AWaRe categorization (for J01)`,
         product= tolower(`ATC level name` ))

single_atc_refs <- who_atc_ref %>%
  mutate(ab_length= str_count(product, ' ')) %>%
  filter(ab_length<1)

comb_atc_refs <- who_atc_ref %>%
  mutate(ab_length= str_count(product, ' ')) %>%
  filter(ab_length>0 &!is.na(DDD))

#importing the test dataset (focus is on non-combinational drugs)
amc_test <- read_excel('test-data/AMC/AMC_test_data.xlsx') %>%
  extract("product", c("product_char", "strength"), "(\\D*)(\\d.*)") %>%    #separating the names from strength
  mutate(product_char=trimws(tolower(product_char))) %>%
  mutate(product_char2=trimws(str_replace(product_char,' caps|cap | syrup| tabs| syr|syr |tabs | tab|tab | injection| inj|inj | suspension| im| iv', ''))) %>%
  mutate(str_pr2= str_count(product_char2, ' '))%>%
  mutate(product_char3=ifelse(str_pr2==0, ab_name(product_char2), ''))

##Subseting names that are in the reference database
part1a <- amc_test %>% filter(product_char %in% who_atc_ref$product)

part2 <- amc_test %>%
  anti_join (part1a)

part2a <- part2%>%
 # mutate(product_char2=trimws(str_replace(product_char,'caps| syrup| tabs| syr|syr |tabs | tab|tab ', '')))%>%
  filter(product_char2 %in% who_atc_ref$product)

part3 <- part2 %>%
  anti_join (part2a)

part3a <- part3%>%
  # mutate(product_char3=ifelse(str_pr2==0, ab_name(product_char2), ''))%>%
  filter(str_pr2 == 0) %>%
  filter(tolower(product_char3) %in% who_atc_ref$product)


part4 <- part3 %>%
  anti_join (part3a)



sort(unique(part3$product_char))

sort(unique(part4$product_char2))

ab_name("co amoxiclav"  )




#ADD A SOLVED NAME COLUMN
