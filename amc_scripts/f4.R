##

cat("Initializing updates on the dataset\n")
message("Initializing updates on the dataset\n")


#read in the user-validated file
file_path <- file.path(amc_updates_dir,'DDD_information_updates.xlsx')

if (file.exists(file_path)) {
  ddd_updates <- read.xlsx(file_path)%>% rename(`ATC level name`=ATC.level.name)  # or read.csv, fread, etc.
} else {
  ddd_updates=ddd_updates
  message("File not found — ddd_info_updates")
}

ddd_updates=ddd_updates %>% mutate(name_route=paste0(`ATC level name`, '_',tolower(Adm.R)))




ddd_ref_update <- bind_rows_match_classes(list(ddd_ref,ddd_updates %>% filter(DDD!=' ')))




#updating the atcs cleaned file
writexl::write_xlsx(ddd_ref_update, 'amc_resources/ab_molecules_amc.xlsx')


#incorporating the added ddd info
amc_dataset_comb2 <- amc_dataset_comb_r %>%  filter(name_route %in% ddd_ref$name_route)

writexl::write_xlsx(amc_dataset_comb2, paste0(amc_updates_dir,'/unused_amc_data.xlsx'))
#after editing DDDs, use this; amc_dataset_comb2

cat(paste("Done... uncleaned data stored", (length(unique(amc_dataset_comb2$uid))), 'records\n'))
message(paste("Done... uncleaned data stored", (length(unique(amc_dataset_comb2$uid))), 'records'))
