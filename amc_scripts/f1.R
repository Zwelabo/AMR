#amc_prep
##adding folder_path

##load the packages
source('scripts/install_packages_packman.R')

if (rstudioapi::isAvailable()) {
  folder_path <- rstudioapi::selectDirectory()
  print(folder_path)
} else {
  #  cat("Not running in RStudio.\n")
  folder_path=NA
}


input_file <- list.files(folder_path, pattern = "^AMC.*.xlsx")

amc_raw <- readxl::read_excel(file.path(folder_path,input_file))

# Replace missing dates with dates e.g. from registration date col --------
excel_origin = "1899-12-30"
date_parse_vec = c("ymd HMS", "ymd HM", "mdy HMS", "dmy HMS", "ymd", "dmy", "mdy")


###matching the cols
# Sample vector
choices1 <- c(names(amc_raw),'not available')  # Use your real variable


#prelim required columns
cols <- c("region", "product", "strength", "pack_size", "quantity", "date", "route")

empty_amc_df <- data.frame(Required_variables=cols,
                           Corresponding_variables=c(rep('',7)),
                           Country=c(rep('',7)),
                           Population=c(rep('',7)))


#create a folder to hold the temporary files
#create the results directory
amc_updates_dir <- file.path(folder_path, "analysis_updates")

if(!dir.exists(amc_updates_dir)){dir.create(amc_updates_dir, recursive = T)}
