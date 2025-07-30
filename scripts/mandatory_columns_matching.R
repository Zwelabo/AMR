#load the packages
source('scripts/install_packages_packman.R')

##adding folder_path

if (rstudioapi::isAvailable()) {
  folder_path <- rstudioapi::selectDirectory()
  print(folder_path)
} else {
  #  cat("Not running in RStudio.\n")
  folder_path=NA
}

###comeback later
# Check if folder_path is NA
# if (is.na(folder_path)) {
#
#   # Load tcltk package
#   if (!requireNamespace("tcltk", quietly = TRUE)) {
#     install.packages("tcltk")
#   }
#   library(tcltk)
#
#   # Prompt user to select a folder
#   folder_path <- tk_choose.dir(caption = "Select folder with your data files")
#
#   # Check what they selected
#   print(folder_path)
#
# } else {
#   # Do nothing
# }


input_file <- list.files(folder_path, pattern = "^AMR.*.xlsx")

amr <- readxl::read_excel(file.path(folder_path,input_file))

# Replace missing dates with dates e.g. from registration date col --------
excel_origin = "1899-12-30"
date_parse_vec = c("ymd HMS", "ymd HM", "mdy HMS", "dmy HMS", "ymd", "dmy", "mdy")


###matching the cols
# Sample vector
choices <- c(names(amr),'not available')  # Use your real variable

# Define initial df
initial_df <- data.frame(
  man_vars = c('Specimen date', 'Date of data entry', 'Specimen type', "Organism", 'Age',
               'Sex', "Identification number"),
  my_dataset = factor(rep('please select', 7), levels = choices),
  enter_country_name_or_code='',
  stringsAsFactors = FALSE
)


source('scripts/columns_match_app.R')
match_app()


