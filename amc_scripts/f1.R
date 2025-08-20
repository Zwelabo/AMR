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


# Match column classes of df2 to df1 and allow missing columns
match_col_classes <- function(df1, df2) {
  # Add missing columns to df2
  missing_cols <- setdiff(names(df1), names(df2))
  for (col in missing_cols) {
    class_type <- class(df1[[col]])[1]
    df2[[col]] <- if (class_type == "numeric") NA_real_ else
      if (class_type == "integer") NA_integer_ else
        if (class_type == "character") NA_character_ else
          if (class_type == "factor") factor(NA, levels = levels(df1[[col]])) else
            NA
  }

  # Reorder columns to match df1
  df2 <- df2[, names(df1), drop = FALSE]

  # Match classes
  df2_matched <- map2_dfc(
    df2,
    df1,
    ~ {
      target_class <- class(.y)[1]
      if (target_class == "numeric") as.numeric(.x) else
        if (target_class == "integer") as.integer(.x) else
          if (target_class == "character") as.character(.x) else
            if (target_class == "factor") factor(.x, levels = levels(.y)) else
              .x
    }
  )
  names(df2_matched) <- names(df1)
  df2_matched
}
#
bind_rows_match_classes <- function(dfs) {
  Reduce(function(x, y) bind_rows(x, match_col_classes(x, y)), dfs)
}

#create a folder to hold the temporary files
#create the results directory
amc_updates_dir <- file.path(folder_path, "analysis_updates")

if(!dir.exists(amc_updates_dir)){dir.create(amc_updates_dir, recursive = T)}
