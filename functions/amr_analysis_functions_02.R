get_inputs_and_prelim_cleanup <- function(){

  # Load AMR data sources ---------------------------------------------------


  input_file <- list.files("test-data", pattern = "^AMR.*.xlsx")

  amr <- readxl::read_excel(file.path("test-data",input_file)) %>%

    dplyr::mutate(rid=row_number()) # assign distinct RIDs

  # Replace missing dates with dates e.g. from registration date col --------
  excel_origin = "1899-12-30"
  date_parse_vec = c("ymd HMS", "ymd HM", "mdy HMS", "dmy HMS", "ymd", "dmy", "mdy")

  amr <- amr %>%
    mutate(
      Specimen_date_new = coalesce(`Specimen date`, `Date of data entry`)
    ) %>%
    mutate(numeric_value = as.numeric(Specimen_date_new)) %>%
    mutate(New_date_posixct = ifelse(is.na(numeric_value),
                                     parse_date_time(Specimen_date_new, orders = date_parse_vec),
                                     as.POSIXct(numeric_value * 24 * 3600, origin = excel_origin, tz = "UTC"))
    ) %>%
    mutate(specimen_date_cleaned = as.Date(as.POSIXct(New_date_posixct, origin = "1970-01-01", tz = "UTC")))

  # Specify mandatory columns -----------------------------------------------

  man_cols = c("specimen_date_cleaned", # data specimen collected

               "Specimen type" , # type of specimemn

               "Organism") # organism identified

  # Remove columns with NA values in any of the mandatory columns

  amr <- amr %>%

    filter(if_all(all_of(man_cols), ~ !is.na(.)))

  return(amr)

}



get_demographics <- function(df){
  # get demographics

  demo_vec <- c("rid","Identification number","First name","Last name",

                "Sex","Date of birth","Age","Age category","Date of admission","Reason")

  lkp_demographics <- df %>%

    dplyr::select(any_of(demo_vec))

  return(lkp_demographics)
}



get_facilities_data <- function(df){

  # get lab and facility information

  facility_vec <- c("rid","Identification number","Laboratory","Institution",

                    "Location","Location type","Department","Origin","Country")

  lkp_facility <- df %>%

    dplyr::select(any_of(facility_vec)) %>%

    dplyr::select(where(~ !all(is.na(.))))  # remove columns with no values

  return(lkp_facility)
}


get_specimen_info <- function(df){
  # get specimens info

  specimen_vec <- c("rid","Identification number","specimen_date_cleaned","Specimen number","Specimen type","Specimen type (Numeric)")

  lkp_specimens <-  df %>%

    dplyr::select(any_of(specimen_vec))

  return(lkp_specimens)

}



get_test_results <- function(df){

  # get test results

  abx_vec <- c("AMK_ND30","AMP_ND10", "AZM_ND15", "FEP_ND30", "CFM_ND5",

               "CTX_ND30","FOX_ND30", "CAZ_ND30", "CRO_ND30", "CIP_ND5",

               "COL_ND10","DOR_ND10", "ETP_ND10", "GEN_ND10", "IPM_ND10",

               "LVX_ND5", "MEM_ND10", "MNO_ND30", "OXA_ND1", "PEN_ND10",

               "SPT_ND100","TGC_ND15","SXT_ND1.2","AMK_NM","AMP_NM","AZM_NM",

               "FEP_NM","CFM_NM","CTX_NM","FOX_NM","CAZ_NM","CRO_NM","CIP_NM",

               "COL_NM","DOR_NM","ETP_NM","GEN_NM","IPM_NM","LVX_NM","MEM_NM",

               "MNO_NM","OXA_NM","PEN_NM","SPT_NM","TGC_NM","SXT_NM","CTX_NE",

               "CRO_NE","PEN_NE") #the list is currently not exhaustive (note_to self: add the ab_code list as a vector)

  # map antibiotics

  abx_conformed <- as.ab(abx_vec)

  # get antimicrobial results
  main_vars <- c("rid","Identification number",man_cols)  ##am adding specimen date here
  amr_res <- df %>%
    dplyr::select(any_of(c(main_vars,abx_vec)))


  p1 <- which(names(amr_res) == "Identification number")
  p2 <- which(names(amr_res) == "Organism")
  p3 <- which(names(amr_res) == "Specimen type")
  p4 <- which(names(amr_res) =="specimen_date_cleaned")

  names(amr_res)[p1] <- "uid"
  names(amr_res)[p2] <- "organism"
  names(amr_res)[p3] <- "specimen_type"
  names(amr_res)[p4] <- "specimen_date"



  amr_res <- amr_res %>%
    drop_na(organism, specimen_type,specimen_date )


  return(amr_res)

}



pivot_abx_results <- function(df){
  # Abx results preparation -------------------------------------------------

  amr_res <- df %>%

    mutate(bacteria = as.mo(organism, info = TRUE)) %>%

    mutate(gramstain = mo_gramstain(bacteria)) %>%

    dplyr::select(rid, uid, specimen_type,bacteria, organism, gramstain,everything()) %>%

    mutate(across(all_of(abx_vec), ~as.character(.)))            #watch out for the uninterpretable bacteria and antibiotics

  mo_failures()

  mo_uncertainties()

  # pivot drugs from wide into long format and map to standard drug codes

  famr_long <- amr_res %>%

    tidyr::pivot_longer(names_to = "drug_code",

                        cols = any_of(abx_vec),

                        values_to = "vals") %>%

    mutate(ab = as.ab(drug_code))

  return(famr_long)

}


get_sir_interpr <- function(df){
  # Drugs with SIR interpretations already

  famr_long_sir <- df %>%

    mutate(int_id=row_number()) %>%

    dplyr::filter(str_detect(vals,'R|I|S|SDD|NI')) %>%

    mutate(test_type=ifelse(grepl('_NM|_EM', drug_code), 'mic','disk'),

           # Use Gilbert's logic to determine if DISK or MIC

           guideline=ifelse(grepl('_N', drug_code), 'CLSI',

                            ifelse(grepl('_E', drug_code), 'EUCAST', 'CLSI')),

           interpreted_res='',                    #to hold results for the next part

           intrinsic_res_status=''

    )

  return(famr_long_sir)
}



get_con_interp <- function(df){
  # Drugs with breakpoints (MIC or DISK)

  famr_long_con <- df %>%

    mutate(int_id=row_number()) %>%

    dplyr::filter(!str_detect(vals,'R|I|S|SDD|NI')) %>%

    # Use Gilbert's logic to determine if DISK or MIC

    mutate(test_type=ifelse(grepl('_NM|_EM', drug_code), 'mic','disk'),

           guideline=ifelse(grepl('_N', drug_code), 'CLSI',

                            ifelse(grepl('_E', drug_code), 'EUCAST', 'CLSI')),

           interpreted_res='',                    #to hold results for the next part

           intrinsic_res_status=''

    )

  return(famr_long_con)
}
