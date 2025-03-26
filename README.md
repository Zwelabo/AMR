# AMR analysis script- MAAP2 (ASLM)


## Introduction

These scripts have been developed to simplify and standardize AMR/C/U data analysis through ASLM as part of the MAAP Phase II project funded through Fleming Fund. These scripts are largely based on the free and open-source [AMR package](https://msberends.github.io/AMR/index.html) implemented in R.

## Set-up and software installation 

First, you will need to download and install the R software and the latest free version of RStudio. To install R;

1.  Navigate to [cran.r-project.org](https://cran.r-project.org/bin/windows/base/), and
2.  If you are on a Windows machine, click Download R-4.4.3 for Windows. This will download the installer to your Downloads folder.
3.  Once the download has been completed, click the installer to install R on your computer and follow the prompts without making any changes.

Next, install RStudio by navigating to the [RStudio desktop download page](https://posit.co/download/rstudio-desktop/). Click the blue tab under Install RStudio and click the installer to begin the installation once the download has completed.

## Running Code

Clone the Country-specific MAAP Data Analysis repository from GitHub

1.  Download GitHUb Desktop from the following link: [GitHub-Desktop download page](https://desktop.github.com/download/), by clicking the **Download for Windows** tab.

    Follow the installation instructions and open the application once installation has completed successfully.

2.   Go to File -\> Clone repository -\> URL and then paste the following link `https://github.com/ASLM-Fabebe/MAAP-Data-Analysis.git` in the first box and

3.  Click Clone

4.  In RStudio go to File -\> New Project -\> Existing Project -\> Browse (Navigate to your Documents -\> GitHub -\> MAAP-Data-Analysis)

5.  Click MAAP-Data-Analysis.Rproj

    This will open the MAAP-Data-Analysis scripts within RStudio on your local computer.

## Load packages

1.  Within RStudio in the bottom right pane, click on Files -\> scripts folder -\> install_packages_pacman.R (This will open the script in the top-right pane)
2.  Select all (Ctrl-A) and click Run (This will install all the requisite packages and prepare your environment for the analysis)

Alternatively;

1.  Click the main analysis script (In the bottom-right pane, click Files -\> scripts folder -\> amr_data_cleaning_and_sir_interpretation.R), and
2.  Run the first line under "Load packages"

## Input Data

-   Add your input data to the test-data folder. For now the script only accepts input in Excel format. Importantly, the file with AST data should have "AMR" as its prefix.

-    Now you can attempt to run the analysis script.

-   If everything runs successfully, you should have the following in the results folder:
      - 3  look-up tables (metadata files)
      - The test result file (interpreted AST results), with AST interpretations
      - An antibiogram of the tested bug-drug combinations
      - CSV files of E. coli resistance distribution by Age, gender, and specimen type

-   In the plots folder:
      - Barplots of the resistance prevalence of E. coli

    *Please note that only E. coli output is currently shown but resistance to all other pathogens can be obtained by substituting accordingly*.

### Loading Packages

```{r}
source(file.path("scripts","install_packages_packman.R"))
```

### Create results folder and set the date

```{r}
res_dir <- file.path("Results")

if (!dir.exists(res_dir)){

  dir.create(res_dir, recursive = T)

}

date_var <- as.Date(date(), format = "%a %b %d %H:%M:%S %Y")
```

### Load functions 

```{r}
source(file.path("functions","amr_analysis_functions_01.R"))
```

### Load data sources and preprocess to remove NAs

```{r}
input_file <- list.files("test-data", pattern = "^AMR.*.xlsx")

amr <- readxl::read_excel(file.path("test-data",input_file)) %>%

  dplyr::mutate(rid=row_number()) # assign distinct RIDs
```

### Specify mandatory columns i.e. columns that should not be NULL/NA

```{r}
man_cols = c("Specimen date", # data specimen collected

             "Specimen type" , # type of specimemn

             "Organism") # organism identified

# Remove columns with NA values in any of the mandatory columns

amr <- amr %>%

  filter(if_all(all_of(man_cols), ~ !is.na(.)))
```

### Split demographics and results 

```{r}
# get demographics

demo_vec <- c("rid","Identification number","First name","Last name",

  "Sex","Date of birth","Age","Age category","Date of admission","Reason")

lkp_demographics <- amr %>%

  dplyr::select(any_of(demo_vec))

# get lab and facility information

facility_vec <- c("rid","Identification number","Laboratory","Institution",

  "Location","Location type","Department","Origin","Country")

lkp_facility <- amr %>%

  dplyr::select(any_of(facility_vec)) %>%

  dplyr::select(where(~ !all(is.na(.))))  # remove columns with no values

# get specimens info

specimen_vec <- c("rid","Identification number","Specimen date","Specimen number","Specimen type","Specimen type (Numeric)")

lkp_specimens <-  amr %>%

  dplyr::select(any_of(specimen_vec))

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
amr_res <- amr %>%
  dplyr::select(any_of(c(main_vars,abx_vec)))


p1 <- which(names(amr_res) == "Identification number")
p2 <- which(names(amr_res) == "Organism")
p3 <- which(names(amr_res) == "Specimen type")
p4 <- which(names(amr_res) =="Specimen date")

names(amr_res)[p1] <- "uid"
names(amr_res)[p2] <- "organism"
names(amr_res)[p3] <- "specimen_type"
names(amr_res)[p4] <- "specimen_date"



amr_res <- amr_res %>%
  drop_na(organism, specimen_type,specimen_date )
```

### Antibiotic results preparation

```{r}
amr_res <- amr_res %>%

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

# separate breakpoints and SIR interpretations

# Drugs with SIR interpretations already

famr_long_sir <- famr_long %>%

  mutate(int_id=row_number()) %>%

  dplyr::filter(str_detect(vals,'R|I|S|SDD|NI')) %>%

  mutate(test_type=ifelse(grepl('_NM|_EM', drug_code), 'mic','disk'),

         # Use Gilbert's logic to determine if DISK or MIC

         guideline=ifelse(grepl('_N', drug_code), 'CLSI',

                          ifelse(grepl('_E', drug_code), 'EUCAST', 'CLSI')),

         interpreted_res='',                    #to hold results for the next part

         intrinsic_res_status=''

         )

# Drugs with breakpoints (MIC or DISK)

famr_long_con <- famr_long %>%

  mutate(int_id=row_number()) %>%

  dplyr::filter(!str_detect(vals,'R|I|S|SDD|NI')) %>%

  # Use Gilbert's logic to determine if DISK or MIC

  mutate(test_type=ifelse(grepl('_NM|_EM', drug_code), 'mic','disk'),

         guideline=ifelse(grepl('_N', drug_code), 'CLSI',

                          ifelse(grepl('_E', drug_code), 'EUCAST', 'CLSI')),

         interpreted_res='',                    #to hold results for the next part

         intrinsic_res_status=''

         )
```

### Convert breakpoints to SIR

```{r}
amr_con <- convert2sir_fun(famr_long_con)

amr_sir <- convert2sir_fun(famr_long_sir)

# combine results

sir_outcomes_df <- amr_con %>%

  dplyr::union(amr_sir) %>%

  dplyr::filter(intrinsic_res_status=='FALSE') %>%   #drop the intrinsically resistant bug-drugs to not skew results

  dplyr::filter(interpreted_res!='NA')  #drop the UNINTERPRETABLE COMBOS FROM GUIDELINES

excluded_rec <- amr_con %>%

  dplyr::union(amr_sir) %>%

  dplyr::filter(intrinsic_res_status=='TRUE'| interpreted_res=='NA')

# get organism full names

lkp_organisms <- AMR::microorganisms %>% dplyr::select(mo,fullname)  #puls the entire list

# pivot wide

sir_outcomes_df_wide <- sir_outcomes_df %>%

  dplyr::select(-c(drug_code,int_id, vals, intrinsic_res_status)) %>%

  pivot_wider(names_from = "ab",

              values_from = "interpreted_res") %>%

  left_join(lkp_organisms, by=join_by("bacteria"=="mo")) %>%

  dplyr::select(rid,uid,specimen_type,mo_organism=fullname,

                gramstain,test_type,guideline, everything())
```

### Start the downstream analysis

Please note Rates are only shown when n \>=30 according to GLASS reccommendations

```{r}
#create an analysis dataframe

an_df <- sir_outcomes_df_wide %>%
  left_join(lkp_demographics %>% select(Age, Sex,rid), by='rid') %>%
  ##cleanung Age cols
  mutate(Age_s=toupper(gsub("[[:digit:]]", "", Age)),
         Age_n= as.numeric(gsub("[^0-9.-]", "", Age)),
         Age_conv=ifelse(Age_s=='D',round(Age_n/365,0),
                         ifelse(Age_s=='W',round(Age_n/7,0),
                                ifelse(Age_s=='M',round(Age_n/12,0),
                                       ifelse(Age_s=='Y',round(Age_n/1,0),Age)))),
         Age_conv=as.numeric(Age_conv),
         Age_g=as.character(age_groups(Age_conv, split_at = c(1,5,20,50,65))),
         Age_g=ifelse(Age_g=='0', '<1',Age_g),
         Age_g=factor(Age_g, levels=c('<1','1-4','5-19', '20-49','50-64','65+')))
```

### Define parameters for subgroup analyses

```{r}
par_df <- tibble(param=c('Age', 'Sex', 'Specimen Type'), var_name=c('Age_g', 'Sex', 'specimen_type')) %>%

  mutate(id=paste0(param,var_name))

cntry='TZ'  #please insert your country name
```

Add/change pathogen and antibiotics of interests to obtain prevalance rates and plots from the 2nd function (amr_grp1_analysis)

```{r}
for (i in par_df$id) {

  par=par_df$param[par_df$id==i]

  par_var_name=par_df$var_name[par_df$id==i]

 
 #E. COLI
amr_grp1_analysis(

  cntry = cntry,

  par=par,

  par_var_name=par_var_name,

org_name='Escherichia coli',

abs_ref <- c("AMP", "CFR", "CTX", "CAZ", "CIP", "GEN", "TOB", "MEC", "MEM", "NIT", "TZP", "TMP", "SXT")

)

#other bug-drug combinations to follow

}


```

check outputs in the results and plots folders respectively

### National antibiogram

Generate an antibiogram for all/subset of the tested bug-drug combinations

```{r}
abg_df <- an_df %>%

  #filter(mo_organism==org_name) %>%

  mutate_if(is_sir_eligible, as.sir) %>%

  antibiogram()
```

### More to follow...
