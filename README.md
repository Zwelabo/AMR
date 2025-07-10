# Introduction

These scripts have been developed to simplify and standardize AMR/C/U data analysis through ASLM as part of the MAAP Phase II project funded through Fleming Fund. These scripts are largely based on the free and open-source [AMR package](https://msberends.github.io/AMR/index.html) implemented in R.

# Set A: AMR analysis script- MAAP2 (ASLM)

## Set-up and software installation 

First, you will need to download and install the R software and the latest free version of RStudio. To install R;

1.  Navigate to [cran.r-project.org](https://cran.r-project.org/bin/windows/base/), and
2.  If you are on a Windows machine, click Download R-4.4.3 for Windows. This will download the installer to your Downloads folder.
3.  Once the download has been completed, click the installer to install R on your computer and follow the prompts without making any changes.

Next, install RStudio by navigating to the [RStudio desktop download page](https://posit.co/download/rstudio-desktop/). Click the blue tab under Install RStudio and click the installer to begin the installation once the download has completed.

## Running Code

Clone the Country-specific MAAP Data Analysis repository from GitHub

1.  Download GitHUb Desktop from the following link: [GitHub-Desktop download page](https://desktop.github.com/download/), by clicking the **Download for Windows or Mac** tab.

    Follow the installation instructions and open the application once installation has completed successfully.

2.   Go to `File` -\> `Clone repository` -\> URL and then paste the following link `https://github.com/ASLM-Fabebe/MAAP-Data-Analysis.git` in the first box and

3.  Click Clone

4.  In RStudio go to `File` -\> `New Project` -\> `Existing Project` -\> Browse (Navigate to your Documents -\> GitHub -\> MAAP-Data-Analysis)

5.  Click MAAP-Data-Analysis.Rproj

    This will open the MAAP-Data-Analysis scripts within RStudio on your local computer.
6. Click on `File` -\> `New File` -\> `R Script`
   Alternatively just press `Ctrl+Shift+N` to open a new R Script file

   *You can copy and paste the commands in `Step 1 - 4` below to perform your analysis*


## Input Data

-   Add your input data to the `test-data` folder. For now the script only accepts input in Excel format. Importantly, the file with AST data should have "AMR" as its prefix.

-    Now you can attempt to run the analysis script.

-   You can specify your organisms of interest by adding them on `Step 2` below:


## Step 1: Load packages

1.  Within RStudio in the bottom right pane, click on Files -\> scripts folder -\> install_packages_pacman.R (This will open the script in the top-right pane)
2.  Select all (Ctrl-A) and click Run (This will install all the requisite packages and prepare your environment for the analysis)

Alternatively;

1.  Click the main analysis script (In the bottom-right pane, click Files -\> scripts folder -\> amr_data_cleaning_and_sir_interpretation.R), and
2.  Run the first line under "Load packages"
     

### Loading Packages

```{r}
source(file.path("scripts","install_packages_packman.R"))
```

## Step 2: Provide your pathogens of interest by adding them to the vector below and execute

```{r}
# Specify organisms of interest -------------------------------------------

eskape_pathogens <- c(
  "Enterococcus faecium",
  "Staphylococcus aureus",
  "Klebsiella pneumoniae",
  "Acinetobacter baumannii",
  "Pseudomonas aeruginosa",
  "Escherichia coli",
  "Enterobacter cloacae complex", 
  "Enterobacter aerogenes", 
  "Enterobacter hormaechei" 
)
```
*By default the list only includes ESKAPE pathogens*

## Step 3: Specify your country code 

```{r}
# Insert your country code ------------------------------------------------

cntry='TZ' # e.g. TZ for Tanzania
```

## Step 4: Execute the main script - this will perform the analysis end-to-end

```{r}
source(file.path("scripts","run_step_01.R"))
```

-   If everything runs successfully, you should have the following in the results folder:
      - 4 tables based on the provided input file
        - Demographics
        - Facilities information
        - Generic organisms list
        - The test result file (interpreted AST results), with cleaned and standardized AST interpretations
      - An antibiogram of the tested bug-drug combinations
      - Sub-folders of analyzed pathogens e.g. ESKAPE pathogens, with each folder named after the organism analyzed, the results in these folders include:
        - CSV files of Resistance distribution by Age, gender, and specimen type
        - Barplots of the resistance prevalence of ESKAPE pathogens
 


# Set B: AMC analysis script- MAAP2 (ASLM)
This R script processes and analyzes antimicrobial consumption (AMC) data to calculate Defined Daily Doses (DDD) and DDD per 1,000 inhabitants per day (DiD), with visualization of trends and patterns. Please follow the Steps 1 to 4 below to be able to utilize the scripts develop.

## Step 1: Set-Up and Software Installation
To get started, install the required software:
- Install R: Visit [cran.r-project.org](https://cran.r-project.org/bin/windows/base/) and download the appropriate version for your system (e.g., Download R-4.4.3 for Windows or Mac).
- Install RStudio: Go to the [RStudio desktop download page](https://posit.co/download/rstudio-desktop/) and download the latest version. Click the installer to begin installation once downloaded.
- Install GitHub Desktop: Navigate to the [GitHub-Desktop download page](https://desktop.github.com/download/) and click Download for Windows or Mac. Follow the installation instructions and open the application after successful installation.

## Step 2: Clone the Repository
If you haven't already, clone the repository to your local machine using GitHub Desktop. Follow the steps up in the AMR module (Set A). The repository contains scripts for AMR analysis, including AMC & AMU processing and plotting.

## Step 3: Upload Input Data
Place your input datasets into the test-data/AMC/ folder. Two datasets are required:

1. Reference Data
- Format: .xlsx (Excel)
- File name: ATC-DDD WHO core and optional antimicrobials.xlsx
- Description: Core and optional antimicrobial reference list from WHO.

2. AMC Test Data
- Format: .xlsx (Excel)
- File name: AMC_test_data.xlsx (only for demo)
- Description: Country-specific antimicrobial consumption data, including product names, strengths, routes, and quantities.

### Important: Keep the file names exactly as specified above unless otherwise advised & modified.

## Step 4: Run the Analysis
To run the analysis in RStudio:
- Open RStudio and go to: File → New File → R Script
- Copy and paste the single line of command below into the new script:

```{r}
{ source(file.path("scripts", "run_amc_analysis.R")); run_amc_analysis() }
```

- Run the script. You’ll be prompted to enter your country’s estimated population. The script will then execute an end-to-end analysis automatically.

## Output
If the script runs successfully, the following visualizations will be generated and saved in the plots_AMC folder:
- AMC Class Distribution (percentage)
- AMC Class Totals
- AWaRe Category Distribution
- Monthly Consumption Trends

## Usage
1. Set the population size when prompted
2. Ensure input files are in the correct folder (`test-data/AMC/`)
3. Run the script to generate:
   - Processed data frame (`amc`) with DiD calculations
   - Visualizations in `plots_AMC/` directory

# Set C: AMU analysis script- MAAP2 (ASLM)
This script analyzes Antimicrobial Use (AMU) data from healthcare facilities, producing:
- Demographic summaries (age/gender distributions)
- AMU prevalence metrics (overall, by ward/age/gender)
- Antimicrobial utilization patterns (by class/molecule/route/AWaRe category)

