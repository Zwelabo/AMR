# AMR/U/C Data Analysis

The Antimicrobial Resistance (AMR), Antimicrobial Consumption (AMC), and Antimicrobial Use (AMU) Data Standardization Project aims to enhance data-driven decision-making by providing key stakeholders with standardized methodologies for data collection, analysis, and reporting. This initiative supports national surveillance systems in improving data quality, comparability, and utilization to inform evidence-based policies and interventions.

## Table of Contents
- [Preliminary Steps for AMR/U/C Data Analysis](#preliminary_steps)
- [AMR Surveillance Data Analysis Plan](#amr_analysis)
- [AMC Data Analysis Plan](#amc_analysis)
- [AMU Data Analysis Plan](#amu_analysis)
- [AMR/U/C Data Archive and Protection](#data_archive)
- [Data Presentation](#data_presentation)
- [Online AMR Data Sources](#Online_data)
  
### Our World in Data: AMR Surveillance Systems Map Automation
This section contains an R script ([oneworld.R](https://github.com/ASLM-Fabebe/MAAP-Data-Analysis/blob/main/oneworld.R)) that automates the process of downloading, processing, and visualizing data on Antimicrobial Resistance (AMR) surveillance systems in FF Supported countries. The script generates an interactive Shiny app with a map visualization.

#### Overview
The script performs the following steps:
1. **Downloads Data**: Fetches the latest AMR surveillance data from [Our World in Data](https://ourworldindata.org).
2. **Processes Data**: Cleans and prepares the data for visualization.
3. **Generates Map**: Creates an interactive map using `ggplot2` and `plotly`.
4. **Runs Shiny App**: Launches a Shiny app with a dropdown to filter the map by FF status.

#### How to Use

##### Prerequisites
- R (version 4.0 or higher)
- Required R packages: `ggplot2`, `maps`, `mapdata`, `sf`, `plotly`, `shiny`, `dplyr`
