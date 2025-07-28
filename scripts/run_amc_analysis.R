run_amc_analysis <- function() {
  # Load required libraries
  if(!require(pacman)) install.packages("pacman")
  pacman::p_load(readxl, tidyverse, AMR, ggplot2, zoo)

  # User input
  cat("AMC Analysis requires population data\n")
  pop <- as.numeric(readline(prompt = "Enter your country's estimated population: "))
  if(is.na(pop) || pop <= 0) stop("Invalid population input")

  # File paths
  ref_path <- 'test-data/AMC/ATC-DDD WHO core and optional antimicrobials.xlsx'
  data_path <- 'test-data/AMC/AMC_test_data.xlsx'

  if(!file.exists(ref_path)) stop("Reference file not found at: ", ref_path)
  if(!file.exists(data_path)) stop("Data file not found at: ", data_path)

  # Import data
  who_atc_ref <- read_excel(ref_path) %>%
    filter(`Core/Optional` == 'Core') %>%
    mutate(name_route = paste0(`ATC level name`, '_', tolower(Adm.R))) %>%
    rename(aware_cats = `2023 AWaRe categorization (for J01)`)

  amc_test <- read_excel(data_path) %>%
    extract("product", c("product", "strength"), "(\\D*)(\\d.*)") %>%
    extract("pack_size", c("pack_size_unit", "pack_size"), "(\\D*)(\\d.*)") %>%
    mutate(
      route = case_when(
        str_detect(tolower(route), 'oral') ~ 'o',
        str_detect(tolower(route), 'parenteral') ~ 'p',
        TRUE ~ tolower(route)
      ),
      name_route = paste0(trimws(product), '_', route),
      strength_val = as.numeric(str_extract(strength, "\\d+")),
      strength_unit = str_remove(strength, "\\d+"),
      pack_size_val = as.numeric(str_extract(pack_size, "\\d+"))
    )

  # Analysis
  amc <- amc_test %>%
    left_join(who_atc_ref, by = 'name_route') %>%
    filter(!is.na(`ATC code`)) %>%
    mutate(
      total = quantity * strength_val * pack_size_val,
      total_g = ifelse(strength_unit == 'mg', total/1000, total),
      ddd_equivalent = total_g/as.numeric(DDD),
      did = ddd_equivalent*1000/365/pop,
      year = year(date),
      y_month = format(date, "%Y-%m"),
      y_month_date = as.Date(as.yearmon(y_month))
    ) %>%
    filter(ddd_equivalent > 0)

  # Create output directory
  if(!dir.exists("plots_AMC")) dir.create("plots_AMC")

  # Classes Distribution Plots
  amc_cats_b <- amc %>%
    group_by(year, Class) %>%
    summarise(tot_did = sum(did), .groups = "drop") %>%
    arrange(desc(tot_did)) %>%
    group_by(year) %>%
    mutate(
      annual_totals = sum(tot_did),
      dist = round(tot_did * 100 / annual_totals, 2),
      class = factor(Class, levels = unique(Class))
    ) %>%
    arrange(desc(dist))

  plt_class_dist <- ggplot(amc_cats_b, aes(x = year, y = dist, fill = class)) +
    geom_bar(stat = 'identity') +
    labs(x = 'Year', y = 'DiD (%)') +
    theme_classic() +
    theme(legend.title = element_blank())
  ggsave('plots_AMC/AMC_classes_dist.png', plt_class_dist, width = 8, height = 8, units = "in", dpi = 300)

  plt_class_tot <- ggplot(amc_cats_b, aes(x = year, y = tot_did, fill = class)) +
    geom_bar(stat = 'identity') +
    labs(x = 'Year', y = 'DiD') +
    theme_classic() +
    theme(legend.title = element_blank())
  ggsave('plots_AMC/AMC_classes.png', plt_class_tot, width = 8, height = 8, units = "in", dpi = 300)

  # AWARe Distribution
  amc_cats_aware <- amc %>%
    group_by(year, aware_cats) %>%
    summarise(tot_did = sum(did), .groups = "drop") %>%
    arrange(desc(tot_did)) %>%
    group_by(year) %>%
    mutate(
      annual_totals = sum(tot_did),
      dist = round(tot_did * 100 / annual_totals, 2),
      aware_cats = factor(aware_cats, levels = unique(aware_cats))
    ) %>%
    arrange(desc(dist))

  plt_aware_dist <- ggplot(amc_cats_aware %>% drop_na(aware_cats), aes(x = year, y = dist, fill = aware_cats)) +
    geom_bar(stat = 'identity') +
    labs(x = 'Year', y = 'DiD (%)') +
    theme_classic() +
    theme(legend.title = element_blank())
  ggsave('plots_AMC/AMC_aware.png', plt_aware_dist, width = 8, height = 8, units = "in", dpi = 300)

  # Trend Plot
  amc_trend <- amc %>%
    group_by(y_month_date, Class) %>%
    summarise(tot_did = sum(did), .groups = "drop") %>%
    arrange(desc(tot_did))

  plt_trend <- ggplot(amc_trend, aes(x = y_month_date, y = tot_did, color = Class)) +
    geom_line() +
    labs(x = 'Year', y = 'DiD') +
    theme_classic() +
    theme(legend.title = element_blank(), legend.position = 'bottom')
  ggsave('plots_AMC/AMC_trends_holder.png', plt_trend, width = 12, height = 8, units = "in", dpi = 300)

  message("Analysis complete! Check 'plots_AMC' directory for results.")
}
