# Load libraries
library(ggplot2)
library(maps)
library(mapdata)
library(sf)
library(plotly)
library(shiny)
library(dplyr)

# Function to automate the entire process
automate_amr_map <- function() {
  # URL of the CSV file
  csv_url <- "https://ourworldindata.org/grapher/national-system-to-monitor-spread-of-antimicrobial-resistance-in-humans.csv?v=1&csvType=full&useColumnShortNames=true"

  # Load the data directly from the URL
  data <- read.csv(csv_url)

  # Rename columns for easier use
  colnames(data) <- c("Entity", "Code", "Year", "Surveillance_Status")

  # Rename country names to fit map dataset
  data <- data %>%
    mutate(Entity = case_when(
      Entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
      Entity == "Congo" ~ "Republic of Congo",
      TRUE ~ Entity  # Keeps other values unchanged
    ))

  # Define FF and Non-FF Countries
  none_ff_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burundi", "Cape Verde",  "Central African Republic", "Chad", "Comoros", "Republic of Congo", "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Lesotho", "Liberia", "Libya", "Madagascar", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Rwanda", "Sao Tome and Principe", "Seychelles", "Somalia", "South Africa", "South Sudan", "Sudan", "Togo", "Tunisia", "Democratic Republic of the Congo")
  ff_countries <- c("Burkina Faso", "Cameroon", "Eswatini", "Gabon", "Ghana", "Kenya", "Malawi", "Nigeria", "Senegal", "Sierra Leone", "Tanzania", "Uganda", "Zambia", "Zimbabwe")

  # Assign FF_Status
  data <- data %>%
    mutate(
      FF_Status = case_when(
        Entity %in% none_ff_countries ~ "None FF Countries",
        Entity %in% ff_countries ~ "FF Countries",
        TRUE ~ ""  # Leaves other countries blank
      ))

  # Load world map data
  world_map <- map_data("world")

  # Rename country names to fit map dataset
  world_map <- world_map %>%
    mutate(region = case_when(
      region == "Ivory Coast" ~ "Cote d'Ivoire",
      region == "Swaziland" ~ "Eswatini",
      TRUE ~ region  # Keeps other values unchanged
    ))

  # Merge the filtered data with the world map data
  merged_data <- world_map %>%
    left_join(data, by = c("region" = "Entity"))

  # List of selected countries
  selected_countries <- c(
    "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde",
    "Central African Republic", "Chad", "Comoros", "Republic of Congo", "Cote d'Ivoire", "Djibouti", "Egypt",
    "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea",
    "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali",
    "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda",
    "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa",
    "South Sudan", "Sudan","Eswatini", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe", "Democratic Republic of the Congo"
  )

  # Filter the merged data for selected countries
  filtered_data <- merged_data %>% filter(region %in% selected_countries)

  # Define the UI for the Shiny app
  ui <- fluidPage(
    titlePanel("AMR Surveillance Systems in African Countries (2024)"),
    sidebarLayout(
      sidebarPanel(
        style = "width: 200px;",  # Set fixed width in pixels
        selectInput("ff_status", "Select FF Status:",
                    choices = c("All", unique(filtered_data$FF_Status)))
      ),
      mainPanel(
        plotlyOutput("map", height = "800px", width = "1200px"),  # Map output
        tags$div(style = "text-align: center; margin-top: 20px; font-size: 12px; color: #666; font-style: italic;",
                 "Data Source: ",
                 tags$a(href = "https://ourworldindata.org/grapher/national-system-to-monitor-spread-of-antimicrobial-resistance-in-humans?region=Africa", "Our World in Data", target = "_blank"))  # Hyperlink
      )
    )
  )

  # Define the server logic
  server <- function(input, output) {
    output$map <- renderPlotly({
      # Filter data based on selected FF Status
      filtered <- filtered_data
      if (input$ff_status != "All") {
        filtered <- filtered_data %>% filter(FF_Status == input$ff_status)
      }

      # Create the static map
      static_map <- ggplot(filtered, aes(x = long, y = lat, group = group, fill = Surveillance_Status,
                                         text = paste("region: ", region, "<br>Status: ", Surveillance_Status))) +
        geom_polygon(color = "black", size = 0.1) +
        scale_fill_manual(
          values = c(
            "Local collation of AMR data for common bacterial infections" = "#fdb462",
            "National collation of AMR data for common bacterial infections" = "#ffffb3",
            "Standardized national AMR surveillance system" = "#38aaba",
            "National AMR surveillance system linked with antimicrobial consumption data" = "#4c6a9c",
            "No capacity for generating data on antibiotic resistance" = "#e56e5a",
            "None FF Countries" = "#bababa"
          ),
          na.value = "gray90"  # Color for missing data
        ) +
        labs(title = "AMR Surveillance Systems in African Countries (2024)",
             subtitle = "Data Source: Our World in Data",
             fill = "Surveillance Status") +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              legend.position = "bottom",  # Move legend to the bottom
              legend.text = element_text(size = 8),  # Reduce legend text size
              legend.title = element_text(size = 8)  # Reduce legend title size
        ) +
        coord_map("mercator")  # Apply Mercator projection

      # Convert to interactive map
      ggplotly(static_map, tooltip = "text", height = 800, width = 1200)  # Adjust height and width here
    })
  }

  # Run the Shiny app
  shinyApp(ui = ui, server = server)
}

# Call the function to run the entire process
automate_amr_map()
