match_app <- function(){
# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .handsontable .ht_master table {
        font-size: 16px;
      }
    "))
  ),
  titlePanel("Select AMR Variables"),
  rHandsontableOutput("table", height = 300),
  downloadButton("save_excel", "Save to Excel")
)

# Define Server
server <- function(input, output, session) {

  values <- reactiveValues(data = initial_df)

  output$table <- renderRHandsontable({
    rhandsontable(values$data, rowHeaders = NULL, width = 700, height = 300) %>%
      hot_col("my_dataset", type = "dropdown", source = choices, width = 300) %>%
      hot_col("man_vars", readOnly = TRUE, width = 300)  # Optional: Make label column readonly
  })

  observeEvent(input$table, {
    values$data <- hot_to_r(input$table)
  })

  output$save_excel <- downloadHandler(
    filename = function() {
      paste0("select_amr_variables.xlsx")
    },
    content = function(file) {
      write.xlsx(values$data, file)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

}



















