#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h3("Load a data file"),
        fileInput(ns("load_data"),"Load data",accept=".csv")
      ),
      mainPanel(
        verbatimTextOutput(ns("summary"))
      )
    )
  )
}
    
#' load_data Server Functions
#'
#' @noRd 
mod_load_data_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    shinyEventLogger::set_logging_session()
    
    shinyEventLogger::log_message("mod_load_data_server")
    
    output$summary <- shiny::renderPrint({
      shinyEventLogger::log_message("output$summary data_path:",input$load_data$datapath)
      
      file <- input$load_data
      ext <- tools::file_ext(file$datapath)
      shiny::req(file$datapath)
      shiny::validate(shiny::need(ext == "csv", "Please upload a csv file"))
      
      session$userData$NEW_DATA_DF <- read.csv(file$datapath)
      summary(session$userData$NEW_DATA_DF)
    })
  
 
  })
}
    
## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")
    
## To be copied in the server
# mod_load_data_server("load_data_ui_1")
