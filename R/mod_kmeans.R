#' kmeans UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_kmeans_ui <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::selectInput(
        ns("method"),
        "Clustering Method",
        choices = c(
          "kmeans",
          "kmedians",
          "jaccard",
          "ejaccard",
          "hardcl",
          "neuralgas",
          "hclust"
        ),
        selected = dplyr::coalesce(params$method, "kmeans")
      ),
      shiny::sliderInput(
        ns("enter_start_end_k"),
        "Choose range of K to search",
        min = 2,
        max = 20,
        step = 1,
        value = c(
          dplyr::coalesce(params$start_k, 2),
          dplyr::coalesce(params$end_k, 8)
        )
      ),
      shiny::numericInput(
        ns("enter_iter_max"),
        "Enter max iterations to converge",
        dplyr::coalesce(params$max_iter, 200)
      ),
      shiny::numericInput(
        ns("enter_km_nrep"),
        "Enter iterations to run for each K",
        dplyr::coalesce(params$km_nrep, 10)
      ),
      shiny::actionButton(ns("run_kmeans"), "Run")
    ),
    shiny::mainPanel(width = 9,
                     shiny::tabsetPanel(
                       shiny::tabPanel(
                         "Scree plot",
                         shiny::plotOutput(ns("plot_scree")),
                         shiny::verbatimTextOutput(ns("print_summary_km"))
                       )
                       
                     ))
  ))
}
    
#' kmeans Server Functions
#'
#' @noRd 
mod_kmeans_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Start logging
    shinyEventLogger::set_logging_session()
    
    shinyEventLogger::log_message("mod_kmeans_server")
    shinyEventLogger::log_message("KM_MODEL set to params$km_model")
    KM_MODEL <- shiny::reactiveVal(session$userData$PARAMS$km_model)
    
    # Click on run cluster button
    shiny::observeEvent(input$run_kmeans, {

      # Save values to global params
      session$userData$PARAMS$seed <- 12345 #input$enter_seed
      session$userData$PARAMS$start_k <- input$enter_start_end_k[1]
      session$userData$PARAMS$end_k <- input$enter_start_end_k[2]
      session$userData$PARAMS$km_nrep <- input$enter_km_nrep
      session$userData$PARAMS$method <- input$method
      session$userData$PARAMS$max_iter <- input$enter_iter_max

      shinyEventLogger::log_started("mod_kmeans_server", name = "fit_kmeans")
      # Save the model for future use
      session$userData$PARAMS$km_model <-
        fit_kmeans(
          seed = session$userData$PARAMS$seed,
          start_k = session$userData$PARAMS$start_k,
          end_k = session$userData$PARAMS$end_k,
          nrep = session$userData$PARAMS$km_nrep,
          max_iter = session$userData$PARAMS$max_iter,
          method = session$userData$PARAMS$method
        )
      shinyEventLogger::log_message("length(km_model) ",
                                    length(session$userData$PARAMS$km_model),
                                    name = "fit_kmeans")
      shinyEventLogger::log_message(
        "length(km_model@models) ",
        length(session$userData$PARAMS$km_model@models),
        name = "fit_kmeans"
      )
      shinyEventLogger::log_done("mod_kmeans_server", name = "fit_kmeans")

      KM_MODEL(session$userData$PARAMS$km_model) # Need to use reactiveVal to trigger redraw 
    })
    
    output$plot_scree <- shiny::renderPlot({
      #KM_MODEL(session$userData$PARAMS$km_model)
      shiny::validate(shiny::need(length(KM_MODEL()) > 0, "Run KMeans first"))
      plot_scree(KM_MODEL())
    })
    
    output$print_summary_km <- shiny::renderPrint({
      shiny::validate(
        shiny::need(length(KM_MODEL()) > 0,"Run KMeans first")
      )
      print_summary_km(KM_MODEL())
    })
    
    # Move to Pick K
    # output$plot_slsa <- renderPlot({
    #   #KM_MODEL(params$km_model)
    #   validate(
    #     need(length(KM_MODEL())>0,"Run KMeans first")
    #   )
    #   plot_slsa(KM_MODEL())
    # })
    

  })
}
    
## To be copied in the UI
# mod_kmeans_ui("kmeans_ui_1")
    
## To be copied in the server
# mod_kmeans_server("kmeans_ui_1")
