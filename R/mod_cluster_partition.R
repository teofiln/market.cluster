#' cluster_partition UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cluster_partition_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    sidebarLayout(
      sidebarPanel(width=3,
                   selectInput(ns("centering_method"),"Centering Method",
                               choices=c("Fast Kmeans",
                                         "Mean", 
                                         "Median",
                                         "Angle",
                                         "Nelder-Mead",
                                         "L-BFGS-B"),
                               selected="Means"),
                   selectInput(ns("distance_function"),"Distance",
                               choices=c("Euclidean", 
                                         "Manhattan",
                                         "Minkowski",
                                         "Max",
                                         "Jaccard",
                                         "Angle",
                                         "Canberra",
                                         "Correlation"),
                               selected="Euclidean"),
                   radioButtons(ns("init_center"),"Initial Center",
                                choices=c("Random","Kmeans++"),selected="Random"),
                   radioButtons(ns("classification"),"Classification",
                                choices=c("Hard","Simulated Annealing"), selected="Hard"),
                   sliderInput(ns("max_iter"),"Max iterations",
                               min=10,max=1000,step=10,value=200),
                   sliderInput(ns("enter_start_end_k"), "Choose range of K to search",
                               min=2, max=20, step=1, 
                               value=c(dplyr::coalesce(params$start_k,2),
                                       dplyr::coalesce(params$end_k,8))),
                   numericInput(ns("enter_km_nrep"),"Iterations to run for each K",dplyr::coalesce(params$km_nrep,10)),
                   numericInput(ns("enter_seed"),"Enter a random seed",dplyr::coalesce(params$seed,12345)),
                   actionButton(ns("run"),"Run")
      ),
      mainPanel(width=9,
                tabsetPanel(
                  tabPanel("Scree plot",
                           plotOutput(ns("plot_scree")),
                           verbatimTextOutput(ns("print_summary_km")))
                  # tabPanel("SLSA Plot",
                  #          plotOutput(ns("plot_slsa")))
                  
                )
      )
    )
    
    
  )
}
    
#' cluster_partition Server Functions
#'
#' @noRd 
mod_cluster_partition_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Start logging
    shinyEventLogger::set_logging_session()
    
    shinyEventLogger::log_message("mod_cluster_partition_server")
    shinyEventLogger::log_message("KM_MODEL set to params$km_model")
    KM_MODEL <- reactiveVal(params$km_model)
    
    # Click on run cluster button
    observeEvent(input$run, {
      
      shinyEventLogger::log_message("input$run",
                                    "\ninput$enter_seed:",input$enter_seed,
                                    "\ninput$enter_start_end_k:", input$enter_start_end_k[1], "-", input$enter_start_end_k[2],
                                    "\ninput$init_center:", input$init_center,
                                    "\ninput$classification:", input$classification,
                                    "\ninput$max_iter:",input$max_iter,
                                    "\ninput$enter_km_nrep:", input$enter_km_nrep,
                                    "\ninput$centering_method:", input$centering_method,
                                    "\ninput$distance:", input$distance_function)
      # Save values to global params
      params$seed <<- input$enter_seed
      params$start_k <<- input$enter_start_end_k[1]
      params$end_k <<- input$enter_start_end_k[2]
      params$init_center <<- input$init_center
      params$classification <<- input$classification
      params$max_iter <<- input$max_iter
      params$km_nrep <<- input$enter_km_nrep
      params$method <<- paste(input$centering_method,"/",input$distance_function)
      params$centering_method <<- input$centering_method
      params$distance <<- input$distance_function
      
      shinyEventLogger::log_started("input$run")
      # Save the model for future use
      params$km_model <<- fit_partition()
      shinyEventLogger::log_message("length(km_model) ",length(params$km_model),name="fit_partition")
      shinyEventLogger::log_message("length(km_model@models) ",length(params$km_model@models),name="fit_partition")
      shinyEventLogger::log_done("input$run")
      
      KM_MODEL(params$km_model) # Need to use reactiveVal to trigger redraw 
    })
    
    output$plot_scree <- renderPlot({
      #KM_MODEL(params$km_model)
      validate(
        need(length(KM_MODEL())>0,"Run KMeans first")
      )
      plot_scree(KM_MODEL())
    })
    
    output$print_summary_km <- renderPrint({
      validate(
        need(length(KM_MODEL())>0,"Run KMeans first")
      )
      print_summary_km(KM_MODEL())
    })
    

  })
}
    
## To be copied in the UI
# mod_cluster_partition_ui("cluster_partition_ui_1")
    
## To be copied in the server
# mod_cluster_partition_server("cluster_partition_ui_1")
