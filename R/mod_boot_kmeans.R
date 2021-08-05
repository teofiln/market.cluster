#' boot_kmeans UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boot_kmeans_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width=3,
        textOutput(ns("show_seed")),
        textOutput(ns("show_start_k")),
        textOutput(ns("show_end_k")),
        textOutput(ns("show_km_nrep")),
        textOutput(ns("show_method")),
        numericInput(ns("enter_boot_rep"),"Enter bootstrap iterations to run for each K",10),
        checkboxInput(ns("scale_free"), label = "Top 2 Scales Free", value = TRUE),
        checkboxInput(ns("without_k2"), label = "Plot without K2", value = TRUE),
        actionButton(ns("run_boot_kmeans"),"Run Bootstrap")
      ),
      mainPanel(width=9,
        tabsetPanel(
          tabPanel("Summary",
                   verbatimTextOutput(ns("print_summary_boot_km"))),
          tabPanel("Global Stability Plot",
                   plotOutput(ns("plot_boot_km_boxplot")),
                   plotOutput(ns("plot_boot_km_density")))
          
        )
      )
    )
  )
}
    
#' boot_kmeans Server Functions
#'
#' @noRd 
mod_boot_kmeans_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Logging
    shinyEventLogger::set_logging_session()
    
    shinyEventLogger::log_message("mod_boot_kmeans_server")
    shinyEventLogger::log_message("BOOT_KM_MODEL set to params$boot_km_model")
    
    # Variable shared between panels
    BOOT_KM_MODEL <- reactiveVal(params$boot_km_model)
    REPS_KM_MODEL <- reactiveVal(params$reps_km_model)
    
    output$show_seed <- renderText({
      paste("Seed set as ",as.character(params$seed))
    })

    output$show_start_k <- renderText({
      paste("Starting K set as ",as.character(params$start_k))
    })

    output$show_end_k <- renderText({
      paste("Ending K set as ",as.character(params$end_k))
    })

    output$show_km_nrep <- renderText({
      paste("Kmeans iteration set as ",as.character(params$km_nrep))
    })
    
    output$show_method <- renderText({
      paste("Clustering method ",as.character(params$method))
    })

    observeEvent(input$run_boot_kmeans, {
      
      # Save values to global params
      params$boot_rep = input$enter_boot_rep
      
      shinyEventLogger::log_started("mod_boot_kmeans_server",name="fit_boot_kmeans")
      # Save the model for future use
      params$boot_km_model <<- fit_boot_kmeans(seed=params$seed,
                                   start_k=params$start_k,
                                   end_k=params$end_k,
                                   km_nrep=params$km_nrep,
                                   boot_rep = params$boot_rep,
                                   method=params$method)
      shinyEventLogger::log_message("length(boot_km_model) ",length(params$boot_km_model),name="fit_boot_kmeans")
      shinyEventLogger::log_done("mod_boot_kmeans_server",name="fit_boot_kmeans")
      
      BOOT_KM_MODEL(params$boot_km_model) # Need to use reactiveVal to trigger redraw 
      
      shinyEventLogger::log_started("mod_boot_kmeans_server",name="fit_reps_kmeans")
      # Save the model for future use
      params$reps_km_model <<- fit_reps_kmeans(seed=params$seed,
                                             start_k=params$start_k,
                                             end_k=params$end_k,
                                             km_nrep=params$km_nrep,
                                             fit_nrep = params$boot_rep,
                                             method=params$method)
      shinyEventLogger::log_message("length(reps_km_model) ",length(params$reps_km_model),name="fit_reps_kmeans")
      shinyEventLogger::log_done("mod_boot_kmeans_server",name="fit_reps_kmeans")
      
      REPS_KM_MODEL(params$reps_km_model) # Need to use reactiveVal to trigger redraw 
      
    })
    
    output$plot_boot_km_boxplot <- renderPlot({
      # Need to trigger redraw 
      validate(
        need(length(BOOT_KM_MODEL())>0,"Run Bootstrap first")
      )
      plot_boot_km_boxplot(BOOT_KM_MODEL())
    })
    
    output$plot_boot_km_density <- renderPlot({
      # Need to trigger redraw 
      validate(
        need(length(BOOT_KM_MODEL())>0,"Run Bootstrap first")
      )
      plot_boot_km_density(BOOT_KM_MODEL())
    })
    
    output$plot_top2_boot <- renderPlot({
      validate(
        need(length(BOOT_KM_MODEL())>0,"Run Bootstrap first")
      )
      plot_top2_boot(BOOT_KM_MODEL(),scale_free=input$scale_free, without_k2=input$without_k2)
    })

    output$plot_top2_reps <- renderPlot({
      validate(
        need(length(REPS_KM_MODEL())>0,"Run Reps first")
      )
      plot_top2_reps(REPS_KM_MODEL(),scale_free=input$scale_free, without_k2=input$without_k2)
    })
    
    output$print_summary_boot_km <- renderPrint({
      validate(
        need(length(BOOT_KM_MODEL())>0,"Run Bootstrap first")
      )
      print_summary_boot_km(BOOT_KM_MODEL())
    })
  })
}
    
## To be copied in the UI
# mod_boot_kmeans_ui("boot_kmeans_ui_1")
    
## To be copied in the server
# mod_boot_kmeans_server("boot_kmeans_ui_1")
