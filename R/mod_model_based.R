#' model_based UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model_based_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    sidebarLayout(
      sidebarPanel(width=3,
                   selectInput(ns("method"),"Distribution for model",
                               choices=c("Bernoulli Distribution", "Normal Distribution", "Poisson Distribution"),
                               selected=dplyr::coalesce(params$method,"Bernoulli Distribution")),
                   sliderInput(ns("enter_start_end_k"), "Choose range of K to search",
                               min=2, max=20, step=1, 
                               value=c(dplyr::coalesce(params$start_k,2),
                                       dplyr::coalesce(params$end_k,8))),
                   numericInput(ns("enter_km_nrep"),"Enter iterations to run for each K",dplyr::coalesce(params$km_nrep,10)),
                   actionButton(ns("run_flexmix"),"Run")
      ),
      mainPanel(width=9,
                tabsetPanel(
                  tabPanel("Scree plot",
                           plotOutput(ns("plot_scree")), #plot(stepFelxmix)
                           verbatimTextOutput(ns("print_summary"))), #show(stepFlexmix),
                  # tabPanel("SLSA Plot",
                  #          plotOutput(ns("plot_slsa")))
                  tabPanel("Model Parameters",
                           selectInput(ns("select_k"),"Select segment",
                                       choices = 2:20, #ifelse(params$flexmix_model,2,params$flexmix_model@k),
                                       selected = 2),
                           plotOutput(ns("plot_parameters")),
                           verbatimTextOutput(ns("print_parameters")))
                  
                )
      )
    )
    
    
  )
}
    
#' model_based Server Function
#'
#' @noRd 
mod_model_based_server <- function(input, output, session){
  ns <- session$ns
  
  # Start logging
  shinyEventLogger::set_logging_session()
  
  shinyEventLogger::log_message("mod_kmeans_server")
  shinyEventLogger::log_message("FM_MODEL set to params$flexmix_model")
  FM_MODEL <- reactiveVal(session$userData$PARAMS$flexmix_model)
  
  observeEvent(input$run_flexmix,{
    
    shinyEventLogger::log_started("mod_kmeans_server",name="fit_kmeans")
    
    session$userData$PARAMS$seed <- 12345 #input$enter_seed
    session$userData$PARAMS$start_k <- input$enter_start_end_k[1]
    session$userData$PARAMS$end_k <- input$enter_start_end_k[2]
    session$userData$PARAMS$km_nrep <- input$enter_km_nrep
    session$userData$PARAMS$method <- input$method
    
    session$userData$PARAMS$flexmix_model <- fit_flexmix(
      seed =    session$userData$PARAMS$seed,
      start_k = session$userData$PARAMS$start_k,
      end_k =   session$userData$PARAMS$end_k,
      nrep =    session$userData$PARAMS$km_nrep,
      method =  session$userData$PARAMS$method
    )
    
    FM_MODEL(session$userData$PARAMS$flexmix_model)
    
    shinyEventLogger::log_done("mod_kmeans_server",name="fit_kmeans")
    
  })

  output$plot_scree <- renderPlot({
    validate(
      need(length(FM_MODEL())>0,"Run first")
    )
    plot_flexmix_scree(FM_MODEL())
  }) 
  
  output$print_summary <- renderPrint({
    validate(
      need(length(FM_MODEL())>0,"Run first")
    )
    print_flexmix_summary(FM_MODEL())
    
  })
  
  output$plot_parameters <- renderPlot({
    validate(
      need(length(FM_MODEL())>0,"Run first")
    )
    plot_flexmix_parameters(FM_MODEL(),input$select_k)    
  })
  
  output$print_parameters <- renderPrint({
    validate(
      need(length(FM_MODEL())>0,"Run first")
    )
    print_flexmix_parameters(FM_MODEL(), input$select_k )    
  })
}
    
## To be copied in the UI
# mod_model_based_ui("model_based_ui_1")
    
## To be copied in the server
# callModule(mod_model_based_server, "model_based_ui_1")
 
