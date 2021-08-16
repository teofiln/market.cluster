#' pick_k UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pick_k_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    sidebarLayout(
      sidebarPanel(width=3,
        textOutput(ns("show_seed")),
        textOutput(ns("show_start_k")),
        textOutput(ns("show_end_k")),
        textOutput(ns("show_km_nrep")),
        textOutput(ns("show_method")),
        selectInput(ns("select_k"),"Select K",choices = params$start_k:params$end_k, selected = params$start_k),
        selectInput(ns("select_n"),"Select Number of profiles",choices = 1:10, selected = 3),
        downloadButton(ns("downloadData"),"Download solution")),
      mainPanel(width=9,
        tabsetPanel(
          tabPanel("SLSA Plot",
                   plotOutput(ns("plot_slsa"))),
          tabPanel("Segment stability Plot",
                   plotOutput(ns("plot_segment_stability_within_solution"))),
          tabPanel("Pick Solution",
                   actionButton(ns("gen_solutions"),"Generate Solutions"),
                   sliderInput(ns("pick_solution"),"Pick solution",min=1,max=10,value=1,step=1),
                   plotOutput(ns("plot_many_segment_profiles"),height="900px"),
                   checkboxInput(ns("hull"),"Plot with hull",value=FALSE),
                   checkboxInput(ns("neighborhood"),"Plot with neighborhood",value=FALSE),
                   plotOutput(ns("plot_solutions"),width="2048px", height="1600"))
        )
      )
    )
    
  )
}
    
#' pick_k Server Functions
#'
#' @noRd 
mod_pick_k_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Logging
    shinyEventLogger::set_logging_session()
    
    shinyEventLogger::log_message("mod_pick_k_server")
    shinyEventLogger::log_message("REPS_KM_MODEL set to params$reps_km_model")
    
    REPS_KM_MODEL <- reactiveVal(session$userData$PARAMS$reps_km_model)
    SOLUTIONS <- reactiveVal(list())
    PARAMS <- reactiveVal(session$userData$PARAMS)
    
    output$show_seed <- renderText({
      paste("Seed set as ",as.character(session$userData$PARAMS$seed))
    })
    
    output$show_start_k <- renderText({
      paste("Starting K set as ",as.character(session$userData$PARAMS$start_k))
    })
    
    output$show_end_k <- renderText({
      paste("Ending K set as ",as.character(session$userData$PARAMS$end_k))
    })
    
    output$show_km_nrep <- renderText({
      paste("Kmeans iteration set as ",as.character(session$userData$PARAMS$km_nrep))
    })
    
    output$show_method <- renderText({
      paste("Clustering method ",as.character(session$userData$PARAMS$method))
    })
    
    output$plot_neighborhood <- renderPlot({
      validate(
        need(length(session$userData$PARAMS$km_model)>0,"Run KMeans first")
      )
      plot_neighborhood(session$userData$PARAMS$km_model,k=input$select_k)
    }) 
    
    output$plot_slsa <- renderPlot({
      validate(
        need(length(session$userData$PARAMS$km_model)>0,"Run KMeans first")
      )
      plot_slsa(session$userData$PARAMS$km_model)
    })

    
    output$plot_hull <- renderPlot({
      validate(
        need(length(session$userData$PARAMS$km_model)>0,"Run KMeans first")
      )
      plot_hull(session$userData$PARAMS$km_model,k=input$select_k)
    }) 

    output$plot_segment_profile <- renderPlot({
      validate(
        need(length(session$userData$PARAMS$km_model)>0,"Run KMeans first")
      )
      plot_segment_profile(session$userData$PARAMS$km_model,k=input$select_k)
    })
    
    output$plot_segment_stability_within_solution <- renderPlot({
      shinyEventLogger::log_started("plot_segment_stability_within_solution")
      validate(
        need(length(session$userData$PARAMS$km_model)>0,"Run KMeans first")
      )
      plot_segment_stability_within_solution(session$userData$PARAMS$km_model,k=input$select_k)
      shinyEventLogger::log_done("plot_segment_stability_within_solution")
    })
    
    output$plot_gorge <- renderPlot({
      validate(
        need(length(session$userData$PARAMS$km_model)>0,"Run KMeans first")
      )
      plot_gorge(session$userData$PARAMS$km_model,k=input$select_k)
    })
    
    observeEvent(input$gen_solutions,{
      shinyEventLogger::log_started("gen_solutions")
      validate(
        need(length(session$userData$PARAMS$km_model)>0,"Run KMeans first")
      )
      session$userData$PARAMS$solutions <- plot_many_segment_profiles(k=input$select_k, n=input$select_n)
      SOLUTIONS(session$userData$PARAMS$solutions)
      shinyEventLogger::log_done("gen_solutions")
    })
    
    output$plot_many_segment_profiles <- renderPlot({
      shinyEventLogger::log_started("plot_many_segment_profiles")
      validate(
        need(length(SOLUTIONS())>0,"Generate solutions first")
      )
      solutions<-SOLUTIONS()
      plot_profile(solutions,input$pick_solution)
      shinyEventLogger::log_done("plot_many_segment_profiles")
    })
    
    output$plot_solutions <- renderPlot({
      shinyEventLogger::log_started("plot_solutions")
      validate(
        need(length(SOLUTIONS())>0,"Generate solutions first")
      )
      solutions<-SOLUTIONS()
      plot_solution(solutions,input$pick_solution, input$hull, input$neighborhood)
      shinyEventLogger::log_done("plot_solutions")
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("solution-", Sys.Date(), ".Rda", sep="")
      },
      content = function(file) {
        save(params, file=file)
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_pick_k_ui("pick_k_ui_1")
    
## To be copied in the server
# mod_pick_k_server("pick_k_ui_1")
