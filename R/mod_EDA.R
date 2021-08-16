#' EDA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_EDA_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::tag$h3("Exploratory Plots"),
        shiny::radioButtons(
          ns("which_data"),
          "Select data",
          choices = c("current", "new"),
          selected = "current"
        ),
        shiny::actionButton(ns("replace_data"), "Replace current with new"),
        shiny::sliderInput(
          ns("plotsize"),
          label   = 'Plot size',
          value = 2,
          min = 1,
          max = 6,
          step = 1
        )
      ),
      shiny::mainPanel( width = 9,
                        shiny::tabsetPanel(
                          shiny::tabPanel("Summary",
                                          shiny::verbatimTextOutput(ns("print_skim"))
                   #textOutput(ns("print_skim"))
                   #htmlOutput(ns("print_skim"))
                   ),
                   shiny::tabPanel("Plot count",
                                   shiny::plotOutput(ns("plot_responses"))
                   ),
                   shiny::tabPanel("Plot Proportion",
                                   shiny::plotOutput(ns("plot_responses_pct"))
                   ),
                   shiny::tabPanel("PCA",
                                   shiny::uiOutput(ns("plot_pca")),
                                   shiny::verbatimTextOutput(ns("print_summary_pca")),
          ),
          shiny::tabPanel("PCA Scree",
                          shiny::selectInput(ns("scree_which"),"Which scree plot",
                               choices = c("Standard deviation","Proportion of Variance","Cumulative Proportion","Eigen Values"),
                               selected = "Eigen Values"),
                          shiny::plotOutput(ns("plot_pca_scree")),
                          shiny::dataTableOutput(ns("table_components"))
          ),
          shiny::tabPanel("Correlation Plot",
                          shiny::uiOutput(ns("plot_pairs"))
                   ),
          shiny::tabPanel("tSNE",
                          shiny::sliderInput(ns("perplexity"),"Perplexity",
                               min = 1, max=100,value=50,step=5),
                          shiny::sliderInput(ns("theta"),"Theta",
                               min = 0.0, max=1.0,value=0.5,step=0.1),
                          shiny::sliderInput(ns("exag"),"Exagerration",
                               min = 0.0, max=50,value=12,step=1),
                          shiny::uiOutput(ns("plot_tsne"))
          )
          
        )
      )
    )
 
  )
}
    
#' EDA Server Functions
#'
#' @noRd 
mod_EDA_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Logging
    shinyEventLogger::set_logging_session()
    shinyEventLogger::log_message("mod_boot_kmeans_server")
    
    DATA <- shiny::reactiveVal(session$userData$RADIANT_DATA)
    
    shiny::observe({
      shiny::req(DATA())
      print(head(DATA()))
    })
    
    ##### Plot Size in reactive
    
    plotsize <- shiny::reactive({
      shiny::req(input$plotsize)
      as.numeric(input$plotsize)
    })
    
    plotHeight <- shiny::reactive(480 * plotsize())      
    
    #####
    output$plot_responses <- shiny::renderPlot({
      plot_responses(DATA())
    })
    
    output$plot_responses_pct <- shiny::renderPlot({
      plot_responses_pct(DATA())
    })

    ## Plot PCA
    
    output$plot_pca_raw <- shiny::renderPlot({
      plot_pca(DATA())
    })
    
    output$plot_pca <- shiny::renderUI({
      shiny::plotOutput(ns("plot_pca_raw"), height = plotHeight())
    })
    
    #####
    
    ## Plot tsne
    
    output$plot_tsne_raw <- shiny::renderPlot({
      plot_tsne(
        data = DATA(),
        perp = input$perplexity,
        theta = input$theta,
        exag = input$exag
      )
    })
    
    output$plot_tsne <- shiny::renderUI({
      shiny::plotOutput(ns("plot_tsne_raw"), height = plotHeight())
    })
    
    #####
    output$print_skim <- shiny::renderPrint({
      shinyEventLogger::log_message("save DATA to params$data")
      session$userData$PARAMS$data <- DATA()
      shinyEventLogger::log_message("output$print_skim")
      skimr::skim(DATA())
    }, width = 120)
    
    output$print_summary_pca <- shiny::renderPrint({
      print_summary_pca(DATA())
    })
    
    shiny::observeEvent(input$replace_data, {
      shinyEventLogger::log_message(
        "input$replace_data\n BEFORE data_df:",
        nrow(data_df),
        "rows  new_data_df:",
        nrow(new_data_df),
        " rows"
      )
      session$userData$DATA_DF <- new_data_df
      DATA <- shiny::reactiveVal(session$userData$DATA_DF)
      shinyEventLogger::log_message(
        "AFTER  data_df:",
        nrow(session$userData$DATA_DF),
        "rows  new_data_df:",
        nrow(session$userData$DATA_DF),
        " rows"
      )
    })
    
    shiny::observeEvent(input$which_data, {
      if (input$which_data == "current") {
        DATA(data_df)
      } else {
        DATA(new_data_df)
      }
    })
    
    output$plot_pca_scree <- shiny::renderPlot({
      plot_pca_scree(DATA(), which = input$scree_which)
    })
    
    output$table_components <- shiny::renderDataTable({
      table_components(DATA())
    })
    
    ## Plot correlation pairs
    
    output$plot_pairs_raw <- shiny::renderPlot({
      plot_pairs(DATA())
    })
    
    output$plot_pairs <- shiny::renderUI({
      shiny::plotOutput(ns("plot_pairs_raw"), height = plotHeight())
    })
    
    ####
  })
}
    
## To be copied in the UI
# mod_EDA_ui("EDA_ui_1")
    
## To be copied in the server
# mod_EDA_server("EDA_ui_1")
