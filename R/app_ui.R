#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      # Logging on js
      shinyEventLogger::log_init(),
      
      titlePanel("Clustering Workbench"),
      navbarPage("Cluster",
                 navbarMenu("Data",
                            tabPanel("Load data",
                                     mod_load_data_ui("load_data_ui_1")
                            ),         
                            tabPanel("EDA",
                                     mod_EDA_ui("EDA_ui_1")
                            )
                 ),
                 navbarMenu("Pick Method",
                            tabPanel("Common Methods",mod_kmeans_ui("kmeans_ui_1")),
                            tabPanel("Partition Methods", mod_cluster_partition_ui("cluster_partition_ui_1"))
                 ),
                 tabPanel("Find K",
                          mod_boot_kmeans_ui("boot_kmeans_ui_1")
                 ),
                 tabPanel("Pick K",
                          mod_pick_k_ui("pick_k_ui_1")
                 )
                 # Can't get this to work.
                 #tabPanel("About",
                 #          fluidPage(includeHTML("data/McDonalds-Case-Study.html"))
                 #)
      )
    )
  )
}


# navbarMenu("Data",
#            tabPanel("Load data",
#                     mod_load_data_ui("load_data_ui_1")
#            ),         
#            tabPanel("EDA",
#                     mod_EDA_ui("EDA_ui_1")
#            ),
#            tabPanel("a"), tabPanel("b")
# ),
# tabPanel("Kmeans",
#          mod_kmeans_ui("kmeans_ui_1")
# ),
# tabPanel("Find K",
#          mod_boot_kmeans_ui("boot_kmeans_ui_1")
# ),
# tabPanel("Pick K",
#          mod_pick_k_ui("pick_k_ui_1")
# ),
# tabPanel("About",
#          fluidPage(
#            includeHTML("data/McDonalds-Case-Study.html")
#          )
# )


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'flexclust.golem'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


#' App page
#' 
#' @description Generate the same UI as `app_ui()`, but without the page 
#' containers
#' 
#' @export
app_page <- function(request) {
  tagList(
      # Logging on js
      shinyEventLogger::log_init(),
      
                 navbarMenu("Data",
                            tabPanel("Load data",
                                     mod_load_data_ui("load_data_ui_1")
                            ),         
                            tabPanel("EDA",
                                     mod_EDA_ui("EDA_ui_1")
                            )
                 ),
                 navbarMenu("Pick Method",
                            tabPanel("Common Methods",mod_kmeans_ui("kmeans_ui_1")),
                            tabPanel("Partition Methods", mod_cluster_partition_ui("cluster_partition_ui_1"))
                 ),
                 tabPanel("Find K",
                          mod_boot_kmeans_ui("boot_kmeans_ui_1")
                 ),
                 tabPanel("Pick K",
                          mod_pick_k_ui("pick_k_ui_1")
                 )
                 # Can't get this to work.
                 #tabPanel("About",
                 #          fluidPage(includeHTML("data/McDonalds-Case-Study.html"))
                 #)
  )
}