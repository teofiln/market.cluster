#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  shinyEventLogger::set_logging()
  # Your application server logic
  mod_load_data_server("load_data_ui_1")
  mod_EDA_server("EDA_ui_1")
  mod_kmeans_server("kmeans_ui_1")
  mod_cluster_partition_server("cluster_partition_ui_1")
  mod_boot_kmeans_server("boot_kmeans_ui_1")
  mod_pick_k_server("pick_k_ui_1")
  callModule(mod_model_based_server, "model_based_ui_1")
}
