# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

# Logging 
shinyEventLogger::set_logging()

MClapply <- function(X, FUN, multicore=TRUE, ...)
{
  parLapply(multicore, X, FUN)
}

# Run shiny app
options(width=120)
flexclust.golem::run_app() # add parameters here (if any)

