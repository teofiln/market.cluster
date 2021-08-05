# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Logging 
shinyEventLogger::set_logging()

MClapply <- function(X, FUN, multicore=TRUE, ...)
{
  parLapply(multicore, X, FUN)
}

# Run the application
run_app()
