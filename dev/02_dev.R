# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "lattice" )
usethis::use_package( "modeltools" )
usethis::use_package( "stats4" )
usethis::use_package( "grid" )

usethis::use_package( "clue" )
usethis::use_package( "flexclust" )
usethis::use_package( "dplyr" )
usethis::use_package( "ggplot2" )
usethis::use_package( "ggfortify" )
usethis::use_package( "GGally" )
usethis::use_package( "tidyr" )
usethis::use_package( "tibble" )
usethis::use_package( "forcats" )
usethis::use_package( "purrr" )
usethis::use_package( "broom" )
usethis::use_package( "Rtsne" )

usethis::use_package( "MASS" )
usethis::use_package( "shinyEventLogger" )
usethis::use_package( "doParallel" )
usethis::use_package( "skimr" )
usethis::use_package( "readr" )
usethis::use_package( "corrplot" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "EDA" ) # Explore data
golem::add_module( name = "load_data" ) # Load data
golem::add_module( name = "kmeans" ) # Run Kmeans
golem::add_module( name = "cluster_partition" ) # Run Partition methods
golem::add_module( name = "boot_kmeans" ) # Run Boot Kmeans
golem::add_module( name = "pick_k" ) # Run Pick K
golem::add_module( name = "pick_solution" ) # Run Pick solution
golem::add_module( name = "model_based" ) # Run Pick solution


## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "eda_plot" ) 
golem::add_fct( "kmeans" ) 
golem::add_fct( "model_based" ) 
golem::add_fct( "boot_kmeans" )
golem::add_fct( "pick_k" )
golem::add_utils( "general" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "data_df", open = FALSE ) 
usethis::use_data_raw( name = "params", open = FALSE ) 
usethis::use_data_raw( name = "new_data_df", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("flexclust.golem")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

