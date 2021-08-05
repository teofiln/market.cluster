#' Survey of preferences of MacDonalds
#'
#' A dataset containing the the survey responses (Yes/No) to 11 questions on opinion of MacDonald's food 
#'
#' @format A data frame with 1453 rows and 11 1/0 variables:
#' data_df                                   
#''data.frame':\t1453 obs. of  11 variables:  
#' $ yummy     : int  0 1 0 1 0 1 1 1 0 1 ...
#' $ convenient: int  1 1 1 1 1 1 1 1 0 1 ...
#' $ spicy     : int  0 0 1 0 0 0 1 0 0 0 ...
#' $ fattening : int  1 1 1 1 1 1 1 1 1 1 ...
#' $ greasy    : int  0 1 1 1 1 0 0 1 1 1 ...
#' $ fast      : int  1 1 1 1 1 1 1 1 0 1 ...
#' $ cheap     : int  1 1 0 1 1 1 0 1 0 0 ...
#' $ tasty     : int  0 1 1 1 0 1 1 1 0 1 ...
#' $ expensive : int  1 1 1 0 0 0 1 0 1 1 ...
#' $ healthy   : int  0 0 1 0 1 0 1 0 0 0 ...
#' $ disgusting: int  0 0 0 1 0 0 0 0 1 0 ...
#' \describe{
#'   \item{yummy}{Is it yummy?}
#'   \item{convenient}{Is it convenient?}
#'   \item{spicy}{Is it spicy?}
#'   \item{fattening}{Is it fattening?}
#'   \item{greasy}{Is it greasy?}
#'   \item{fast}{Is it fast?}
#'   \item{cheap}{Is it cheap?}
#'   \item{tasty}{Is it tasty?}
#'   \item{expensive}{Is it expensive?}
#'   \item{healthy}{Is it healthy?}
#'   \item{disgusting}{Is it disgusting?}
#' }
#' @source \url{https://www.springer.com/gp/book/9789811088179}
#' 
## code to prepare `data_df` dataset goes here
data_df <- read.csv("data-raw/dataset.csv")
usethis::use_data(data_df, overwrite = TRUE)

