## code to prepare `params` dataset goes here

params <- list( seed = 12345,
                start_k = 2,
                end_k = 8,
                max_iter = 200,
                classification = "Hard",
                init_center = "Random",
                centering_method = "Means",
                distance = "Euclidean",
                km_nrep = 10,
                boot_rep = 10,
                data = data_df,
                km_model = list(),
                flexmix_model = list(),
                boot_km_model = list(),
                reps_km_model = list(),
                method = "kmeans")

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

## code to prepare `data_df` dataset goes here
data_df <- read.csv("data-raw/dataset.csv")


## code to prepare `new_data_df` dataset goes here

new_data_df <- data_df

usethis::use_data(params, data_df, new_data_df, internal = TRUE, overwrite = TRUE)
