#'
#'
#'
#'

plot_responses <- function(data_df){
  data_df %>% 
    tidyr::gather(key="Variable",value="Response") %>% 
    ggplot2::ggplot(ggplot2::aes(y=Variable, x=Response)) + 
    ggplot2::geom_col() +
    ggplot2::ggtitle("Count of YES responses by question") +
    ggplot2::xlab("Count") +
    ggplot2::ylab("Question")
}

plot_responses_pct <- function(data_df){
  data_df %>% 
    tidyr::gather(key="variable",value="response") %>%
    dplyr::group_by(variable) %>% 
    dplyr::summarise(response_pct = sum(response)/dplyr::n()) %>% 
    ggplot2::ggplot(ggplot2::aes(y=variable, x = response_pct)) + 
    ggplot2::geom_col() +
    ggplot2::ggtitle("Proportion of YES responses by question") +
    ggplot2::xlab("Proportion") +
    ggplot2::ylab("Question")
}

# PCA 

print_summary_pca <- function(data_df){
  # Reduce dimension using PCA
  pca_df <- prcomp(data_df)
  # Summary of PCA
  summary(pca_df)
}

table_components <- function(data_df) {
  # Reduce dimension using PCA
  pca_df <- prcomp(data_df)
  as.data.frame(signif(pca_df$rotation, 3)) %>% tibble::rownames_to_column(var = "Original")
}

plot_pca <- function(data_df) {
  # Reduce dimension using PCA
  pca_df <- prcomp(data_df)
  # Plot PCA
  ggplot2::autoplot(
    pca_df,
    col = "grey",
    loadings = TRUE,
    label = FALSE,
    loadings.label = TRUE,
    loadings.label.repel = TRUE,
    loadings.colour = "grey"
  )
}

plot_tsne <- function(data_df,
                      perp = 50,
                      theta = 0.5,
                      pca = TRUE,
                      exag = 0.5) {
  unique_matrix <- unique(as.matrix(data_df))
  
  set.seed(params$seed)
  t_out <- Rtsne::Rtsne(
    unique_matrix,
    perplexity = perp,
    theta = theta,
    pca = pca,
    exaggeration_factor = exag
  )
  plot_out <- as.data.frame(t_out$Y)
  
  ggplot2::ggplot(plot_out, aes(x = V1, y = V2)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle(paste("tSNE - perp:", perp, "theta:", theta, "pca:", pca, "exag:", exag))
}

plot_pca_scree <- function(data_df,which="Eigen Values"){
  
  pca <- prcomp(as.matrix(data_df))
  sum_pca <- summary(pca)
  
  # Get eigen values
  r <- cor(as.matrix(data_df))
  s <- eigen(r)
  
  # Add to importance
  m <- rbind(sum_pca$importance,s$values)
  
  # Set name to eigen values
  rn <- row.names(m)
  rn[4] <- "Eigen Values"
  rownames(m) <- rn
  
  if (!which %in% rn)
    stop("Invalid Parameter 'which'.")
  
  p <- m %>% 
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tidyr::gather(key = "component", value = "value", -rowname) %>%
    dplyr::filter(rowname == which) %>%
    dplyr::mutate(order = dplyr::row_number(),
           component = forcats::fct_reorder(component, order)) %>%
    ggplot2::qplot(data = .,
          x = component,
          y = value,
          geom = "col") +
    ggplot2::ggtitle(paste("PCA Scree Plot - ", which)) +
    ggplot2::ylab(which) + ggplot2::xlab("Component")
  
  
  if (which == "Eigen Values") {
    p <- p + ggplot2::geom_hline(yintercept = 1, col = "red")
  }
  
  if (which == "Cumulative Proportion") {
    p <- p + ggplot2::geom_hline(yintercept = c(0.7,0.8,0.9), col = "red")
  }
  
  return(p)
}


plot_pairs <- function(data_df) {
  corr <- cor(data_df)
  corr_sig <- corrplot::cor.mtest(data_df, conf.level = .95)
  
  corrplot::corrplot(
    corr,
    method = "color",
    order = "hclust",
    addrect = 5,
    p.mat = corr_sig$p,
    sig.level = 0.10
  )
}

table_eda <- function(df){

  #df <- data_df
  data(dentitio); df <- dentitio
  
  column_details <- skimr::skim(df) %>% as.data.frame()
  
  unique_count <-
    df %>%
    tibble::add_column(fn = "unique_count") %>%
    dplyr::summarise_all(dplyr::n_distinct) %>%
    tidyr::gather(key = "column", value = "unique_count")
  
  row_count <- df %>%
    tibble::add_column(fn = "row_count") %>%
    dplyr::summarise_all( ~ n()) %>%
    tidyr::gather(key = "column", value = "row_count")
  
  data_type <- df %>%
    tibble::add_column(fn = "data type") %>%
    dplyr::summarise_all( ~ class(.x)) %>%
    tidyr::gather(key = "column", value = "data_type")
  
  col_min <- df %>%
    tibble::add_column(fn = "min") %>%
    dplyr::summarise_all(min) %>%
    tidyr::gather(key = "column", value = "min")
  
  col_max <- df %>%
    tibble::add_column(fn = "max") %>%
    dplyr::summarise_all(max) %>%
    tidyr::gather(key = "column", value = "max")

  column_details <-
    unique_count %>%
    dplyr::inner_join(row_count) %>%
    dplyr::inner_join(data_type) %>%
    dplyr::inner_join(col_min) %>%
    dplyr::inner_join(col_max) %>%
    dplyr::filter(column != "fn") %>%
    dplyr::inner_join(column_details, by = c("column" = "skim_variable"))
  
  column_details %>% 
    dplyr::mutate(is_categorical = dplyr::if_else(unique_count < 10, TRUE, FALSE))
  
}

# unlikely this would be used
# save_data <- function(df){
#   session$userData$data <- df
# }