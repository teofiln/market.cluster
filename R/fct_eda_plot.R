#'
#'
#'
#'

plot_responses <- function(data_df){
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  data_df %>% 
    gather(key="Variable",value="Response") %>% 
    ggplot(aes(y=Variable, x=Response)) + 
    geom_col() +
    ggtitle("Count of YES responses by question") +
    xlab("Count") +
    ylab("Question")
}

plot_responses_pct <- function(data_df){
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  data_df %>% 
    gather(key="variable",value="response") %>%
    group_by(variable) %>% 
    summarise(response_pct = sum(response)/n()) %>% 
    ggplot(aes(y=variable, x = response_pct)) + 
    geom_col() +
    ggtitle("Proportion of YES responses by question") +
    xlab("Proportion") +
    ylab("Question")
}

# PCA 

print_summary_pca <- function(data_df){
  # Reduce dimension using PCA
  pca_df <- prcomp(data_df)
  # Summary of PCA
  summary(pca_df)
}

table_components <- function(data_df){
  library(dplyr)
  library(tibble)
  
  # Reduce dimension using PCA
  pca_df <- prcomp(data_df)
  components <- as.data.frame(signif(pca_df$rotation,3)) %>% rownames_to_column(var="Original")    
  return(components)
}

plot_pca <- function(data_df){
  library(ggplot2)
  library(ggfortify)
  # Reduce dimension using PCA
  pca_df <- prcomp(data_df)
  # Plot PCA
  autoplot(pca_df, col = "grey", loadings=TRUE, label = FALSE,  
           loadings.label = TRUE, loadings.label.repel = TRUE, loadings.colour = "grey") 
  
}

plot_tsne <- function(data_df, perp=50,theta=0.5,pca=TRUE,exag=0.5){
  library(Rtsne)
  
  unique_matrix <- unique(as.matrix(data_df))
  
  set.seed(params$seed)
  t_out <- Rtsne(unique_matrix, 
                 perplexity = perp, 
                 theta=theta, 
                 pca=pca, 
                 exaggeration_factor = exag)
  plot_out <- as.data.frame(t_out$Y)
  
  ggplot(plot_out, aes(x=V1, y=V2)) + 
    geom_point() +
    ggtitle(paste("tSNE - perp:",perp,"theta:",theta,"pca:",pca,"exag:",exag))
}


plot_pca_scree <- function(data_df,which="Eigen Values"){
  
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(forcats)
  library(ggfortify)
  
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
  
  if(!which %in% rn) stop("Invalid Parameter 'which'.")
  
  m %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    gather(key="component",value = "value",-rowname) %>% 
    filter(rowname==which) %>% 
    mutate(
      order = row_number(),
      component = fct_reorder(component,order)
      ) %>% 
    qplot(data=.,x=component,y=value, geom="col") +
    ggtitle(paste("PCA Scree Plot - ",which)) +
    ylab(which) + xlab("Component") -> p
  
  if(which == "Eigen Values"){
    p <- p + geom_hline(yintercept = 1, col="red")
  }
  
  if(which == "Cumulative Proportion"){
    p <- p + geom_hline(yintercept = c(0.7,0.8,0.9), col="red")
  }
  
  
  return(p)
}


plot_pairs <- function(data_df){

  library(corrplot)
  corr <- cor(data_df)
  
  corr_sig <- cor.mtest(data_df, conf.level = .95)

  corrplot(corr, 
           method = "color", 
           order="hclust", addrect = 5,
           p.mat = corr_sig$p, sig.level = 0.10)
  
}

table_eda <- function(df){

  #df <- data_df
  data(dentitio); df <- dentitio
  library(dplyr)
  library(skimr)
  library(tidyr)
  
  column_details <- skim(df) %>% as.data.frame()
  
  df %>% 
    add_column(fn="unique_count") %>%  
    summarise_all(n_distinct) %>% 
    gather(key="column", value="unique_count") -> unique_count

  df %>% 
    add_column(fn="row_count") %>%  
    summarise_all(~n()) %>% 
    gather(key="column", value="row_count") -> row_count

  df %>% 
    add_column(fn="data type") %>%  
    summarise_all(~class(.x)) %>% 
    gather(key="column", value="data_type") -> data_type

  df %>% 
    add_column(fn="min") %>%  
    summarise_all(min) %>% 
    gather(key="column", value="min") -> col_min

  df %>% 
    add_column(fn="max") %>%  
    summarise_all(max) %>% 
    gather(key="column", value="max") -> col_max


  unique_count %>% 
  inner_join(row_count) %>% 
    inner_join(data_type) %>% 
    inner_join(col_min) %>% 
    inner_join(col_max) %>% 
    filter(column != "fn") %>% 
    inner_join(column_details, by=c("column"="skim_variable")) -> column_details
    
  column_details %>% 
    mutate(is_categorical = if_else(unique_count < 10, TRUE, FALSE))
  
}

save_data <- function(df){
  params$data <<- df
}