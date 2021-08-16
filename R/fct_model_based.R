fit_flexmix <- function(seed = 12345,
                        start_k = 2,
                        end_k = 8,
                        nrep = 10,
                        method = "Bernoulli Distribution") {
  data_mat <- as.matrix(data_df)
  
  # 1. Which is the suitable probability distribution?
  #   (multi) Bernoulli distribution
  # 2. How many subpopulations should we consider?
  #   Let's try from 2 to 8 clusters and pick by BIC.
  # 3. Which are the parameters and their estimations?
  # Each p(probability) for each column. Also the proportions for each column.
  
  # Search for K
  
  show_progress = TRUE
  
  if (method == "Bernoulli Distribution") {
    set.seed(seed)
    fit <- flexmix::stepFlexmix(
      data_mat ~ 1,
      model = flexmix::FLXMCmvbinary(),
      k = start_k:end_k,
      nrep = nrep,
      verbose = show_progress,
      control = list(tolerance = 1e-15, iter.max = 5000)
    )
    
  } else if (method == "Normal Distribution") {
    set.seed(seed)
    fit <- flexmix::stepFlexmix(
      data_mat ~ 1,
      model = flexmix::FLXMCmvnorm(),
      k = start_k:end_k,
      nrep = nrep,
      verbose = show_progress,
      control = list(tolerance = 1e-15, iter.max = 5000)
    )
    
  } else if (method == "Poisson Distribution") {
    set.seed(seed)
    fit <- flexmix::stepFlexmix(
      data_mat ~ 1,
      model = flexmix::FLXMCmvpois(),
      k = start_k:end_k,
      nrep = nrep,
      verbose = show_progress,
      control = list(tolerance = 1e-15, iter.max = 5000)
    )
  } else {
    stop("Metod is not implemented yet")
  }
  return(fit)
}

plot_flexmix_scree <- function(fm_model){
  flexmix::plot(fm_model,
                main = paste("Method",params$method),
                ylab = "value of information criteria (AIC, BIC, ICL)")
}

print_flexmix_summary <- function(fm_model){
  show(fm_model)
}

plot_flexmix_parameters <- function(fm_model, k){
  
  if (length(fm_model) == 0)
    return(NA)
  message("length of fm_model > 0")
  message("class(fm_model)", class(fm_model))
  message("class(getModel(fm_model,1))", class(getModel(fm_model, 1)))
  
  tidy_params <-
    flexclust::parameters(flexclust::getModel(fm_model, as.character(k))) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "factors") %>%
    tidyr::gather(key = "cluster", value = "probability", -factors)

  # Order labels by hclust  
  labels <- unique(tidy_params$factors)
  lab_order <- hclust(dist(t(as.matrix(data_df))))$order
  
  tidy_params %>%
    dplyr::mutate(factors = factor(factors, levels = labels[lab_order])) %>%
    ggplot2::ggplot(ggplot2::aes(x = factors, y = probability)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap( ~ cluster) +
    ggplot2::coord_flip()
}

print_flexmix_parameters <- function(fm_model, k){
  flexclust::parameters(flexclust::getModel(fm_model,as.character(k)))
}