

fit_kmeans <- function(seed,start_k,end_k,nrep, max_iter=200, method="kmeans"){
  library(flexclust)
  library(dplyr) 
  
  MC=FALSE
  
  #shinyEventLogger::log_started(NULL,name="fit_kmeans")

  if (!method %in% c("kmeans", 
                     "kmedians",
                     #"angle", # does not work 
                     "jaccard",
                     "ejaccard",
                     "neuralgas",
                     "hardcl",
                     "hclust"
                     )) { stop("invalid method"); return(NULL) }
  
  show_progress = TRUE

  # Process kmeans 
  set.seed(seed)
  # start_k=2;end_k=8;nrep=30
  
  warning("method:", method)
  
  kcca_ctl <- new("flexclustControl")
  kcca_ctl@iter.max <- max_iter
  cclust_ctl <- new("cclustControl")
  cclust_ctl@iter.max <- max_iter
  
  if (method %in% c(#"kmeans", # Use cclust 
                    "kmedians",
                    "angle", # does not work 
                    "jaccard",
                    "ejaccard")) {
    
    warning("Running stepFlexclust...")
    km <- stepFlexclust(as.matrix(data_df), start_k:end_k, nrep = nrep,
                        family=kccaFamily(method),
                        verbose = show_progress, control=kcca_ctl,
                        multicore=MC)
    
  } else if (method=="hclust"){
    km <- stepFlexclust(as.matrix(data_df), k=start_k:end_k, nrep=1, 
                        FUN=hclust_kcca, control=kcca_ctl,
                        multicore=MC)
    
  } else { # hardcl, neuralgas, kmeans
    
    warning("Running stepcclust...")
    km <- stepcclust(as.matrix(data_df), k=start_k:end_k, nrep=nrep,
                     method=method,
                     verbose = show_progress, control=cclust_ctl,
                     multicore=MC)
  }
  
  warning("...done")
  # Relabel the segments so that it is repeatable
  km <- relabel(km)
  
  #shinyEventLogger::log_done(NULL,name="fit_kmeans")
  
  return(km)
}

params_to_flexclustControl <- function() {
  
  library(flexclust)
  
  # Control params for partition methods
  ctl <- new("flexclustControl")
  
  ctl@iter.max <- params$max_iter
  ctl@verbose=1
  
  if (params$classification == "Hard"){
    ctl@classify <- "hard"
  } else if (params$classification == "Simulated Annealing") {
    ctl@classify <- "simann"
  } else {
    ctl$classify <- "auto"
  }
  
  if(params$init_center == "Random") {
    ctl@initcent <- "randomcent"
  } else {
    ctl@initcent <- "kmeanspp"
  }
  
  return(ctl)
}

params_to_kccaFamily <- function(){

  library(flexclust)
  
  cluster_function <- kcca # default
  
  if (       params$distance == "Euclidean") {
    dist_fun = distEuclidean
  } else if (params$distance == "Manhattan") {
    dist_fun = distManhattan
  } else if (params$distance == "Max") {
    dist_fun = distMax
  } else if (params$distance == "Minkowski") {
    dist_fun = distMinkowski
  } else if (params$distance == "Angle") {
    dist_fun = distAngle
  } else if (params$distance == "Correlation") {
    dist_fun = distCor
  } else if (params$distance == "Canberra") {
    dist_fun = distCanberra
  } else if (params$distance == "Jaccard") {
    dist_fun = distJaccard
  } else {
    stop("Invalid distance function")
  }

  if ( params$centering_method == "Mean") {
    cent_method = centMean
  } else if ( params$centering_method == "Fast Kmeans") {
    cluster_function <- cclust
    cen_method = centMean
  } else if ( params$centering_method == "Median") {
    cent_method = centMedian
  } else if ( params$centering_method == "Angle") {
    cent_method = centAngle
  } else if ( params$centering_method == "L-BFGS-B") {
    cent_method = centOptim01 # used for Jaccard
  } else if ( params$centering_method == "Nelder-Mead") {  
    cent_method = centOptim
  } else {
    stop("Invalid centering function")
  }
  
  kcca_name = paste(params$centering_method,"/",params$distance)
  
  # Family of centering methods and distance functions
  fam <- kccaFamily( name=kcca_name,
                     dist=dist_fun, 
                     cent=cent_method)
  return(fam)
}

fit_partition <- function(){
  library(flexclust)
  
  MC = FALSE
  
  if (params$centering_method == "Fast Kmeans") { # Use cclust for fast kmeans

    # Distance function
    if (params$distance == "Euclidean") {
      cclust_dist = "euclidean"
    } else {
      cclust_dist = "manhattan"
    }
    # Centering method
    cclust_method = "kmeans"
    # Control 
    cclust_ctl = list(iter.max=params$max_iter, 
                      verbose=1)
    
    cluster_fit <- stepFlexclust(x=data_df, k=params$start_k:params$end_k, 
                                 verbose=TRUE, seed = params$seed, nrep=params$km_nrep,
                                 FUN = cclust, dist = cclust_dist, method = cclust_method, control = cclust_ctl,
                                 multicore=MC)
  } else {
    # Family of centering methods and distance functions
    kcca_fam <- params_to_kccaFamily()
    
    # Control params for partition methods
    kcca_ctl <- params_to_flexclustControl()
    
    cluster_fit <- stepFlexclust(x=data_df, k=params$start_k:params$end_k, 
                                 verbose=TRUE, seed = params$seed, nrep=params$km_nrep,
                                 FUN = kcca, family=kcca_fam, control=kcca_ctl,
                                 multicore=MC)
  }
  
  cluster_fit <- relabel(cluster_fit)
  return(cluster_fit)
}

print_summary_km <- function(km_model){
  show(km_model)
}


plot_scree <- function(km_model){
  flexclust::plot(km_model, 
                  xlab = "number of segments", 
                  main=paste("Scree Plot of Within Cluster Distances -",params$method))
}

plot_slsa <- function(km_model){
  flexclust::slsaplot(km_model)
  title(main=paste("SLSA Plot - ",params$method))
}

plot_neighborhood <- function(km_model,k=2) {

  # info(cl,"help")
  # info(cl,"size")
  # info(cl,"av_dist")
  # info(cl,"max_dist")
  # info(cl,"separation")
  # info(cl,"distsum")
  k_list = km_model@k
  k_id <- which(k_list==k)
  this_solution <- km_model[[k_id]]
  
  pop_av_dist <- with(this_solution@clusinfo, sum(size*av_dist)/sum(size))
  main_txt <- paste("kcca ","Av Dist = ", format(pop_av_dist, digits = 5), this_solution@family@name, " - ",k)
  
  df_pca <- prcomp(data_df)
  plot(this_solution, 
       data = as.matrix(data_df), 
       project = df_pca, which=1:2,
       points = TRUE, 
       main = main_txt
  )
}

plot_hull <- function(km_model,k=2) {
  
  # info(cl,"help")
  # info(cl,"size")
  # info(cl,"av_dist")
  # info(cl,"max_dist")
  # info(cl,"separation")
  # info(cl,"distsum")
  k_list = km_model@k
  k_id <- which(k_list==k)
  this_solution <- km_model[[k_id]]
  
  pop_av_dist <- with(this_solution@clusinfo, sum(size*av_dist)/sum(size))
  main_txt <- paste("kcca ","Av Dist = ", format(pop_av_dist, digits = 5), this_solution@family@name, " - ",k)
  
  df_pca <- prcomp(data_df)
  image(this_solution,  
        graph=TRUE,
        main = main_txt, data=as.matrix(data_df))
}

plot_segment_profile <- function(km_model, k=2){
  k_list = km_model@k
  k_id <- which(k_list==k)
  this_solution <- km_model[[k_id]]
  
  pop_av_dist <- with(this_solution@clusinfo, sum(size*av_dist)/sum(size))
  main_txt <- paste("kcca ", this_solution@family@name, " - ",k)
  
  label_clust <- hclust(dist(t(as.matrix(data_df))))
  
  # Activity Profiles for each segment
  barchart(this_solution, 
           which=label_clust$order,
           main = main_txt, strip.prefix = "#",
           scales = list(cex = 0.6),
           shade = TRUE,
           legend = TRUE,
           sub = paste("\nAv Dist = ", format(pop_av_dist, digits = 5),
                       ", k = ", k, sep = "")
  )
  
}

plot_segment_stability_within_solution <- function(km_model, k=2) {
  
  library(flexclust)
  
  # Parallel
  library(parallel)
  library(doParallel)
  no_cores <- parallel::detectCores(logical=TRUE)-1  
  MC <- parallel::makeCluster(no_cores, outfile="")
  
  k_list = km_model@k
  k_id <- which(k_list==k)
  this_solution <- km_model[[k_id]]

  method = params$method
  if(method %in% c("kmedians",
                   "angle", 
                   "jaccard",
                   "ejaccard")) {
    
    warning("running slswFlexclust kcca ...")
    slsw_df <- slswFlexclust(as.matrix(data_df), this_solution,
                             FUN=flexclust::kcca, family=flexclust::kccaFamily(method),
                             multicore=MC)
    
  } else if(method == "hclust"){
    warning("running slswFlexclust hclust_kcca ...")
    slsw_df <- slswFlexclust(as.matrix(data_df), this_solution,
                             FUN=hclust_kcca, family=flexclust::kccaFamily(method),
                             multicore=FALSE)
    
  } else if ((method %in% c("kmeans",
                            "hardcl", 
                            "neuralgas"))){
    
    warning("running slswFlexclust cclust ...")
    slsw_df <- slswFlexclust(as.matrix(data_df), this_solution,
                             FUN=flexclust::cclust, method=method,
                             multicore=MC)
    
  } else if (method == "Fast Kmeans / Euclidean") {
    warning("running slswFlexclust cclust ...")
    slsw_df <- slswFlexclust(as.matrix(data_df), this_solution,
                             FUN=flexclust::cclust, method="kmeans", dist="euclidean",
                             multicore=MC)
    
  } else {
    fam = params_to_kccaFamily()
    ctl = params_to_flexclustControl()
    warning("running slswFlexclust kcca ...")
    slsw_df <- slswFlexclust(as.matrix(data_df), this_solution,
                             FUN=flexclust::kcca, family=fam, control=ctl,
                             multicore=MC)
  }
  
  # Close cluster
  parallel::stopCluster(MC)
  message("Done...")
  
  
  main_txt <- paste("Segment Stability Within Solution K ",k, "using", method)
  
  
  flexclust::plot(slsw_df, ylim = 0:1, 
       xlab = "segment number",
       ylab = "segment stability",
       main = main_txt)
  
}

plot_gorge <- function(km_model, k=2) {
  
  k_list = km_model@k
  k_id <- which(k_list==k)
  this_solution <- km_model[[k_id]]
  main_txt <- paste("kcca ", this_solution@family@name, " - ",k)
  
  histogram(this_solution, data = data_df, 
            xlim = 0:1,
            main=main_txt)
  
}

