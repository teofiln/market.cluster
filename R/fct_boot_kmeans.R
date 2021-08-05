
fit_boot_kmeans <- function(seed,
                        start_k,
                        end_k,
                        km_nrep,
                        boot_rep,
                        method = "kmeans"){

  library(flexclust)
  library(dplyr) 
  
  library(ggfortify) # Helper for autoplot
  library(broom)     # Helper for extracting PCA values
  
  # Parallel
  library(parallel)
  library(doParallel)
  no_cores <- parallel::detectCores(logical=TRUE)-1  
  MC <- parallel::makeCluster(no_cores, outfile="")
  
  #Register with foreach()
  doParallel::registerDoParallel(MC) #cores=no_cores)  
  

  if (is.na(method)) stop("No method specified")
  
  if (!method %in% c("kmeans", 
                     "kmedians",
                     "angle", 
                     "jaccard",
                     "ejaccard",
                     "hardcl",
                     "neuralgas",
                     "hclust")) { 
    if ( stringr::str_detect(method,"/") ) {
      # Nothing
    } else {
      stop("invalid method") 
    }  
  }
  
  show_progress = TRUE
  
  # Bootstrap boot_rep times and see the stability
  set.seed(seed)
  
  warning("method:", method,"  boot_rep:",boot_rep, " km_nrep:",km_nrep)
  
  if(method %in% c("kmedians",
                   "angle", 
                   "jaccard",
                   "ejaccard")) {

    warning("running booFlexclust kcca ...")
    boot_km <- bootFlexclust(as.matrix(data_df), start_k:end_k, nrep = km_nrep, 
                             nboot = boot_rep, verbose=show_progress,
                             FUN=flexclust::kcca, family=flexclust::kccaFamily(method),
                             multicore=MC)
    
  } else if(method == "hclust"){
    boot_km <- bootFlexclust(as.matrix(data_df), start_k:end_k, nrep = 1, 
                             nboot = boot_rep, verbose=show_progress,
                             FUN=hclust_kcca, family=flexclust::kccaFamily(method),
                             multicore=FALSE)
    
  } else if ((method %in% c("kmeans",
                            "hardcl", 
                            "neuralgas"))){
    
    warning("running booFlexclust cclust ...")
    boot_km <- bootFlexclust(as.matrix(data_df), start_k:end_k, nrep = km_nrep, 
                             nboot = boot_rep, verbose=show_progress,
                             FUN=flexclust::cclust, method=method,
                             multicore=MC)
    
  } else if (method == "Fast Kmeans / Euclidean") {
    warning("running booFlexclust cclust ...")
    boot_km <- bootFlexclust(as.matrix(data_df), start_k:end_k, nrep = km_nrep, 
                             nboot = boot_rep, verbose=show_progress,
                             FUN=flexclust::cclust, method="kmeans", dist="euclidean",
                             multicore=MC)
    
  } else {
    fam = params_to_kccaFamily()
    ctl = params_to_flexclustControl()
    boot_km <- bootFlexclust(as.matrix(data_df), start_k:end_k, nrep = km_nrep, 
                             nboot = boot_rep, verbose=show_progress,
                             FUN=flexclust::kcca, family=fam, control=ctl,
                             multicore=MC)
  }
  warning("...done")
  
  # This take a very long time so do only 1 time.
  # After process finished can show the following plots of this result
  
  
  # Close cluster
  parallel::stopCluster(MC)
  
  
  return(boot_km)
  
}

generate_trials <- function(seed,
                            start_k,
                            end_k,
                            km_nrep,
                            fit_nrep,
                            method = "kmeans"){
  library(tidyr)
  library(purrr)
  library(dplyr)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(iterators)
  
  show_progress = TRUE
  
  # Bootstrap boot_rep times and see the stability
  
  warning("method:", method,"  fit_nrep:",fit_nrep)
  
  # Parallel
  no_cores <- parallel::detectCores(logical=TRUE)-1  
  MC <- parallel::makeCluster(no_cores, outfile="")
  
  #Register with foreach()
  doParallel::registerDoParallel(MC) #cores=no_cores)  
  
  
  nrep = fit_nrep
  set.seed(seed)
  seeds <- round(runif(nrep,min=10000,max=99999),0)
  
  if(method %in% c("kmeans","hardcl","neuralgas")){
    warning("running stepcclust ...")
    trials <- tibble(seed=seeds) 
    trials$fit_km <- foreach(seed = seeds, .combine="c", .export=c("data_df"), .packages="flexclust") %dopar% {
      flexclust::stepFlexclust(as.matrix(data_df),start_k:end_k,seed=seed,nrep=km_nrep,
                    FUN=flexclust::cclust,
                    method=method, verbose=show_progress, multicore=FALSE)
    } 
  } else if(method=="hclust"){
    warning("running stepFlexclust hclust_kcca ...")
    trials <- tibble(seed=seeds) 
    trials$fit_km <- foreach(seed = seeds, .combine="c", .export=c("data_df"), .packages="flexclust") %dopar% {
      flexclust::stepFlexclust(as.matrix(data_df),start_k:end_k,seed=seed,nrep=1,
                    FUN=hclust_kcca, 
                    family=flexclust::kccaFamily(method), multicore=FALSE)
    }
  } else if (method %in% c("kmedians",
                           "angle", 
                           "jaccard",
                           "ejaccard")){
    
    warning("running stepFlexclust ...")
    trials <- tibble(seed=seeds) 
    trials$fit_km <- foreach(seed = seeds, .combine="c", .export=c("data_df"), .packages="flexclust") %dopar% {
      flexclust::stepFlexclust(as.matrix(data_df),start_k:end_k,seed=seed,nrep=km_nrep,
                    FUN=flexclust::kcca,
                    family=flexclust::kccaFamily(method), multicore=FALSE)
    }
  } else if (method == "Fast Kmeans / Euclidean") {
    warning("running stepFlexclust cclust ...")
    trials <- tibble(seed=seeds) 
    trials$fit_km <- foreach(seed = seeds, .combine="c", .export=c("data_df"), .packages="flexclust") %dopar% {
      flexclust::stepFlexclust(as.matrix(data_df),start_k:end_k,seed=seed,nrep=km_nrep,
                    FUN=flexclust::cclust,
                    method="kmeans", dist="euclidean",verbose=show_progress, multicore=FALSE)
    }
    
  } else {
    warning("running stepFlexclust kcca...")
    kcca_fam = params_to_kccaFamily()
    kcca_ctl = params_to_flexclustControl()
    trials <- tibble(seed=seeds) 
    trials$fit_km <- foreach(seed = seeds, .combine="c", .export=c("data_df"), .packages="flexclust") %dopar% {
      
      flexclust::stepFlexclust(as.matrix(data_df),start_k:end_k,seed=seed,nrep=km_nrep,
                    FUN=flexclust::kcca,
                    family=kcca_fam, control=kcca_ctl, multicore=FALSE)
    }
  }
  warning("... Done")
  
  # Close cluster
  parallel::stopCluster(MC)
  
  return(trials)
}

# Check if data will make kde2d fail 
check_kde2d <- function(rank_12){
  
  message("check_kde2d...")
  rank_12 %>% 
    group_by(k) %>%  
    summarise(x=n_distinct(k_assign_1),
              y=n_distinct(k_assign_2),
              hx = MASS::bandwidth.nrd(k_assign_1),
              hy = MASS::bandwidth.nrd(k_assign_2)) -> check_kde2d
  
  message("n_distinct(k_assign_1):",paste(check_kde2d$x,collapse = ","),"\n")
  message("n_distinct(k_assign_2):",paste(check_kde2d$y,collapse = ","),"\n")
  message("hx:",paste(check_kde2d$hx,collapse = ","),"\n")
  message("hy:",paste(check_kde2d$hy,collapse = ","),"\n")
  
  check_kde2d %>% 
    filter(hx == 0 | hy == 0) %>% 
    tally() %>% pull(n) -> error_count
  
  cat("check_kde2d:error_count=",error_count,"\n",file=stderr())

  check_kde2d %>% 
    filter(hx == 0 | hy == 0) %>% pull(k) %>% return()
  
  message("...done")
  
}

fit_reps_kmeans <- function(seed,
                            start_k,
                            end_k,
                            km_nrep,
                            fit_nrep,
                            method = "kmeans"){
  
  library(tidyr)
  library(purrr)
  library(dplyr)

  trials <- generate_trials(seed=seed,
                            start_k=start_k,
                            end_k=end_k,
                            km_nrep=km_nrep,
                            fit_nrep=fit_nrep,
                            method = method)
  
  trials %>% 
    mutate(model = map(fit_km,"models")) %>% 
    unnest(model) %>% 
    mutate(k = map_int(model,"k"),
           clusinfo = map(model,~`@`(.x,"clusinfo"))) %>% 
    dplyr::select(-fit_km, -model) %>% 
    unnest(clusinfo) %>% 
    # Order by n of each segment
    arrange(seed,k, desc(size)) %>% 
    group_by(seed,k) %>% 
    # Re-number the segments
    mutate(k_assign = row_number()) %>%
    # Pick top 2
    filter(k_assign < 3) %>% 
    dplyr::select(-av_dist,-max_dist,-separation) %>% 
    spread(key=k_assign,sep="_",value = size) -> rank_12 
  
  
  return(rank_12)
  
}



print_summary_boot_km <- function(boot_km){
  # Render Print
  flexclust::summary(boot_km)
}

# Shiny output - Global Stability Plot

plot_boot_km_boxplot <- function(boot_km){
  # Render box Plot
  boxplot(boot_km, 
          xlab = "number of segments (k)",
          ylab = "adjusted Rand index",
          main= paste("Global Stability Plot",params$method))
}

plot_boot_km_density <- function(boot_km){
  # Render density plot
  densityplot(boot_km, 
              xlab = "number of segments (k)",
              ylab = "adjusted Rand index",
              main=paste("Global Stability Plot",params$method))
}

rank_12_xyz <- function(rank_12){

  message("rank_12_xyz...")
  
  # Check if kde2d will fail on any K solution
  failed_k <- check_kde2d(rank_12)
  
  message("k:",paste(rank_12$k,collapse = ","))
  message("failed_k:",paste(failed_k,collapse = ","))
  
  safe_kde2d = safely(MASS::kde2d,otherwise = "FAILED")
  
  # Generate xyz using kde2d
  rank_12 %>% 
    # Filter out K solution that will fail kde
    # filter(!k %in% failed_k) %>%
    group_by(k) %>%     
    nest() %>% 
    mutate( s2d_result = map(data,~safe_kde2d(.x$k_assign_1, .x$k_assign_2, n=100)),
            s2d_summary = map(s2d_result,
                          function(SR){
                            S <- SR$result
                            if (class(S) != "character"){
                              message("s2d_dim : ",class(S), " elements=",length(S))
                              message("length(x) : ",length(S$x))
                              message("length(y) : ",length(S$y))
                              message("dim(z) : ",dim(S$z)[1]," X ",dim(S$z)[2])
                              message("max(s2d$z) :",max(S$z))
                              message("min(s2d$z) :",min(S$z))
                              message("which max :", which(S$z == max(S$z)))
                              message("S$x[which_max %% 100 +1] : [",  S$x[which(S$z == max(S$z)) %% 100 +1],"]")
                              message("S$y[which_max %/% 100 +1] : [", S$y[which(S$z == max(S$z)) %/% 100 +1],"]")
                              
                              tibble(nx = length(S$x),
                                     ny = length(S$y),
                                     nz1 = dim(S$z)[1],
                                     nz2 = dim(S$z)[2],
                                     min_z = min(S$z),
                                     max_z = max(S$z),
                                     peak_height = max_z,
                                     which_max = which(S$z == max(S$z)),
                                     size1_peak_at = S$x[(which_max %% 100)+1],
                                     size2_peak_at = S$y[which_max %/% 100 +1]
                              )
                              
                            } else {
                              message("No result from kde2d()")
                              
                              tibble( nx = 0,
                                      ny = 0,
                                      nz1 = 0,
                                      nz2 = 0,
                                      min_z = 0,
                                      max_z = 1e6,
                                      peak_height = max_z,
                                      which_max = 0,
                                      size1_peak_at = 0,
                                      size2_peak_at = 0)
                            }
                          })
    ) %>% unnest(s2d_summary) %>% 
    arrange(desc(max_z)) -> xyz

  message("...done")
  
  return(xyz)
  
}

rank_12_distance <- function(rank_12, xyz){
  
  # join with rank_12 for final df
  rank_12 %>% 
    inner_join(xyz, by=c("k")) %>% 
    mutate(
      dist = sqrt((k_assign_1 - size1_peak_at)^2 + (k_assign_2 - size2_peak_at)^2)
    ) %>% 
    arrange(dist) -> rank_12_dist
  return(rank_12_dist)
}


plot_top2_boot <- function(boot_km, scale_free=TRUE, without_k2=FALSE){
  
  library(tidyr)
  library(purrr)
  library(MASS)
  library(dplyr)
  
  # Unpack cluster1 from bootFlexclust object into df
  fit_array <- boot_km@cluster1
  
  nrows = dim(fit_array)[1]
  k_idx = dim(fit_array)[2]
  boot_max = dim(fit_array)[3]
  
  df <- tibble()
  for (k_id in seq_along(1:k_idx)) {
    for (boot in seq_along(1:boot_max)){
      #print(tibble(row_id = fit_array[,k,boot], k, boot))
      this_df <- tibble(cluster_assignment = fit_array[,k_id,boot], k_id, boot)
      if (k_id==1 & boot==1){
        df <- this_df
      } else {
        df <- rbind(df,this_df)
      }
    }
  }
  
  df %>% 
  group_by(boot, k_id, cluster_assignment) %>% 
    tally() %>% 
    arrange(boot, k_id, desc(n)) %>% 
    group_by(boot, k_id) %>% 
    mutate( k_assign = row_number(),
            k = n()) %>% 
    filter(k_assign <= 2) %>% ungroup() %>% 
    dplyr::select(k, k_assign, n, boot) %>% 
    spread(key=k_assign, value = n, sep="_") -> rank_12

  # Filter out k2 if necessary
  rank_12 <- rank_12 %>% filter(k>1+without_k2)

  # 
  xyz <- rank_12_xyz(rank_12)
  
  # Plot stability of top 2 segments with density using xyz
  rank_12_distance(rank_12, xyz) %>% 
    ggplot(aes(x=k_assign_1, y=k_assign_2)) + 
    geom_density2d_filled(n=100, h=NULL, adjust=1) +
    #geom_point(alpha=0.3, col="grey") + 
    geom_point(data=xyz, aes(x=size1_peak_at, y=size2_peak_at), color="red") +
    facet_wrap(~k,scales=if_else(scale_free,"free","fixed")) +
    geom_text(aes(x=size1_peak_at,
                  y=size2_peak_at+50,
                  label=round(peak_height*1e8,0)),col="white") +
    ggtitle(paste("Stability of top 2 segments",params$method)) +
    xlab("n of segment 1") +
    ylab("n of segment 2")
}


# Generate solutions for K
# Given data_df and k and config of kmeans
# Return list of n_solutions of cluster assignments


plot_top2_reps <- function(rank_12,scale_free=TRUE, without_k2=FALSE, type=1){

  library(tidyr)
  library(purrr)
  library(dplyr)

  # Check if kde2d will fail
  failed_k <- check_kde2d(rank_12)
  
  # Filter out k2 if necessary
  rank_12 <- rank_12 %>% filter(k>1+without_k2)
  
  xyz <- rank_12_xyz(rank_12)

  rank_12_dist <- rank_12_distance(rank_12,xyz)

  # Hard code type 2 chart  
  type = 2
  if (type == 1) {
    # Plot Use density
    rank_12 %>% 
      ggplot(aes(x=k_assign_1, y=k_assign_2)) + 
      geom_density2d_filled(n=100, h=NULL, adjust=1) +
      #geom_point(alpha=0.3, col="grey") + 
      geom_point(data=xyz, aes(x=size1_peak_at, y=size2_peak_at), color="red") +
      facet_wrap(~k,scales=if_else(scale_free,"free","fixed"))
    
  } else if (type == 2) {

    # Plot Use density with peak height
    rank_12_dist %>% 
      ggplot(aes(x=k_assign_1, y=k_assign_2)) + 
      geom_density2d_filled(n=100, h=NULL, adjust=1) +
      #geom_point(alpha=0.3, col="grey") + 
      geom_point(data=xyz, aes(x=size1_peak_at, y=size2_peak_at), color="red") +
      facet_wrap(~k,scales=if_else(scale_free,"free","fixed")) +
      geom_text(aes(x=size1_peak_at,
                    y=size2_peak_at+50,
                    label=round(peak_height*1e8,0)),col="white") +
      ggtitle(paste("Stability of top 2 segments",params$method)) +
      xlab("n of segment 1") +
      ylab("n of segment 2")
    
    
  } else {
    
    # Custom plot
    xyz %>% ungroup() %>% 
      filter(k>1+without_k2) %>% 
      mutate(x=map(s2d,"x"),
             y=map(s2d,"y"),
             z=map(s2d,"z"),
             xyz = pmap(list(x,y,z), function(X,Y,Z){
               xx = tibble(x_id = 1:length(X),X)
               yy = tibble(y_id = 1:length(Y),Y)
               Z %>% as_tibble() %>% 
                 mutate(x_id=row_number()) %>% 
                 gather(key="y",value="z",-x_id) %>% 
                 mutate(y = as.integer(stringr::str_extract(y,"[0-9]+"))) %>%
                 rename(y_id=y) %>% 
                 inner_join(crossing(xx,yy))
             })) %>% 
      dplyr::select(k,xyz) %>% 
      unnest(xyz) %>% 
      ggplot(aes(x=X,y=Y,col=z)) + 
      geom_point() + 
      geom_point(data=xyz, aes(x=size1_peak_at, y=size2_peak_at), color="red") +
      geom_point(data=xyz %>% unnest(data), aes(x=k_assign_1, y=k_assign_2), size=0.5, alpha=0.3, col="grey") +
      facet_wrap(~k,scales=if_else(scale_free,"free","fixed")) 
    
  }
  
}

# Search from 100 random points and find the closest to the peak of reps_12
plot_many_segment_profiles <- function(k=2, n=3){
  
  #k=4;n=3
  library(tidyr)
  library(purrr)
  library(dplyr)
  
  # Repeat boot_rep times and see the stability

  cat(file=stderr(),"plot_many_segment_profiles()\n")
  
  # Generate nrep trials
  nrep = 100

  warning("Run nrep trials...")
  trials <- generate_trials(seed=params$seed,
                            start_k=k,
                            end_k=k,
                            km_nrep=1,
                            fit_nrep=nrep,
                            method = params$method)

  warning("relabel...")
  # relabel fit_km in place
  # Create a stepFlexclust object from trials so that relabel() can work on it
  reps_km_model <- new("stepFlexclust", 
                       models=trials$fit_km, 
                       k=as(k, "integer"),
                       nrep=as(nrep, "integer"), 
                       call=match.call())
  reps_km_model <- relabel(reps_km_model)
  
  # Replace into reps_km_model
  trials$fit_km <- reps_km_model@models
  
  # Create 2D density x,y,z
  warning("rank12...")
  trials %>% 
    mutate(k = map_int(fit_km,~`@`(.x,"k")),
           clusinfo = map(fit_km,~`@`(.x,"clusinfo"))) %>% 
    dplyr::select(-fit_km) %>% 
    unnest(clusinfo) %>% 
    # Order by n of each segment
    arrange(seed,k, desc(size)) %>% 
    group_by(seed,k) %>% 
    # Re-number the segments
    mutate(k_assign = row_number()) %>%
    # Pick top 2
    filter(k_assign < 3) %>% 
    dplyr::select(-av_dist,-max_dist,-separation) %>% 
    spread(key=k_assign,sep="_",value = size) -> rank_12 
  warning("...done")
  
  # Check if kde2d will fail
  failed_k <- check_kde2d(rank_12) 
  
  xyz <- rank_12_xyz(rank_12)
  

  # Calculate distances of each solution to the center
  rank_12_dist <- rank_12_distance(rank_12,xyz)
  
  # Choose the top n seeds
  rank_12_dist %>% ungroup() %>%  
    head(n) %>% pull(seed) -> seeds
  
  # Return the trials chosen by seeds
  trials %>% 
    filter(seed %in% seeds) 
}

plot_profile <- function(solutions,i){
  
  message("plot_profile...")
  message("class solutions:",class(solutions))
  message("class solutions$fit_km:", class(solutions$fit_km))
  message("class solutions$fit_km[1]:", class(solutions$fit_km[1]))
  message("class solutions$fit_km[1][[1]]:", class(solutions$fit_km[1][[1]]))
  message("i = ",i)
  
  this_solution = solutions$fit_km[i][[1]]
  k <- this_solution@k
  seed = solutions$seed[i]
  pop_av_dist <- with(this_solution@clusinfo, sum(size*av_dist)/sum(size))
  main_txt <- paste("kcca ", "Av Dist = ", format(pop_av_dist, digits = 5), params$method, " - ",k,"seed=",seed)
  
  label_clust <- hclust(dist(t(as.matrix(data_df))))
  
  # Activity Profiles for each segment
  barchart(this_solution, 
           which=label_clust$order,
           main = main_txt, strip.prefix = "#",
           scales = list(cex = 0.6),
           shade = TRUE,
           legend = TRUE
  )
}  


plot_solution <- function(solutions,i,hull=TRUE,neighborhood=TRUE){
  
  message("plot_solution...")
  message("class solutions:",class(solutions))
  message("class solutions$fit_km:", class(solutions$fit_km))
  message("class solutions$fit_km[1]:", class(solutions$fit_km[1]))
  message("class solutions$fit_km[1][[1]]:", class(solutions$fit_km[1][[1]]))
  message("i = ",i)
  
  this_solution = solutions$fit_km[i][[1]]
  k = this_solution@k
  seed = solutions$seed[i]
  pop_av_dist <- with(this_solution@clusinfo, sum(size*av_dist)/sum(size))
  main_txt <- paste("kcca ", "Av Dist = ", format(pop_av_dist, digits = 5), params$method, " - ",k,"seed=",seed)
  
  pca <- prcomp(data_df)
  flexclust::plot(this_solution, project = pca, data = as.matrix(data_df),
       hull = hull, simlines = neighborhood,
       xlab = "principal component 1",
       ylab = "principal component 2",
       main=main_txt)
  projAxes(pca)
  
}  


