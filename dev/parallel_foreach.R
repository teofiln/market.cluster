library(doParallel)
library(flexclust)
library(tidyverse)

# Parallel
no_cores <- parallel::detectCores(logical=TRUE)-1  
cl <- parallel::makeCluster(no_cores)

#Register with foreach()
doParallel::registerDoParallel(cores=no_cores)  

load("data/data_df.Rda")

start_k = 2
end_k = 8
km_nrep=20
show_progress = TRUE
max_iter=200
set.seed(12345)
seeds <- round(runif(nrep,min=10000,max=99999),0)
method="kmeans"
kcca_ctl <- new("flexclustControl")
kcca_ctl@iter.max <- max_iter

km <- stepFlexclust(as.matrix(data_df), start_k:end_k, nrep = km_nrep,
                    family=flexclust::kccaFamily(method),
                    verbose = show_progress, control=kcca_ctl,
                    multicore=cl)



nrep=100
start_k = 2
end_k = 8
km_nrep=10
show_progress = TRUE
set.seed(12345)
seeds <- round(runif(nrep,min=10000,max=99999),0)
method="neuralgas"


#trials <- tibble(seed=seeds) %>% 
#  mutate(fit_km = map(seed,~stepcclust(as.matrix(data_df),start_k:end_k,seed=.x,nrep=km_nrep,
#                                       method=method, verbose=show_progress,
#                                       multicore=FALSE),))

trials <- tibble(seed=seeds)
trials$fit_km <- foreach(seed=seeds, .combine = "c") foreach::%dopar% {
  flexclust::stepcclust(as.matrix(data_df),start_k:end_k,seed=seed,nrep=km_nrep,
             method=method, verbose=show_progress,
             multicore=FALSE)
}
