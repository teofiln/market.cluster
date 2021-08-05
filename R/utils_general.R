hclust_kcca <- function(x, k, family=kccaFamily("kmeans"), weights=NULL,
                        group=NULL, control=list(dist_method = "euclidean", hclust_method = "complete"), 
                        simple=FALSE, save.data=FALSE)
{
  MYCALL <- match.call()
  
  # control <- as(control, "flexclustControl")
  
  if(!control$dist_method %in% c("euclidean", 
                                 "maximum", 
                                 "manhattan", 
                                 "canberra", 
                                 "binary", 
                                 "minkowski")) stop()
  
  if(!control$hclust_method %in% c("ward.D", 
                                   "ward.D2", 
                                   "single", 
                                   "complete", 
                                   "average",  # (= UPGMA), 
                                   "mcquitty", # (= WPGMA), 
                                   "median",   # (= WPGMC)
                                   "centroid")) stop()  # (= UPGMC).
  
  x <- as(x, "matrix")
  #x <- family@preproc(x)
  N <- nrow(x)
  
  # control = dist_method, hclust_method
  
  hc <- hclust(dist(x,method=control$dist_method),method=control$hclust_method)
  z <- as.kcca(hc,x,k=k)
  
  if(save.data)
    z@data <- ModelEnvMatrix(designMatrix=x)
  
  z
}




