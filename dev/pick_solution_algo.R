set.seed(SEED) # For reproducibility
MD_km4444 <- stepFlexclust(MD_x, c(4,4,4,4), nrep = 20,
                         verbose = FALSE)
MD_km4444 <- relabel(MD_km4444)

MD_km4444@k

MD_km4444@models




shuffleTT <- function(n) list(train1=sample(1:n, replace=FALSE),
                           train2=sample(1:n, replace=FALSE),
                           test=1:n)

randEval <- function(c1,c2)
  doRandIndex(table(c1,c2), correct=TRUE, original=TRUE)

repeatedScheme <- new("resampleScheme",
                  traintest = shuffleTT,
                  validate = flexclust:::randEval,
                  valname = c("ARI", "RI"))

MD_km_reps <- flexclust:::resampleFlexclust(x=as.matrix(MD_x),k=4,scheme = repeatedScheme)

MD_km_reps
methods(class="resampleFlexclust")
show(MD_km_reps)
summary(MD_km_reps)

MD_km_reps@centers1[[1]] %>% dim() # 4 segments, 11 vars, 100 boots
MD_km_reps@centers2
MD_km_reps@cluster1[[2]] %>% dim() # list of cluster assignments for each boot 
MD_km_reps@cluster2 
MD_km_reps@index1[[1]] %>%  length()# list of row index in each boot
MD_km_reps@index2
MD_km_reps@indextest[[1]] %>% length() # list of row index in test set
MD_km_reps@validation # array, metrix components, 1, boot


# Convert resampleFlexclust/bootFlexclust@centers1/2 to dataframe
centers2df <- function(repeated_centers){
  v <- dim(repeated_centers)
  n_seg = v[1]
  n_var = v[2]
  n_boot = v[3]
  m <- matrix(0,nrow=n_seg*n_boot,ncol=n_var+2)
  colnames(m) <- c("boot","seg",as.character(1:v[2]))
  
  i = 0
  for(boot_id in 1:n_boot){ # boot
    for(seg_id in 1:n_seg){ # segment
      i = i+1
      m[i,1] = boot_id
      m[i,2] = seg_id
      for(var_id in 1:n_var){# columns
        m[i,var_id+2] =  repeated_centers[seg_id,var_id,boot_id]
      }
    }
  }
  
  as.data.frame(m)
}

# Calculate summary metric of the centers
mean_centers <- function(resample_res, summary_fun=mean){
  
  centers2df(resample_res@centers1[[1]]) %>% 
    select(-boot) %>% 
    drop_na() %>% 
    group_by(seg) %>% 
    summarise_all(summary_fun %>% 
    select(-seg) %>% as.matrix
}

# Distance of resample results to the solutions
resample_centers_distance <- function(resample_res, solutions){
  # Mean centers of resample
  mean_center <- mean_centers(resample_res)
  
  # Many solutions
  centers <- centers2df(solutions@centers1[[1]])
  
  centers %>% 
    rename(solution_id = boot) %>% 
    group_by(solution_id) %>%
    select(-seg) %>% 
    nest() %>% 
    mutate(
      dist = map(data,function(CENTERS){
        if(identical(dim(CENTERS),dim(mean_center))){
          sqrt(rowMeans(CENTERS - mean_center)^2)
        } else {
          NA
        }
      })
    ) %>% select(-data) %>% 
    unnest(dist) %>%
    group_by(solution_id) %>% 
    summarise(mean_dist = mean(dist)) %>% 
    arrange(mean_dist)
  
}


clusters2df <- function(resample_res){
  
  for(i in 1:length(resample_res@cluster1)){
    df1 <- table(resample_res@cluster1[[i]]) %>% 
      as.data.frame() %>% 
      rename(seg=1,n=2) %>% 
      add_column(solution_id = i)
    if(i == 1){
      df <- df1
    } else {
      df <- rbind(df,df1)
    }
  }
  
  return(df)
}

# Calculate top 2 mean
mean_top2 <- function(resample_res, summary_fun=mean){
  
  df <- clusters2df(resample_res)
  
  df %>% 
    arrange(solution_id,desc(n)) %>% 
    group_by(solution_id) %>% 
    mutate( seg_rank = row_number()) %>% 
    filter(seg_rank <= 2) %>% 
    group_by(seg_rank) %>% 
    summarise(summary = summary_fun(n))
}

# Distance of resample resultsof top 2 to the solutions
resample_top2_distance <- function(resample_res, solutions){
  # Mean centers of resample
  resample_top2 <- mean_top2(resample_res)
  
  # Many solutions
  clusters <- clusters2df(solutions)
  
  clusters %>% 
    arrange(solution_id,desc(n)) %>% 
    group_by(solution_id) %>% 
    mutate( seg_rank = row_number()) %>% 
    filter(seg_rank <= 2) %>% 
    inner_join(resample_top2) %>% 
    mutate(err = summary-n) %>%
    group_by(solution_id) %>% 
    summarise( dist = sqrt(mean(err^2))) %>% 
    arrange(dist)
}

#######


MD_km_boot <- flexclust:::resampleFlexclust(x=as.matrix(MD_x),k=4,scheme = flexclust:::bootScheme)
MD_km_reps <- flexclust:::resampleFlexclust(x=as.matrix(MD_x),k=4,scheme = repeatedScheme)

as(MD_km_reps,"stepFlexclust")

flexclust::relabel
showMethods("relabel")

MD_km28 <- stepFlexclust(x=as.matrix(MD_x),k=2:8)
MD_km28@models[[1]]

MD_km28[[1]]@cluster#first cluster assignment
MD_km28[[1]]@second #second cluster assignment
MD_km28[[1]]@index # null
MD_km28[[1]]@centers # centers for the solution
MD_km28[[1]]@cldist # every data row has 2 entries, dist to what?
MD_km28[[1]]@iter
MD_km28[[1]]@xrange # range of values for each variable 
MD_km28[[1]]@xcent # center of all data
MD_km28[[1]]@totaldist # distance of all rows to the center
MD_km28[[1]]@cldist[,1] # within cluster distance

  
solution_dist_to_mean_centers <- resample_centers_distance(MD_km_boot, MD_km_reps)
solution_dist_to_mean_centers
MD_km_reps@centers1[[1]][,,91]
mean_centers(MD_km_boot@centers1[[1]])

# 1. bootstrap 
# 2. take solution centers and take median of the centers
# 3. ?????

mean_top2(MD_km_boot)
mean_top2(MD_km_reps)

solution12 <- clusters2df(MD_km_reps) %>% 
  arrange(solution_id,desc(n)) %>% 
  group_by(solution_id) %>% 
  mutate( seg_rank = row_number()) %>% 
  filter(seg_rank <= 2) %>% 
  select(-seg) %>% 
  spread(key=seg_rank, value=n, sep="_")

mean_center12 = mean_top2(MD_km_boot) %>% spread(seg_rank,summary,sep="_")
mean_center12 = mean_top2(MD_km_reps) %>% spread(seg_rank,summary,sep="_")

solution12 %>% 
  qplot(data=.,x=seg_rank_1, y=seg_rank_1) +
  geom_point(data=mean_center12,aes(x=seg_rank_1,y=seg_rank_2),color="red", size=5)



resample_top2_distance(MD_km_boot, MD_km_reps) %>% 
  qplot(data=.,x=dist)


# How to pick list of closest solutions
