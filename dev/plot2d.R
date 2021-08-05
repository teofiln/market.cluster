
# Bootstrap kmeans
fit_boot <- fit_boot_kmeans(12345,2,8,10,100)


# cluster assignments
fit_array <- fit_boot@cluster1

#cluster1[row_id, k_idx, boot] = cluster assignment of row_id for k_idx at boot

nrows = dim(fit_array)[1]
k_idx = dim(fit_array)[2]
boot_max = dim(fit_array)[3]

df1 <- tibble()
k_id=1; boot=3
for (k_id in 1:k_idx) {
  for (boot in 1:boot_max){
    #print(tibble(row_id = fit_array[,k_id,boot], k_id, boot))
    this_df <- tibble(cluster_assignment = fit_array[,k_id,boot], k_id, boot)
    if (k_id==1 & boot==1){
      df1 <- this_df
    } else {
      df1 <- rbind(df1,this_df)
    }
  }
}

fit_array <- fit_boot@cluster2

nrows = dim(fit_array)[1]
k_idx = dim(fit_array)[2]
boot_max = dim(fit_array)[3]

df2 <- tibble()
for (k_id in seq_along(1:k_idx)) {
  for (boot in seq_along(1:boot_max)){
    #print(tibble(row_id = fit_array[,k,boot], k, boot))
    this_df <- tibble(cluster_assignment = fit_array[,k_id,boot], k_id, boot)
    if (k_id==1 & boot==1){
      df2 <- this_df
    } else {
      df2 <- rbind(df2,this_df)
    }
  }
}

df <- rbind(df1,df2)

# df1, df2, df

df %>% 
  group_by(boot, k_id, cluster_assignment) %>% 
  tally() %>% 
  arrange(boot, k_id, desc(n)) %>% 
  group_by(boot, k_id) %>% 
  # re-assign k based on rank of n
  mutate( k_assign = row_number(),
          k = n()) %>% 
  filter(k_assign <= 2) %>% ungroup() %>% 
  select(k, k_assign, n, boot) %>% 
  spread(key=k_assign, value = n, sep="_") -> rank_12

########################
# Reps

reps = 100
seeds <- round(runif(reps,min=10000,max=99999),0)
k = 4
trials <- tibble(seed=seeds) %>% 
#  mutate(fit_km = map(seed,~stepFlexclust(data_df,seed=.x,k=2:8,nrep=1,FUN=cclust)))
  mutate(fit_km = map(seed,~stepFlexclust(as.matrix(data_df),seed=.x,k=2:8,nrep=1,
                                          family=kccaFamily("ejaccard"))))

trials %>% 
  mutate(model = map(fit_km,"models")) %>% 
  unnest(model) %>% 
  mutate(k = map_int(model,"k"),
         clusinfo = map(model,~`@`(.x,"clusinfo"))) %>% 
  dplyr::select(-fit_km,-model) %>% 
  unnest(clusinfo) %>% 
  arrange(seed,k, desc(size)) %>% 
  group_by(seed,k) %>% 
  mutate(k_assign = row_number()) %>% 
  filter(k_assign < 3) %>% 
  dplyr::select(-av_dist,-max_dist,-separation) %>% 
  spread(key=k_assign,sep="_",value = size) -> rank_12 

#######################  Prep data for plot 

rank_12 %>% 
  group_by(k) %>% 
  nest() %>% 
  mutate( s2d = map(data,~MASS::kde2d(.x$k_assign_1, .x$k_assign_2, n=100)),
          peak_height = map_dbl(s2d,~max(.x$z)),
          peak_at = map_dbl(s2d,~which(.x$z == peak_height)),
          size1_peak_at = map_dbl(s2d,~round(.x$x[peak_at %% 100], 1)),
          size2_peak_at = map_dbl(s2d,~round(.x$y[peak_at %/% 100], 1))
  ) %>% dplyr::select(-peak_at) %>%  
  arrange(desc(peak_height)) -> xyz

rank_12 %>% 
  inner_join(xyz, by=c("k")) %>% 
  mutate(
    dist = sqrt((k_assign_1 - size1_peak_at)^2 + (k_assign_2 - size2_peak_at)^2)
  ) %>% 
  arrange(dist) -> rank_12_dist

#####################################################################

# Plot Use density
rank_12 %>% filter(k>2) %>% 
  ggplot(aes(x=k_assign_1, y=k_assign_2)) + 
  geom_density2d_filled(n=100, h=NULL, adjust=1) +
  #geom_point(alpha=0.3, col="grey") + 
  geom_point(data=xyz, aes(x=size1_peak_at, y=size2_peak_at), color="red") +
  facet_wrap(~k,scales="free")

# Plot Use density
rank_12_dist %>% filter(k>2) %>% 
  ggplot(aes(x=k_assign_1, y=k_assign_2)) + 
  geom_density2d_filled(n=100, h=NULL, adjust=1) +
  #geom_point(alpha=0.3, col="grey") + 
  geom_point(data=xyz, aes(x=size1_peak_at, y=size2_peak_at), color="red") +
  facet_wrap(~k,scales="fixed") +
  geom_text(aes(x=size1_peak_at,
                y=size2_peak_at+50,
                label=round(peak_height*1e8,0)),col="white")
  


# Custom plot
xyz %>% ungroup() %>% 
  filter(k>2) %>% 
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
  facet_wrap(~k,scales="fixed") 

