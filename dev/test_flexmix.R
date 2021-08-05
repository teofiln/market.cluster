#test_flexmix

load("data/data_df.Rda")
library(flexmix)

skimr::skim(data_df)

# Convert to matrix
data_mat <- as.matrix(data_df)

# 1. Which is the suitable probability distribution?
#   (multi) Bernoulli distribution
# 2. How many subpopulations should we consider?
#   Let's try from 2 to 8 clusters and pick by BIC.
# 3. Which are the parameters and their estimations?
# Each p(probability) for each column. Also the proportions for each column.

# Search for K
bernoulli_mix_model <- stepFlexmix(data_mat~1,
                                   model=FLXMCmvbinary(),
                                   k=1:8,
                                   control = list(tolerance = 1e-15, iter.max = 5000))


res <- as.data.frame(show(bernoulli_mix_model))

plot(bernoulli_mix_model)

res %>% dplyr::select(k,AIC,BIC,ICL) %>% 
  gather(key="InfoCrit", value="val",-k) %>% 
  group_by(InfoCrit) %>% 
  mutate(which_min = which.min(val),
         size = if_else(which_min == k,5,3)) %>% 
  ggplot(aes(x=k,y=val,col=InfoCrit)) + geom_line() + geom_point(aes(size=size))

# Select best fit based on information criterion
best_fit <- getModel(bernoulli_mix_model,which="BIC")
# BIC and ICL = 7
# AIC = 8

best_fit <- getModel(bernoulli_mix_model,"4")

best_fit
plot(best_fit)

# proportions
prior(best_fit)

library(tidyverse)

parameters(best_fit) %>% as.data.frame() %>% rownames_to_column(var="factors") %>%  
  gather(key="cluster", value="probability",-factors) %>% 
  ggplot(aes(x=factors, y=probability)) + geom_col() + facet_wrap(~cluster)

