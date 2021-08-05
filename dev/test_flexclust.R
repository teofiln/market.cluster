library(flexclust)
library(tidyverse)



d1 <- priceFeature(500, which = "circle")
km_d1 <- stepFlexclust(d1, k=2:12, nrep=10, save.data = TRUE)
image(getModel(km_d1,"3"), fastcol=FALSE)

d2 <- as.matrix(priceFeature(500, which = "ellipse"))
km_d2 <- stepFlexclust(d2, k=2:12, nrep=10, save.data = TRUE)
image(getModel(km_d2,"3"), fastcol=FALSE)

d3 <- as.matrix(priceFeature(500, which = "3clust"))
km_d3 <- stepFlexclust(d3, k=2:12, nrep=10, save.data = TRUE)
image(getModel(km_d3,"3"), fastcol=FALSE)



df <- tibble(performance=d1[,1],price=d1[,2])
df
km <- stepFlexclust(df, k=2:12, nrep=10, save.data = TRUE)
km_k10 <- getModel(km,"10")
image(km_k10)

cluster <- as.character(clusters(km_k6))

qplot(data=df,x=performance,y=price,col=cluster)
clusterhulls(df, clusters(km_k6))

library(ggvoronoi)

outline.df <- data.frame(x = c(0, 10, 10, 0),
                         y = c(0, 0, 10, 10))
centers_df <- as.data.frame(km_k6@centers) %>% mutate(c=row_number())
ggplot() + 
  geom_point(data=df,aes(x=performance,y=price),col=cluster) +
  geom_voronoi(data=centers_df,
               aes(x=performance,y=price,fill=c),col="grey",alpha=0.3,outline=outline.df)
image(km_k6)
library(MSA)

library(tripack)

data(tritest)
tritest.vm<-voronoi.mosaic(tritest$x,tritest$y)
tritest.vm

plot(voronoi.polygons(tritest.vm))

plot(df,pch=19,xlab="",ylab="",col=cluster)
clusterhulls(df,clusters(km_k3))
points(km_k3@centers[,1],km_k3@centers[,2],pch=3,cex=1.5,lwd=2)
V <- voronoi.mosaic(km_d3_k3@centers[,1],km_d3_k3@centers[,2])
P <- voronoi.polygons(V)
V

plot(P,add=TRUE)
points(V,pch=19)
plot(V,add=TRUE)
