#unsupervised addition to
#k-means####


# Set seed
set.seed(1)
# Set up 2 x 3 plotting grid
# par(mfrow = c(2, 3))
for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(Bulgariascaled, 5, nstart=1)
  
  # Plot clusters
  plot(Bulgariascaled, col = km.out$cluster, 
       main = km.out$tot.withinss, 
       xlab = "", ylab = "")
}
# par(mfrow = c(1, 1))
#PCA - Dimensiality reductions####
# 
# #normalization first
# colMeans(wisc.data)
# apply(wisc.data, 2, sd)

pr.out<-prcomp(Bulgariascaled,scale = T)
summary(pr.out)
biplot(pr.out)

pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)


plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")
#Plotting PCAs
plot(pr.out$x[, c(1, 2)], 
     xlab = "PC1", ylab = "PC2")

plot(pr.out$x[, c(1, 3)], 
     xlab = "PC1", ylab = "PC3")

#Explaining variance
# Create a hierarchical clustering model: wisc.pr.hclust
pr.hclust <- hclust(dist(pr.out$x[, 1:2]), method = "complete")
# Cut model into 4 clusters: wisc.pr.hclust.clusters
pr.hclust.clust  <- cutree(pr.hclust, k = 4)

target<-Bulgariascaled[,1]

table(target,pr.hclust.clust)
#check the rest
# table(target, wisc.hclust.clusters)
# table(target, wisc.km$cluster)