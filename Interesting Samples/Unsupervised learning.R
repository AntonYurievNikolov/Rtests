#unsupervised addition to
#k-means###
# Set up 2 x 3 plotting grid
par(mfrow = c(2, 3))

# Set seed
set.seed(1)

for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(Bulgariascaled, 5, nstart=1)
  
  # Plot clusters
  plot(Bulgariascaled, col = km.out$cluster, 
       main = km.out$tot.withinss, 
       xlab = "", ylab = "")
}