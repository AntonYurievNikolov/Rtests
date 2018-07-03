#install.packages("dummies")
#install.packages("dendextend")
#install.packages("cluster")
library(dummies)
library(dendextend)
library(cluster)
#scaling -standartization t value
#scale(three_trees)
#Jicard similiarities for binary + dumification
#distance type
# omplete distance between group 1-2 and 3
#max(c(distance_1_3, distance_2_3))
# single distance between group 1-2 and 3
#min(c(distance_1_3, distance_2_3))
#  average distance between group 1-2 and 3
#mean(c(distance_1_3, distance_2_3))

####Hierarchical Clustering####
#Scale

Bulgariascaled<-scale(
  Bulgaria%>%
    # filter(!is.na(YearsCodingProf)&!is.na(YearsCoding))%>%
    mutate( YearsCodingProf = ifelse(is.na(YearsCodingProf),YearsCoding,YearsCodingProf))%>%
    select(Salary,YearsCodingProf,YearsCoding)
)
# Calculate the Distance

BulgariaDistance<-dist(Bulgariascaled,method = "euclidean")
hc_Bulgaria <- hclust(BulgariaDistance, method = "complete") #average #single
clusters_k5 <- cutree(hc_Bulgaria, k =5)#h = 1 cut at height

plot(hc_Bulgaria, main = 'Complete Linkage')
# Create a new dataframe storing these results
BsalaryClusters <-Bulgaria
BsalaryClusters$cluster <-clusters_k5

ggplot(BsalaryClusters, aes(x = Salary, y = YearsCodingProf, color = factor(cluster), size = cluster)) +
  geom_point()
#cDendograms####
dend_B <- as.dendrogram(hc_Bulgaria)
plot(dend_B)
dend_20 <- color_branches(dend_B, h = 4)
plot(dend_20)
####K means####
#Elbow plot 
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = Bulgariascaled, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)
#Silhouette####
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = Bulgariascaled, k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)
#pam####
BsalaryClusters$cluster <- pam(Bulgariascaled,5)$cluster
ggplot(BsalaryClusters, aes(x = Salary, y = YearsCodingProf, color = factor(cluster), size = cluster)) +
  geom_point()

#actual algo K - means####
BsalaryClusters$cluster <- kmeans(Bulgariascaled,4)$cluster
ggplot(BsalaryClusters, aes(x = Salary, y = YearsCodingProf, color = factor(cluster), size = cluster)) +
  geom_point()

