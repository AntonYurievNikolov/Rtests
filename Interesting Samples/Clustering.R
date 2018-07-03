#install.packages("dummies")
#install.packages("dendextend")
library(dummies)
library(dendextend)
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
BsalaryClusters$cluster <- kmeans(Bulgariascaled,5)$cluster
ggplot(BsalaryClusters, aes(x = Salary, y = YearsCodingProf, color = factor(cluster), size = cluster)) +
  geom_point()

