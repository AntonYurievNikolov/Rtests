#PCA again :) ####
pr.out<-prcomp (USArrests , scale =TRUE)
pr.out$rotation
biplot (pr.out , scale =0)

pr.out$rotation<--pr.out$rotation
pr.out$x<--pr.out$x
biplot (pr.out , scale =0)

#% explained
pr.var<-pr.out$sdev ^2
pve<-pr.var/sum(pr.var )

plot(pve , xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1) ,type= "b")

plot(cumsum(pve ), xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1) ,type= "b")
#K means is done in several other places#
#H Clust####
set.seed (2)
x<-matrix (rnorm (50*2) , ncol =2)
x[1:25 ,1]<-x[1:25 ,1]+3
x[1:25 ,2]<-x[1:25 ,2] -4

hc.complete <-hclust (dist(x), method ="complete")
hc.average <-hclust (dist(x), method ="average")
hc.single <-hclust (dist(x), method ="single")

par(mfrow =c(1,3))
plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="", cex =.9)
plot(hc.average , main =" Average Linkage ", xlab="", sub ="",cex =.9)
plot(hc.single , main =" Sinle Linkage ", xlab="", sub ="",cex =.9)
cutree (hc.complete , 2)
       