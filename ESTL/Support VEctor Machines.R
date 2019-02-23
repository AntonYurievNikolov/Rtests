# install.packages("e1071")
library(e1071)
set.seed (1)
x=matrix (rnorm (20*2) , ncol =2)
y=c(rep (-1,10) , rep (1 ,10) )
x[y==1 ,]= x[y==1,] + 1
plot(x, col =(3-y))

dat=data.frame(x=x, y=as.factor (y))
 svmfit =svm(y~., data=dat , kernel ="linear", cost =10,
              scale =FALSE )
 #tuning
 set.seed (1)
tune.out=tune(svm ,y~.,data=dat ,kernel ="linear",
                 ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
bestmod =tune.out$best.model
summary (bestmod )

xtest=matrix (rnorm (20*2) , ncol =2)
ytest=sample (c(-1,1) , 20, rep=TRUE)
xtest[ytest ==1 ,]= xtest[ytest ==1,] + 1
testdat =data.frame (x=xtest , y=as.factor (ytest))

ypred=predict (bestmod ,testdat )
table(predict =ypred , truth= testdat$y )

###SVM#####
#radial and poly instead of linear and d and Y for param