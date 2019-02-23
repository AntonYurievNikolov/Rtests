library (tree)
# install.packages("tree")
# install.packages("randomForest")
attach (Carseats )
High<-ifelse (Sales <=8," No"," Yes ")
Carseats<-data.frame(Carseats ,High)
tree.carseats<-tree(High~.-Sales ,Carseats )
summary (tree.carseats )

# prune.boston =prunetree(tree.carseats,best =5)

#Random forest and bagging#####
library (MASS)
library (randomForest)
set.seed (1)
train <- sample (1: nrow(Boston ), nrow(Boston )/2)
boston.test<-Boston [-train ,"medv"]

bag.boston <-randomForest(medv~.,data=Boston ,subset =train ,
                           mtry=13, importance =TRUE)
bag.boston

set.seed (1)
rf.boston<-randomForest(medv~.,data=Boston ,subset =train ,
                          mtry=6, importance =TRUE)
yhat.rf<-predict (rf.boston ,newdata =Boston [-train ,])
mean(( yhat.rf -boston.test)^2)

#Bossting#### FIx later
library (gbm)
# install.packages("gbm")
set.seed (1)
boost.boston =gbm(medv~.,data=Boston [train ,], distribution=
                      "gaussian",n.trees =5000 , interaction.depth =4)