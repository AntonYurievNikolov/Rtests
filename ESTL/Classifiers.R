library (MASS)
library (ISLR)
library (tidyverse)
library(broom)
pairs(Smarket )
cor(Smarket [,-9])

attach (Smarket )
plot(Volume )

#Logistic#####
glm.fits<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,
             data=Smarket ,family =binomial )
summary (glm.fits)
glm.probs<-Smarket
glm.probs$PredictPerc<-predict(glm.fits,type ="response")
glm.probs$Predict<-ifelse(glm.probs$PredictPerc>0.5,"Up","Down")
table(glm.probs$Predict,glm.probs$Direction)
mean(glm.probs$Predict==glm.probs$Direction )

#ROC#####
ROC <- roc(ifelse(glm.probs$Predict=="Up",1,0),ifelse(glm.probs$Direction=="Up",1,0))
plot(ROC, col = "blue")
auc(ROC)

# ggplot(Smarket, aes(y=Direction,x=Volume)) + 
#   geom_line(aes(x=Volume,y=Direction, col ="red"))+
#   geom_jitter(width = 0 , height = 0.05, alpha = .5)+
#   geom_smooth(method = "glm",method.args=list(family = "binomial"), se = F)

#Linear Discriminant Analysis - LDA####
train <-(Year <2005)
Smarket.2005<- Smarket [!train ,]
Direction.2005= Direction [!train]

lda.fit=lda(Direction~Lag1+Lag2 ,data=Smarket ,subset =train)
lda.fit
plot(lda.fit)
lda.pred<-predict (lda.fit , Smarket.2005)

lda.class<-lda.pred$class
table(lda.class ,Direction.2005)

mean(lda.class == Direction.2005)
#Dwn
sum(lda.pred$posterior [,1]>.7)


#Quadratic Discriminant Analysis####
qda.fit<-qda(Direction~Lag1+Lag2 ,data=Smarket ,subset =train)
qda.fit

qda.class<-predict(qda.fit,Smarket.2005)$class


#KNN####
train.X<-cbind(Lag1 ,Lag2)[train ,]
test.X<-cbind (Lag1 ,Lag2)[!train ,]
train.Direction <-Direction [train]
knn.pred<-knn (train.X,test.X,train.Direction ,k=1)
table(knn.pred ,Direction.2005)
