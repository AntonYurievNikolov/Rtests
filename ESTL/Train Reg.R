library (MASS)
library (ISLR)
library (tidyverse)
library(broom)
# install.packages("ISLR")
names(Boston )
lm.fit <-lm(medv~lstat ,data=Boston )
summary (lm.fit)
confint (lm.fit)

predict (lm.fit ,data.frame(lstat=c(5 ,10 ,15) ),
         interval ="prediction")

predict (lm.fit ,data.frame(lstat=c(5 ,10 ,15) ),
         interval ="confidence")

predict (lm.fit ,data.frame(lstat=c(5 ,10 ,15) ),
         interval ="none")

ggplot(Boston, aes(y = medv, x = lstat)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
#Augment to see the residuals
aug<-as.tbl( augment(lm.fit))
plot(aug$.fitted,aug$.resid)
plot(predict (lm.fit), residuals (lm.fit))
plot(aug$.fitted,aug$.std.resid)
plot(predict (lm.fit), rstudent (lm.fit))

#######Multivalue####
lm.fit <-lm(medv~.,data=Boston )
summary (lm.fit)
#RSE
summary(lm.fit)$sigma
#RSQR
summary(lm.fit)$r.sq

##Poly
lm.fit <-lm(medv~lstat, data = Boston)


lm.fit2<-lm(medv~lstat + I(lstat ^2),data = Boston)
anova(lm.fit ,lm.fit2)
lm.fit2<-lm(medv~poly(lstat ,2),data = Boston)
lm.fit5<-lm(medv~poly(lstat ,5),data = Boston)

anova(lm.fit ,lm.fit2)
#interaction terms
it<-lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats )
summary(it)
