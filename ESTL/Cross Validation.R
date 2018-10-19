library (ISLR)
set.seed (1)
train<-sample (392 ,196)
lm.fit <-lm(mpg~horsepower ,data=Auto ,subset =train )
#MSE####
attach (Auto)
mean((mpg - predict(lm.fit ,Auto))[-train ]^2)

lm.fit3<-lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)

#Leave-One-Out Cross-Validation LOUCV####

library (boot)#?cv.glm

for (i in 1:5)
  {
    glm.fit<-glm(mpg~poly(horsepower ,i),data=Auto)
    cv.error[i]=cv.glm (Auto ,glm.fit)$delta [1]
  }
cv.error

#k-Fold Cross-Validation#####

for (i in 1:10)
{
  glm.fit<-glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm (Auto ,glm.fit,K=10)$delta [1]
}
cv.error
#The Bootstrap####
boot.fn=function (data ,index ) return (coef(lm(mpg~horsepower ,data=data ,subset =index)))
boot.fn(Auto ,1:392)
        
set.seed(1)
        
boot.fn(Auto ,sample (392 ,392 , replace =T))

boot(Auto ,boot.fn ,1000)