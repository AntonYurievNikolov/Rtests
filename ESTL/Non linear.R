library (ISLR)
attach (Wage)
#Polynomial Regression and Step Functions#####
fit<-lm(wage~poly(age ,4) ,data=Wage)
coef(summary (fit))
#predict
agelims =range(age)
age.grid=seq (from=agelims [1], to=agelims [2])
preds=predict (fit ,newdata =list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2*preds$se.fit)
par(mfrow =c(1,2) ,mar=c(4.5 ,4.5 ,1 ,1) ,oma=c(0,0,4,0))
plot(age ,wage ,xlim=agelims ,cex =.5, col ="darkgrey ")
title (" Degree -4 Polynomial ",outer =T)
lines(age.grid ,preds$fit ,lwd =2, col =" blue")
matlines (age.grid ,se.bands ,lwd =1, col =" blue",lty =3)

#ANOVA - M variance 1 is sufficient to explain the data against thealternative hypothesis that a more complex modelM2
fit.1= lm(wage~age ,data=Wage)
fit.2= lm(wage~poly(age ,2) ,data=Wage)
fit.3= lm(wage~poly(age ,3) ,data=Wage)
fit.4= lm(wage~poly(age ,4) ,data=Wage)
fit.5= lm(wage~poly(age ,5) ,data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
#Check vs this - the T values. F test above is with same results
coef(summary (fit.5))
#Logicstic
fit<-glm(I(wage >250)~poly(age ,4) ,data=Wage ,family =binomial )
preds<-predict (fit ,newdata =list(age=age.grid),se=T)

pfit<-exp(preds$fit )/(1+ exp( preds$fit ))
se.bands.logit <- cbind(preds$fit +2* preds$se.fit , preds$fit -2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+ exp(se.bands.logit))
preds<-predict (fit ,newdata =list(age=age.grid),type=" response ",se=T)

plot(age ,I(wage >250) ,xlim=agelims ,type ="n",ylim=c(0 ,.2) )
points (jitter (age), I((wage >250) /5) ,cex =.5, pch ="|",
          col =" darkgrey ")
lines(age.grid ,pfit ,lwd =2, col =" blue")
matlines (age.grid ,se.bands ,lwd =1, col =" blue",lty =3)
#Step function
fit<-lm(wage~cut (age ,4) ,data=Wage)
coef(summary (fit))


#Splines#######
library (splines )
#can also use BF and NS()
fit=lm(wage~bs(age ,knots =c(25 ,40 ,60) ),data=Wage)
pred<-predict (fit ,newdata =list(age =age.grid),se=T)
plot(age ,wage ,col =" gray ")
lines(age.grid ,pred$fit ,lwd =2)
lines(age.grid ,pred$fit +2* pred$se ,lty ="dashed")
lines(age.grid ,pred$fit -2* pred$se ,lty ="dashed")

#Smooth splines
fit=smooth.spline (age ,wage ,df =16)
fit2=smooth.spline (age ,wage ,cv=TRUE)

#LocalRegression
fit2=loess(wage~age ,span =.5, data=Wage)

#GAMs####
#S() spline, Lo 
library(gam)
# install.packages("gam")
gam.lr=gam(I(wage >250)~year+s(age ,df =5)+education ,
             family =binomial ,data=Wage)
par(mfrow =c(1,3))
plot(gam.lr,se=T,col =" green ")
table(education ,I(wage >250) )