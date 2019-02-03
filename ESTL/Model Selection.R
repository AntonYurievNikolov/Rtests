#Subset Selection. best subset######
library (ISLR)
#fix(Hitters )
Hitters<-na.omit(Hitters )
# install.packages("leaps")
library (leaps)
#The asterix shows nth best variable 
regfit.full<-regsubsets (Salary~.,data=Hitters ,nvmax =19)
reg.summary<-summary (regfit.full)
reg.summary$rsq
#Plot the results
par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l")

which.max (reg.summary$adjr2)
points (11, reg.summary$adjr2[11], col ="red",cex =2, pch =20)
#same with which.min (reg.summary$cp )Cp, BIC,
coef(regfit.full,11)

#Forward and Backward Stepwise Selection####
#?step samples in the corr and reg samples
regfit.fwd<-regsubsets (Salary~.,data=Hitters ,nvmax =19, method ="forward")
summary (regfit.fwd )
regfit.bwd=regsubsets (Salary~.,data=Hitters ,nvmax =19,method ="backward")
summary (regfit.bwd)

#Selection an CV####
set.seed (1)
train<-sample (c(TRUE ,FALSE), nrow(Hitters ),rep=TRUE)
test<-(!train )
regfit.best<-regsubsets(Salary~.,data=Hitters [train ,],nvmax =19)
test.mat<-model.matrix (Salary~.,data=Hitters [test ,])

val.errors<-rep(NA ,19)
for(i in 1:19){
 coefi<-coef(regfit.best ,id=i)
 pred<-test.mat [,names(coefi)]%*% coefi
 val.errors [i]<- mean(( Hitters$Salary[test]-pred)^2)
}

coef(regfit.best ,which.min (val.errors ))

predict.regsubsets =function (object ,newdata ,id ,...){
 form<-as.formula (object$call [[2]])
 mat<-model.matrix (form ,newdata )
 coefi <-coef(object ,id=id)
 xvars<-names (coefi )
 mat[,xvars ]%*% coefi
 }

regfit.best<-regsubsets (Salary~.,data=Hitters ,nvmax =19)
coef(regfit.best ,10)
#CV

k<-10
set.seed (1)
folds<-sample (1:k,nrow(Hitters ),replace =TRUE)
cv.errors<-matrix (NA ,k,19, dimnames =list(NULL , paste (1:19) ))

for(j in 1:k){
  best.fit <-regsubsets (Salary~.,data=Hitters [folds !=j,],
                        nvmax =19)
  for(i in 1:19) {
  pred<-predict (best.fit ,Hitters [folds ==j,], id=i)
  cv.errors [j,i]<-mean( (Hitters$Salary[folds ==j]-pred)^2)
  }
  }
apply(cv.errors ,2, mean)

#ridge and lasso####
x<-model.matrix(Salary~.,Hitters )[,-1]
y<-Hitters$Salary
# install.packages("glmnet")
library (glmnet )
grid<-10^ seq (10,-2, length =100)
ridge.mod <-glmnet (x,y,alpha =0, lambda =grid)#alpha = 0 ridge, 1 for lasso
dim(coef(ridge.mod ))
#λ = 11,498,ridge.mod$lambda [50]
sqrt(sum(coef(ridge.mod)[ -1 ,50]^2) )
#Predict the coeff when λ = 50
predict (ridge.mod ,s=50, type ="coefficients")[1:20 ,]


set.seed (1)
train<-sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test<-y[test]

ridge.mod <-glmnet (x[train ,],y[train],alpha =0, lambda =grid )
ridge.pred<-predict (ridge.mod ,s=4, newx=x[test ,])
mean(( ridge.pred -y.test)^2)

#Select best λ
set.seed (1)
cv.out<-cv.glmnet (x[train ,],y[train],alpha =0)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam

out<-glmnet (x,y,alpha =0)
predict (out ,type=" coefficients",s=bestlam )[1:20 ,]

#lasso use this to check the 0 coeff predict (out ,type =" coefficients",s=bestlam )[1:20 ,]

#PCA####
install.packages("pls")
library (pls)
library (ISLR)

set.seed (2)
x<-model.matrix(Salary~.,Hitters )[,-1]
y<-Hitters$Salary

train<-sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test<-y[test]


pcr.fit<-pcr(Salary~., data=Hitters ,scale=TRUE ,validation ="CV")
summary (pcr.fit )

validationplot(pcr.fit ,val.type="MSEP")

pcr.pred<-predict (pcr.fit ,x[test ,], ncomp =7)
mean((pcr.pred -y.test)^2)

#PLS####
pls.fit=plsr(Salary~., data=Hitters ,subset =train ,scale=TRUE ,
             validation ="CV")
summary (pls.fit )