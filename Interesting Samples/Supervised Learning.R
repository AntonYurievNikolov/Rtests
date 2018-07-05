#install.packages("class") 
#install.packages("naivebayes") 
# install.packages("pROC") 
library(class)
library(naivebayes)
library(pROC)
#knn#####
#Scale

Bulgariascaled<-
  Bulgaria%>%
    # filter(!is.na(YearsCodingProf)&!is.na(YearsCoding))%>%
    mutate( YearsCodingProf = ifelse(is.na(YearsCodingProf),YearsCoding,YearsCodingProf))%>%
    mutate( 
            YearsCodingProf =nrmlizeR(YearsCodingProf), #scale(YearsCodingProf), 
            YearsCoding = nrmlizeR(YearsCoding),#scale(YearsCoding) ,
            SalaryScaled = cut(as.integer(Salary),breaks = 4,dig.lab = 7)
          )%>%
    select(Salary,SalaryScaled,YearsCodingProf,YearsCoding)

## split into test and train
smp_size <- floor(0.9 * nrow(Bulgariascaled))
set.seed(123)
train_ind <- sample(seq_len(nrow(Bulgariascaled)), size = smp_size)
train <- Bulgariascaled[train_ind, ]
test <- Bulgariascaled[-train_ind, ]

Salaries <- train$SalaryScaled
sal_pred <- knn(train = train[-c(1,2)], test = test[-c(1,2)], cl = Salaries, k = 25)
# Create a confusion matrix of the actual versus predicted values
sal_actual <- test$SalaryScaled
# table(sal_pred, sal_actual)
# Compute the accuracy
mean(sal_pred == sal_actual)
#test what is the best k
modelAcc <- map_dbl(1:30,  function(k){
  model <- knn(train = train[-c(1,2)], test = test[-c(1,2)], cl = Salaries, k = k)
  mean(model == sal_actual)
})
KmodelAcc<- data.frame(
  k = 1:30,
  modelAcc = modelAcc
)
ggplot(KmodelAcc, aes(x = k, y=modelAcc))+
  geom_line()

#naive Bayes#####

#Test case#
testing<-data.frame(
  YearsCodingProf = as.factor(c(6)),
  FormalEducation = as.factor(c("Bachelor’s degree (BA, BS, B.Eng., etc.)"))
  
)

BulgariaFactors<-
  Bulgaria%>%
  mutate( 
    YearsCodingProf =as.factor(YearsCodingProf), 
    YearsCoding = as.factor(YearsCoding),
    FormalEducation = factor(FormalEducation),
    SalaryScaled = cut(as.integer(Salary),breaks = 8,dig.lab = 7)
  )%>%
  select(Salary,SalaryScaled,YearsCodingProf,YearsCoding,FormalEducation)

locmodel <- naive_bayes(
                        SalaryScaled~YearsCodingProf+FormalEducation, 
                        data = BulgariaFactors,
                        laplace = 1
                        )
predict(locmodel,testing,
        type = "prob"
)


# Logistic Regression####
# better apply to logistics with dummy
# Examine the dataset to identify potential independent variables
#not good for this sample , but checking whether we should pay someone above, below treshold

BulgariaLogistic<-
  Bulgaria%>%
  filter(!is.na(FormalEducation),!is.na(YearsCodingProf),!is.na(YearsCoding))%>%
  mutate( 
    YearsCodingProf = factor(YearsCodingProf), 
    YearsCoding = factor(YearsCoding),
    FormalEducation =  factor(FormalEducation),
    SalaryScaled = ifelse(Salary > 4000,1,0)
  )%>%
  select(SalaryScaled,YearsCodingProf,YearsCoding,FormalEducation)



#stepwise regression##
null_model <- glm(SalaryScaled ~ 1, data = BulgariaLogistic, family = "binomial")
full_model <- glm(SalaryScaled ~ ., data = BulgariaLogistic, family = "binomial")
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
step_prob <- predict(step_model, type = "response")
ROC <- roc(BulgariaLogistic$SalaryScaled, step_prob)
plot(ROC, col = "red")
auc(ROC)

#Buidling the model
model <- glm(SalaryScaled ~ YearsCodingProf + FormalEducation, 
                      data = BulgariaLogistic, family = "binomial")
summary(model)
#testing<- data.frame(YearsCodingProf = c(1), 
#                    FormalEducation =  c(1) )
#predict(model,testing, type = "response")
BulgariaLogistic$PredictedSalaryProb<-predict(model, type = "response")
BulgariaLogistic$PredictedSalar<-ifelse( BulgariaLogistic$PredictedSalaryProb > 
                                          mean(BulgariaLogistic$SalaryScaled) , 1,0)

mean(BulgariaLogistic$PredictedSalar) == mean(BulgariaLogistic$SalaryScaled) 
ROC <- roc(BulgariaLogistic$SalaryScaled, BulgariaLogistic$PredictedSalaryProb)
plot(ROC, col = "blue")
# Calculate the area under the curve (AUC)
auc(ROC)


