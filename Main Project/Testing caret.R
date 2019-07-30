
# install.packages("caret")
library(tidyr)
library(purrr)
library(broom)
# library(spatstat)
library(caret)
# Logistic Regression####
# better apply to logistics with dummy
# Examine the dataset to identify potential independent variables
#not good for this sample , but checking whether we should pay someone above, below treshold

BulgariaLogistic<-
  Bulgaria%>%
  filter(!is.na(FormalEducation),!is.na(YearsCodingProf),!is.na(YearsCoding))%>%
  mutate( 
    SalaryScaled = ifelse(Salary > 4000,1,0)
  )%>%
  group_by(YearsCodingProf)%>%
  mutate( 
    ChanceForSal = mean(SalaryScaled)
  )%>%
  select(SalaryScaled,YearsCodingProf,YearsCoding,FormalEducation,ChanceForSal)

#Buidling the model
# model <- glm(SalaryScaled ~ YearsCodingProf, 
#              data = BulgariaLogistic, family = binomial)
# summary(model)

# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

model <- train(SalaryScaled ~YearsCodingProf, data = BulgariaLogistic, method = "glm",
               trControl = train.control)

print(model)

# 
# ggplot(BulgariaLogistic, aes(y=SalaryScaled,x=YearsCodingProf)) + 
#   geom_line(aes(x=YearsCodingProf,y=ChanceForSal, col ="red"))+
#   geom_jitter(width = 0 , height = 0.05, alpha = .5)+
#   geom_smooth(method = "glm",method.args=list(family = "binomial"), se = F)

#odds scale
#mutate(odds_hat = .fitted / (1 - .fitted))

#log scale
# mutate(log_odds_hat = log(.fitted / (1 - .fitted)))


#predict(model,testing, type = "response")
#ORIGINAL
# BulgariaLogistic$PredictedSalaryProb<-predict(model, type = "response")
#Caret

BulgariaLogistic$PredictedSalaryProb<-predict(model)
BulgariaLogistic$PredictedSalar<-ifelse( BulgariaLogistic$PredictedSalaryProb > 0.6, 1,0)

mean(BulgariaLogistic$PredictedSalar == BulgariaLogistic$SalaryScaled) 



#EDA
# pairs(BulgariaLogistic)
