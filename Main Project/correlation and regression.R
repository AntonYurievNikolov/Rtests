##Linear Regressions####
#fitting multiple models
# install.packages('pROC')
library(tidyr)
library(purrr)
library(broom)
library(pROC)
# Perform a linear regression on each item in the data column
coefficent<-Bulgaria %>%
  select(Salary,YearsCodingProf,IDE) %>%
  nest(-IDE) %>%
  mutate(model = map(data, ~ lm(Salary ~ YearsCodingProf, data = .)))%>%
  mutate(tidied = map(model , tidy)) %>%
  unnest(tidied)

coefficent%>%mutate(p.adjusted  = p.adjust(p.value))%>%filter(p.adjusted <0.05)

#check this for the whole world
#WOW
# coefficentWorld<-WorldData %>%
#   select(Salary,YearsCodingProf,Country) %>%
#   filter(!is.na(Country),!is.na(YearsCodingProf),!is.na(Salary))%>%
#   group_by(Country)%>%
#   mutate(n = n())%>%
#   filter(n>100)%>%
#   ungroup()%>%
#   nest(-Country) %>%
#   #filter(Country=="United Kingdom") %>%
#   mutate(
#          model = map(data, ~ lm(Salary ~ YearsCodingProf, data = .)))%>% 
#          mutate(tidied = map(model , tidy))  %>% 
#   unnest(tidied)
# 
# coefficentWorld%>%mutate(p.adjusted  = p.adjust(p.value))%>%filter(p.adjusted <0.05)
#cut example
ggplot(data = Bulgaria, 
       aes(x = cut(Bulgaria$YearsCodingProf , breaks = 3), y = Bulgaria$Salary)) + 
  geom_boxplot()

# Compute correlation for all non-missing pairs
Bulgaria %>%
  summarise(N = n(), r = cor(YearsCodingProf, Salary, use = "pairwise.complete.obs"))

cor<-cor( Bulgaria$Salary,Bulgaria$YearsCodingProf, use = "pairwise.complete.obs")
mod<-lm(Salary~YearsCodingProf,data = Bulgaria)
#predicting
predict(mod,newdata = Bulgaria[Bulgaria$YearsCodingProf == 12,] %>% select(Salary,YearsCodingProf))
Salary<-c(4800)
YearsCodingProf<-c(11)
test<-data.frame(Salary,YearsCodingProf)
predict(mod,newdata =test)
##fitting the alogorythm
#summary(mod)
#fitted.values(mod)
aug<-as.tbl( augment(mod))

mean(fitted.values(mod)) - mean(Bulgaria$Salary) < 200
mean(residuals(mod))
#RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))

#R2 - how much is explained
1 - var(aug$.resid)/var(aug$Salary) 

## Rank points of high leverage --.cooksd for infuence
aug %>%
  arrange(desc(.cooksd )) %>%
  head()

#Parallel/Multi regression#### 
modParallel <- lm(data = Bulgaria, Salary~YearsCodingProf + NumberMonitors)
summary(modParallel)
# Augment the model
augmented_mod <- augment(modParallel)
glimpse(augmented_mod)
data_space <- ggplot(augmented_mod, aes(x = YearsCodingProf, y = Salary, color = NumberMonitors)) + 
  geom_point()
data_space + 
  geom_line(aes(y = .fitted))

#Adding Interaction between the Variables
modParallel <- lm(data = Bulgaria, Salary~YearsCodingProf + NumberMonitors+YearsCodingProf:NumberMonitors)

# interaction plot
ggplot(Bulgaria, aes(y = Salary, x = YearsCodingProf, color = NumberMonitors)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#Multiple Regression#
modMulti <- lm(data = Bulgaria, Salary~YearsCodingProf +  DatabaseWorkedWith )
summary(modMulti)

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
model <- glm(SalaryScaled ~ YearsCodingProf, 
             data = BulgariaLogistic, family = binomial)
summary(model)
-2.28963 + BulgariaLogistic$YearsCodingProf* 0.34275
ggplot(BulgariaLogistic, aes(y=SalaryScaled,x=YearsCodingProf)) + 
  geom_line(aes(x=YearsCodingProf,y=ChanceForSal, col ="red"))+
  geom_jitter(width = 0 , height = 0.05, alpha = .5)+
  geom_smooth(method = "glm",method.args=list(family = "binomial"), se = F)

#odds scale
#mutate(odds_hat = .fitted / (1 - .fitted))

#log scale
# mutate(log_odds_hat = log(.fitted / (1 - .fitted)))

#testing<- data.frame(YearsCodingProf = c(1), 
#                    FormalEducation =  c(1) )
#predict(model,testing, type = "response")
BulgariaLogistic$PredictedSalaryProb<-predict(model, type = "response")
BulgariaLogistic$PredictedSalar<-ifelse( BulgariaLogistic$PredictedSalaryProb > 0.3, 1,0)

mean(BulgariaLogistic$PredictedSalar == BulgariaLogistic$SalaryScaled) 
ROC <- roc(BulgariaLogistic$SalaryScaled, BulgariaLogistic$PredictedSalaryProb)
plot(ROC, col = "blue")
# Calculate the area under the curve (AUC)
auc(ROC)

#EDA
pairs(BulgariaLogistic)


#stepwise regression##
null_model <- glm(SalaryScaled ~ 1, data = BulgariaLogistic, family = "binomial")
full_model <- glm(SalaryScaled ~ ., data = BulgariaLogistic, family = "binomial")
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
step_prob <- predict(step_model, type = "response")
ROC <- roc(BulgariaLogistic$SalaryScaled, step_prob)
plot(ROC, col = "red")
auc(ROC)

