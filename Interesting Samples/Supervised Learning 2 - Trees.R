# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
#Decision Trees ####
BulgariaTree<-
  Bulgaria%>%
  filter(!is.na(FormalEducation),!is.na(YearsCodingProf),!is.na(YearsCoding),!is.na(VersionControl),!is.na(IDE),!is.na(NumberMonitors),!is.na(DevTy1))%>%
  mutate( 
    YearsCodingProf = factor(YearsCodingProf), 
    YearsCoding = factor(YearsCoding),
    FormalEducation =  factor(FormalEducation),
    DevTy1 = factor(DevTy1),
    Salary = cut(as.integer(Salary),breaks = 6,dig.lab = 8)
  )#%>%
 # select(Salary,YearsCodingProf,YearsCoding,FormalEducation)

#Sample
smp_size <- floor(0.9 * nrow(BulgariaTree))
set.seed(123)
train_ind <- sample(seq_len(nrow(Bulgariascaled)), size = smp_size)
train <- BulgariaTree[train_ind, ]
test <- BulgariaTree[-train_ind, ]

#pre prunninf

tree_model <- rpart(Salary~FormalEducation+YearsCoding+YearsCodingProf+VersionControl+IDE+NumberMonitors
                    , data = train, method = "class", control = rpart.control(cp = 0,maxdepth = 5, minsplit=5 ))
#, rpart.control(cp = 0,maxdepth = 1, minsplit=10 )
test$predict <- predict(tree_model, test, type = "class")
plotcp(tree_model)
#table(test$predict,test$Salary)
mean(test$predict == test$Salary)

#post prunninf
t2 <- prune(tree_model, cp = inf )
test$predict <- predict(t2, test, type = "class")
mean(test$predict == test$Salary)

#plot
rpart.plot(t2)
rpart.plot(tree_model)


#Forests ####
# Load the randomForest package
library(randomForest)

# # Build a random forest model
#not enough data to test this
# library(randomForest)
# loan_model <- randomForest(outcome ~ ., data = loans_train)
# # Compute the accuracy of the random forest
# loans_test$pred <- predict(loan_model, loans_test)
# mean(loans_test$pred == loans_test$outcome)