#install.packages("tidyverse")
library(tidyverse)

path <- file.path("C:\\Users\\User\\Documents\\R", "sur.csv")
WorldData<-read_csv(path)
glimpse(WorldData)
#Select only what we need
DontWant1<-str_detect(names(WorldData),"Assess")+
 str_detect(names(WorldData),"JobContactPrior")+
 str_detect(names(WorldData),"HypotheticalT")+
 str_detect(names(WorldData),"Ads")+
 str_detect(names(WorldData),"AdB")+
 str_detect(names(WorldData),"greeDisag")+
 str_detect(names(WorldData),"JobEmai")+
 str_detect(names(WorldData),"AI")+
 str_detect(names(WorldData),"StackOverflow")+
 str_detect(names(WorldData),"Hypothetical")
#keep<-c(2:17,50:62,65:79,115:129)
WorldData<-WorldData[,!DontWant1]
#dim(WorldData)
#names(WorldData)
#Dplyr 
#cleanse data a little bit
Bulgaria<-WorldData %>%
  filter(Country=="Bulgaria",!is.na(Salary)) 
#Select only what we need

#first remove "wildly innacurate" values, then remove extreme outliers (1 in 15787)
Bulgaria<-Bulgaria %>% 
  filter (Salary <20000, Salary > 301) %>% 
  filter (Salary <4*sd(Bulgaria$Salary)+mean(Bulgaria$Salary) ) %>% 
arrange(desc(Salary))
#The POWER OF MONITORS!!!
Bulgaria%>%group_by(NumberMonitors)%>% 
  summarise(MeanPerMonitor=mean(Salary))

#Check versus known mean (~3k)

mean(Bulgaria$Salary)
##GGPLOT2 tests
#Point 1 - Why you should use GIT 
ggplot(Bulgaria,aes(x=Salary,y=VersionControl)) + geom_point() + ggplot2::scale_x_log10()
ggplot(Bulgaria,aes(x=Salary,y=NumberMonitors,color = NumberMonitors,size =Salary))+geom_point()+facet_wrap(~Hobby)
#Random Checks
BestPaidJobs<-Bulgaria[Bulgaria$Salary>2*sd(Bulgaria$Salary)+mean(Bulgaria$Salary)  ,]
BestPaidJobs$Currency
BestPaidJobs$CurrencySymbol
BestPaidJobs$Salary

MoneyBags <- Bulgaria[which.max(Bulgaria$Salary), ]
MoneyBags$DevType
MoneyBags$Salary
MoneyBags$Age
#clean the env
#rm(list=ls())
