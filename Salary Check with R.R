#install.packages("tidyverse")
library(tidyverse)

path <- file.path("C:\\Users\\User\\Documents\\R", "sur.csv")
WorldData<-read_csv(path)
glimpse(WorldData)
#Select only what we need - rework with dplyr select(contains(".")) to compare time!
DontWant1<-str_detect(names(WorldData),"Assess")+
 str_detect(names(WorldData),"JobContactPrior")+
 str_detect(names(WorldData),"HypotheticalT")+
 str_detect(names(WorldData),"Ads")+
 str_detect(names(WorldData),"AdB")+
 str_detect(names(WorldData),"greeDisag")+
 str_detect(names(WorldData),"JobEmai")+
 str_detect(names(WorldData),"AI")+
 str_detect(names(WorldData),"StackOverflow")+
 str_detect(names(WorldData),"Hypothetical")+
 str_detect(names(WorldData),"Ethics")+
  #Check these later
 str_detect(names(WorldData),"NextYear")+ 
 str_detect(names(WorldData),"Survey")
#keep<-c(2:17,50:62,65:79,115:129)
WorldData<-WorldData[,!DontWant1]
#dim(WorldData)
#names(WorldData)
#Dplyr 
#cleanse data a little bit

Bulgaria<-WorldData %>%
  filter(Country=="Bulgaria",!is.na(Salary)) 
#Select only what we need

#first remove "wildly innacurate" values, then remove extreme outliers ~(1 in 15787)
Bulgaria<-Bulgaria %>% 
  filter (Salary <20000, Salary > 301) %>% 
  #Alternativies for outliers 
  #4*sd(Bulgaria$Salary)+mean(Bulgaria$Salary)
  filter (Salary <3*IQR(Bulgaria$Salary)+mean(Bulgaria$Salary)) %>% 
arrange(desc(Salary))
#cleaner
#c("DT1","DT2","DT3","DT4","DT5","DT6","DT7","DT8")
Bulgaria<-Bulgaria%>%separate( col = DevType, into = c("DevTy1","DevTy2","DevTy3"), sep = ";")%>%
#mainly looking for GIT vs the Rest so 1 column will do
  separate( col = VersionControl, into = c("VersionControl"), sep = ";")
#Check versus known mean (~3k)
#each opperation above was compared against a target mean, because we work with extremely small dataset
Bulgaria$FormalEducation
Bulgaria%>%group_by(FormalEducation)%>% 
  summarise(MeanByVC=mean(Salary))

##GGPLOT2 tests
#Point 1 - Why you should use GIT 
Bulgaria%>%group_by(VersionControl)%>% 
  summarise(MeanByVC=mean(Salary))

ggplot(Bulgaria,aes(x=Salary,y=VersionControl)) + geom_point() + ggplot2::scale_x_log10()
#The POWER OF MONITORS!!!
Bulgaria%>%group_by(NumberMonitors)%>% 
  summarise(MeanPerMonitor=mean(Salary))

ggplot(Bulgaria,aes(x=Salary,y=NumberMonitors,color = NumberMonitors,size =Salary))+geom_point()+facet_wrap(~Hobby)
#Random Checks
BestPaidJobs<-Bulgaria[Bulgaria$Salary>2*sd(Bulgaria$Salary)+mean(Bulgaria$Salary)  ,]
BestPaidJobs$Currency
BestPaidJobs$CurrencySymbol
BestPaidJobs$Salary

MoneyBags <- Bulgaria[which.max(Bulgaria$Salary), ]
MoneyBags$DevTy1
MoneyBags$DevTy2
MoneyBags$DevTy3
MoneyBags$Salary
MoneyBags$Age
#clean the env
#rm(list=ls())
####Ducking Begins


Bulgaria%>%group_by(SecondaryDevType)%>% 
  summarise(S=mean(Salary))


