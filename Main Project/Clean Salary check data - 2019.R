# install.packages("tidyverse")
# install.packages("Hmisc")
# install.packages("ggthemes")
# install.packages("GGally")
library(tidyverse)
library(Hmisc)
library(ggthemes)
library(stringr)
library(GGally)
library(stringr)
WorldData<-read_csv("sur2019.zip")
glimpse(WorldData)
#Select only what we need - rework with dplyr select(contains(".")) to compare time!
# DontWant1<-str_detect(names(WorldData),"Open")+
#   str_detect(names(WorldData),"JobContactPrior")+
#   str_detect(names(WorldData),"Mgr")+
#   str_detect(names(WorldData),"Work")+
#   str_detect(names(WorldData),"Blockchain")+
#   str_detect(names(WorldData),"SOV")+
#   str_detect(names(WorldData),"SO")+
#   str_detect(names(WorldData),"Surve")+
#   str_detect(names(WorldData),"NextYear")+
#   Job
# WorldData<-WorldData[,!DontWant1]
#dim(WorldData)
#names(WorldData)
#Convert To monthly or ignore
WorldData$Salary <- WorldData$CompFreq
WorldData<-  WorldData %>%
  mutate(Salary = if_else(CompFreq == "Yearly" , CompTotal/12,
                     if_else(CompFreq == "Monthly",CompTotal, NA_real_)))
# WorldData[c("Salary","CompFreq","CompTotal")]
  
#change to rename  
WorldData$YearsCoding <- WorldData$YearsCode
WorldData$YearsCodingProf <- WorldData$YearsCodePro
WorldData$FormalEducation <- WorldData$EdLevel
WorldData$CompanySize <- WorldData$OrgSize  

Bulgaria<-WorldData %>%
  filter(Country=="Bulgaria",!is.na(Salary)) 
names(Bulgaria)

Bulgaria<-Bulgaria%>%
  select(
    Salary,
    DevType,
    YearsCodingProf,
    YearsCoding,
    DevEnviron,
    FormalEducation,
    Age,
    CompanySize,

    Gender
  )
#Select only what we need
glimpse(Bulgaria)
#Create Dev Categories
Bulgaria$DevEnviron[is.na(Bulgaria$DevEnviron)]<-"Unknown"
#Create category Other
Bulgaria$DevEnviron[!(str_detect(Bulgaria$DevEnviron,"Visual")+
                 str_detect(Bulgaria$DevEnviron,"Atom")+
                 str_detect(Bulgaria$DevEnviron,"Py")+
                 str_detect(Bulgaria$DevEnviron,"Android")+
                 str_detect(Bulgaria$DevEnviron,"Eclipse")+
                 str_detect(Bulgaria$DevEnviron,"NetBeans")+
                 str_detect(Bulgaria$DevEnviron,"IntelliJ")+
                 str_detect(Bulgaria$DevEnviron,"PHP")+
                 str_detect(Bulgaria$DevEnviron,"Xcode"))]<-"Other" 


Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"Visual")]<-".Net" 
Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"Atom")]<-"Phython"
Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"Py")]<-"Phython"
Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"Android")] <-"Android"
Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"Eclipse")] <-"Java"
Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"NetBeans")] <-"Java"
Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"IntelliJ")] <-"Java"
Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"PHP")]  <-"PHP"
Bulgaria$DevEnviron[str_detect(Bulgaria$DevEnviron,"Xcode")]  <-"Apple"

#first remove "wildly innacurate" values, then remove extreme outliers ~(1 in 15787)
Bulgaria<-Bulgaria %>% 
  filter (Salary <20000, Salary > 301) %>% 
  #Alternativies for outliers 
  #4*sd(Bulgaria$Salary)+mean(Bulgaria$Salary)
  filter (Salary <3*IQR(Bulgaria$Salary)+mean(Bulgaria$Salary)) %>% 
  arrange(desc(Salary))


Bulgaria<-Bulgaria%>%
  #thisone was done above. This approach is better, but we work with too small dataset
  #TO DO - NEED BETTER SPLIT OF DEV TYPE, BOTH ARE BAD
  #separate( col = DevType, into = c("DevTy1","DevTy2","DevTy3"), sep = ";")%>%
  # mutate(DevType = str_split(DevType,";"))%>%
  # unnest(DevType)%>%
  #mainly looking for GIT vs the Rest so 1 column will do
  #this is same as with str_split ( n = 1, simplify = T)

  separate( col = YearsCoding, into = c("YearsCoding"), sep = "-") %>%
  separate( col = YearsCoding, into = c("YearsCoding"), sep = "-") %>%
  separate( col = YearsCodingProf, into = c("YearsCodingProf"), sep = "-")
  

#Cast Few categories to Integer
Bulgaria$YearsCoding[Bulgaria$YearsCoding ==   "30 or more years"]<-"30"
Bulgaria$YearsCoding<-as.integer(Bulgaria$YearsCoding)
Bulgaria$YearsCodingProf<-as.integer(Bulgaria$YearsCodingProf)


#Turn all non-numeric values into factors
getcharCols <- map_chr(Bulgaria,is.character)
Bulgaria[,getcharCols==T]<-map(Bulgaria[,getcharCols==T], factor) 

glimpse(Bulgaria)
mean(Bulgaria$Salary)
mean(Bulgaria$YearsCoding)
mean(Bulgaria$Age[!is.na(Bulgaria$Age)])
