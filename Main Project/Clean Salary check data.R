#install.packages("tidyverse")
#install.packages("Hmisc")
#install.packages("ggthemes")
#install.packages("GGally")
library(tidyverse)
library(Hmisc)
library(ggthemes)
library(stringr)
library(GGally)
library(stringr)
WorldData<-read_csv("sur.zip")
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
  str_detect(names(WorldData),"atisfaction")+
  str_detect(names(WorldData),"Job")+
  #single columns
  str_detect(names(WorldData),"SkipMeals")+
  str_detect(names(WorldData),"Dependents")+
  str_detect(names(WorldData),"MilitaryUS")+
  str_detect(names(WorldData),"Bootcamp")+
  str_detect(names(WorldData),"Survey")+
  str_detect(names(WorldData),"Hope") 

#keep<-c(2:17,50:62,65:79,115:129)
WorldData<-WorldData[,!DontWant1]
#dim(WorldData)
#names(WorldData)

Bulgaria<-WorldData %>%
  filter(Country=="Bulgaria",!is.na(Salary)) 
#Select only what we need

#Create Dev Categories
Bulgaria$IDE[is.na(Bulgaria$IDE)]<-"Unknown"
#Create category Other
Bulgaria$IDE[!(str_detect(Bulgaria$IDE,"Visual")+
                 str_detect(Bulgaria$IDE,"Atom")+
                 str_detect(Bulgaria$IDE,"Py")+
                 str_detect(Bulgaria$IDE,"Android")+
                 str_detect(Bulgaria$IDE,"Eclipse")+
                 str_detect(Bulgaria$IDE,"NetBeans")+
                 str_detect(Bulgaria$IDE,"IntelliJ")+
                 str_detect(Bulgaria$IDE,"PHP")+
                 str_detect(Bulgaria$IDE,"Xcode"))]<-"Other" 


Bulgaria$IDE[str_detect(Bulgaria$IDE,"Visual")]<-".Net" 
Bulgaria$IDE[str_detect(Bulgaria$IDE,"Atom")]<-"Phython"
Bulgaria$IDE[str_detect(Bulgaria$IDE,"Py")]<-"Phython"
Bulgaria$IDE[str_detect(Bulgaria$IDE,"Android")] <-"Android"
Bulgaria$IDE[str_detect(Bulgaria$IDE,"Eclipse")] <-"Java"
Bulgaria$IDE[str_detect(Bulgaria$IDE,"NetBeans")] <-"Java"
Bulgaria$IDE[str_detect(Bulgaria$IDE,"IntelliJ")] <-"Java"
Bulgaria$IDE[str_detect(Bulgaria$IDE,"PHP")]  <-"PHP"
Bulgaria$IDE[str_detect(Bulgaria$IDE,"Xcode")]  <-"Apple"

#first remove "wildly innacurate" values, then remove extreme outliers ~(1 in 15787)
Bulgaria<-Bulgaria %>% 
  filter (Salary <20000, Salary > 301) %>% 
  #Alternativies for outliers 
  #4*sd(Bulgaria$Salary)+mean(Bulgaria$Salary)
  filter (Salary <3*IQR(Bulgaria$Salary)+mean(Bulgaria$Salary)) %>% 
  arrange(desc(Salary))

#now we test whether a split is a good idea
# Test<-mutate(Bulgaria,DevType = str_split(DevType,";"))%>%
#   unnest(DevType)%>%
#   mutate(LanguageWorkedWith = str_split(LanguageWorkedWith,";"))%>%
#   unnest(LanguageWorkedWith)%>%
#   mutate(DatabaseWorkedWith = str_split(DatabaseWorkedWith,";"))%>%
#   unnest(DatabaseWorkedWith)%>%
#   mutate(PlatformWorkedWith = str_split(PlatformWorkedWith,";"))%>%
#   unnest(PlatformWorkedWith)
# Bulgaria%>%
#   summarise( 
#     m = mean(Salary),
#     IQR = IQR(Salary),
#     sd = sd(Salary)
#   )
# Test%>%
#   summarise( 
#     m = mean(Salary),
#     IQR = IQR(Salary),
#     sd = sd(Salary)
#   )
# hist(Bulgaria$Salary)
# hist(Test$Salary)
# Spliting this way is a BAD idea - the IQR,mean and sd are too scewed


Bulgaria<-Bulgaria%>%
  #thisone was done above. This approach is better, but we work with too small dataset
  #separate( col = DevType, into = c("DevTy1","DevTy2","DevTy3"), sep = ";")%>%
  mutate(DevType = str_split(DevType,";"))%>%
  unnest(DevType)%>%
  #mainly looking for GIT vs the Rest so 1 column will do
  #this is same as with str_split ( n = 1, simplify = T)
  separate( col = VersionControl, into = c("VersionControl"), sep = ";")%>%
  separate( col = YearsCoding, into = c("YearsCoding"), sep = "-") %>%
  separate( col = YearsCoding, into = c("YearsCoding"), sep = "-") %>%
  separate( col = YearsCodingProf, into = c("YearsCodingProf"), sep = "-")%>%
  #Only count for these for now. 
  mutate(LanguageWorkedWith = as.integer(str_count(LanguageWorkedWith,";")))%>%
  mutate(DatabaseWorkedWith = as.integer(str_count(DatabaseWorkedWith,";")))%>% 
  mutate(FrameworkWorkedWith = as.integer(str_count(FrameworkWorkedWith,";")))%>%   
  mutate(PlatformWorkedWith = as.integer(str_count(PlatformWorkedWith,";")))   

#Cast Few categories to Integer
Bulgaria$YearsCoding[Bulgaria$YearsCoding ==   "30 or more years"]<-"30"
Bulgaria$YearsCoding<-as.integer(Bulgaria$YearsCoding)
Bulgaria$YearsCodingProf<-as.integer(Bulgaria$YearsCodingProf)

#Formal education as factor
# Bulgaria$FormalEducation <- factor(Bulgaria$FormalEducation, 
#                                    levels = c(0, 1, 2, 3, 4, 5, 6, 7), 
#                                    labels = c("I never completed any formal education", 
#                                               "Primary/elementary school", 
#                                               "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)", 
#                                               "Some college/university study without earning a degree",
#                                               "Bachelor’s degree (BA, BS, B.Eng., etc.)",
#                                               
#                                               "Master’s degree (MA, MS, M.Eng., MBA, etc.)" ,
#                                               "Associate degree" ,
#                                               "Other doctoral degree (Ph.D, Ed.D., etc.)"
#                                               
#                                    )
# )
# #Relevel

#check what to keep



Bulgaria<-Bulgaria%>%
  select(
                 Salary,
                 VersionControl,
                 DevType,
                 YearsCodingProf,
                 YearsCoding,
                 IDE,
                 NumberMonitors,
                 FormalEducation,
                 Age,
                 CompanySize,
                 LanguageWorkedWith ,
                 DatabaseWorkedWith ,
                 FrameworkWorkedWith,
                 PlatformWorkedWith ,
                 Gender
                 )

#Turn all non-numeric values into factors
getcharCols <- map_chr(Bulgaria,is.character)
Bulgaria[,getcharCols==T]<-map(Bulgaria[,getcharCols==T], factor) 


# cor(Bulgaria) 
# Check correlations (as scatterplots), distribution and print corrleation coefficient 
# ggpairs(Bulgaria, cardinality_threshold = 20) 
# ggcorr(Bulgaria, method = c("everything", "pearson"))


#Set Default Theme 
# custom_theme <- theme_tufte() +
#   theme(legend.position = c(0.9, 0.9),
#         legend.title = element_text(face = "italic", size = 12),
#         axis.title = element_text(face = "bold", size = 14))
# theme_set(custom_theme)
# theme_set(theme_classic())

