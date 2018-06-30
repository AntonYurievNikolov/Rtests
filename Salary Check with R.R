
#Check the clean process versus known mean (~3k)
mean(Bulgaria$Salary)
#each opperation above was compared against a target mean, because we work with extremely small dataset

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

ggplot(Bulgaria,aes(x=Salary,y=NumberMonitors,color = NumberMonitors,size =Salary))+
  geom_point()+
facet_wrap( ~as.factor(FormalEducation)  )+
scale_x_continuous(limits = c(0,10000)) +
  stat_smooth(method="lm",se=F)



#Random Checks
BestPaidJobs<-Bulgaria[Bulgaria$Salary>2*sd(Bulgaria$Salary)+mean(Bulgaria$Salary)  ,]

MoneyBags <- Bulgaria[which.max(Bulgaria$Salary), ]

#clean the env
#rm(list=ls())
####Ducking Begins


Bulgaria%>%group_by(NumberMonitors)%>% 
  summarise(S=mean(Salary))


