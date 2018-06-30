
#Check the clean process versus known mean (~3k)
mean(Bulgaria$Salary)
#each opperation above was compared against a target mean, because we work with extremely small dataset


##GGPLOT2 tests
#Point 1 - Why you should use GIT 
Bulgaria%>%group_by(VersionControl)%>% 
  summarise(MeanByVC=mean(Salary))

ggplot(Bulgaria,aes(x=Salary,y=VersionControl, col = IDE)) + 
  geom_point() + ggplot2::scale_x_log10()
#Point 2  - The POWER OF MONITORS!!!
Bulgaria%>%group_by(NumberMonitors)%>% 
  summarise(MeanPerMonitor=mean(Salary))

ggplot(Bulgaria,aes(x=NumberMonitors ,y=Salary,color = NumberMonitors))+
 # geom_point() + 
  stat_summary(geom = "linerange", fun.data = med_IQR,
               position = posn.d, size = 3) +
  stat_summary(geom = "linerange", fun.data = gg_range,
               position = posn.d, size = 3,
               alpha = 0.4) +
  stat_summary(geom = "point", fun.y = median,
               position = posn.d, size = 3,
               col = "black", shape = "M", size = 25)


#Point 3 - Foemal Education

Bulgaria%>%group_by(FormalEducation)%>% 
  summarise(MeanByVC=mean(Salary))

#Random Checks
BestPaidJobs<-Bulgaria[Bulgaria$Salary>2*sd(Bulgaria$Salary)+mean(Bulgaria$Salary)  ,]

MoneyBags <- Bulgaria[which.max(Bulgaria$Salary), ]

#clean the env
#rm(list=ls())
####Ducking Begins


Bulgaria%>%group_by(NumberMonitors)%>% 
  summarise(S=mean(Salary))


