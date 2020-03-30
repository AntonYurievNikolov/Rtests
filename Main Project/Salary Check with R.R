
###Check the clean process versus known mean (~3k)####
#each opperation above was compared against a target mean, because we work with extremely small datas
mean(Bulgaria$Salary)
ggplot(Bulgaria,aes(x=Salary))+
  geom_histogram(bins=15)+
 # scale_x_log10()+
  geom_vline(aes(xintercept = mean(Salary)),col='red',size=2) +
  geom_vline(aes(xintercept = median(Salary)),col='blue',size=2)+
  geom_vline(aes(xintercept = quantile(Bulgaria$Salary)[2]),col='yellow',size=2)+
geom_vline(aes(xintercept = quantile(Bulgaria$Salary)[4]),col='yellow',size=2)

ggplot(Bulgaria,aes(x = 1,y=Salary))+
  geom_boxplot()

###GGPLOT2 tests####


###Point 1 - Why you should use GIT ####
Bulgaria%>%group_by(VersionControl)%>% 
  summarise(MeanByVC=mean(Salary))

ggplot(Bulgaria,aes(x=Salary,col = IDE)) + 
  geom_histogram() + 
  facet_wrap(~VersionControl)+
  ggplot2::scale_x_log10()

ggplot(Bulgaria,aes(y=Salary,x=VersionControl)) + 
  geom_boxplot()+
  theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Point 2  - The POWER OF MONITORS!!!####
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
               col = "black", size = 25)


#zoom to 3-5k range
ggplot(Bulgaria,aes(x=Salary,y=NumberMonitors,color = NumberMonitors,size =Salary))+
  geom_point()+
  facet_wrap( ~as.factor(FormalEducation) ,shrink = T )+
  #scale_x_continuous(limits = c(3000,5000))
  #with zoom below
   coord_cartesian( xlim= c(3000,5000)) +
#theme
  theme(
    plot.background = element_blank(),
   # rect = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.title = element_text(hjust = 0, face = "italic"),
    axis.text = element_text(color = "black"),
    strip.text= element_text(hjust = 0, face = "italic"),
    legend.position = "bottom",
    legend.direction= "horizontal",
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.1, "cm"),
    plot.margin = unit(c(1,2,1,1), "cm")
  )
#+ coord_equal()
#,scale = "free_x", space = "free_x" 

#Point 3 - Foemal Education####

Bulgaria%>%group_by(FormalEducation)%>% 
  summarise(MeanByVC=mean(Salary))

ggplot(Bulgaria,aes(x=FormalEducation ,y=Salary, col = FormalEducation))+
  geom_point() + 
  theme_wsj()+
  theme(
       axis.text.x =   element_blank(),
      legend.position = "right",
      legend.direction= "vertical"
  ) 

#Pie chart  
ggplot(Bulgaria,aes(x=1 ,fill=FormalEducation))+ 
  geom_bar()+
  coord_polar(theta = "y")+
  theme_classic()

#Point 4 - years coding####
ggplot(Bulgaria,aes(x=YearsCoding ,y=Salary))+
  geom_point()+
  stat_smooth(aes(group = 1))+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)


ggplot(Bulgaria,aes(x=YearsCodingProf   ,y=Salary))+
  geom_point()+
  stat_smooth(method = "lm")



#Random Checks
BestPaidJobs<-Bulgaria[Bulgaria$Salary>2*sd(Bulgaria$Salary)+mean(Bulgaria$Salary)  ,]

MoneyBags <- Bulgaria[which.max(Bulgaria$Salary), ]

#clean the env
#rm(list=ls())
####Ducking Begins
Bulgaria%>%
  group_by(NumberMonitors)%>% 
  summarise(S=mean(Salary))

Bulgaria%>%group_by(DevType,Gender )%>% 
  summarise(S=mean(Salary))%>%
  arrange(desc(S))
sd(Bulgaria$Salary)
#T.TEST
c(rnorm(75, mean = 3000, sd = 2000))
test<-t.test(Bulgaria$Salary, y = Bulgaria$Salary-1000,
       mu = 0, paired =F, 
       conf.level = 0.95)


qnorm(1-test$p.value)
