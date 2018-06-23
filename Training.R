tom <- hotdogs[which.max(hotdogs$sodium), ]
#GGPLOT
ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,color=continent,size=pop)) + geom_point()+scale_x_log10()+
  facet_wrap(~year)+expand_limits(y = 0)
#lineplot
ggplot(year_continent, aes(x = year, y = meanLifeExp, color = continent)) +
  geom_line() +
  expand_limits(y = 0)


#dyplr
Bulgaria%>%group_by(NumberMonitors)%>% 
  summarise(MeanPerMonitor=mean(Salary))
#tydir
bmi_long <- gather(bmi,year , bmi_val, -Country)
bmi_wide <- spread(bmi_long, year, bmi_val)
separate(bmi_cc, col = Country_ISO, into = c("Country", "ISO"), sep = "/")
unite(bmi_cc_clean, Country_ISO, Country, ISO, sep = "-")

#lubridate
#stringr
students2$sex <- str_replace(students2$sex,"M" , "Male")
str_detect(names(WorldData),"JobContactPrior")
#MIssing values
complete.cases(social_df)
na.omit(social_df)
any(is.na(social_df))

weather6[ind, ]
ind <- which(is.na(weather6$Max.Gust.SpeedMPH))
#Clean
#DontWant1<-str_detect(names(WorldData),"Assess")+
# str_detect(names(WorldData),"JobContactPrior")+
# str_detect(names(WorldData)," HypotheticalT")+
# str_detect(names(WorldData)," StackOverflowRe")+
# str_detect(names(WorldData)," Ads")+
# str_detect(names(WorldData)," JobEmai")+
# str_detect(names(WorldData)," AI")+
# str_detect(names(WorldData)," StackOverflow")+
# str_detect(names(WorldData)," Hypothetical")