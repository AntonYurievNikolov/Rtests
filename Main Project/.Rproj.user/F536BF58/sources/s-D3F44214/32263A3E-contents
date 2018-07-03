##Regressions
#fitting multiple models

library(tidyr)
library(purrr)
library(broom)
# Perform a linear regression on each item in the data column
coefficent<-Bulgaria %>%
  select(Salary,FormalEducation,IDE) %>%
  nest(-IDE) %>%
  mutate(model = map(data, ~ lm(Salary ~ FormalEducation, data = .)))%>%
  mutate(tidied = map(model , tidy)) %>%
  unnest(tidied)

coefficent%>%mutate(p.adjusted  = p.adjust(p.value))%>%filter(p.adjusted <0.05)

#check this for the whole world
#WOW
coefficentWorld<-WorldData %>%
  select(Salary,FormalEducation,Country) %>%
  filter(!is.na(Country),!is.na(FormalEducation),!is.na(Salary))%>%
  group_by(Country)%>%
  mutate(n = n())%>%
  filter(n>100)%>%
  ungroup()%>%
  mutate(FormalEducation = as.factor(FormalEducation),Country = as.factor(Country))%>%
  nest(-Country) %>%
  #filter(Country=="United Kingdom") %>%
  mutate(
         model = map(data, ~ lm(Salary ~ FormalEducation, data = .)))%>% 
         mutate(tidied = map(model , tidy))  %>% 
  unnest(tidied)

coefficentWorld%>%mutate(p.adjusted  = p.adjust(p.value))%>%filter(p.adjusted <0.05)
#cut example
ggplot(data = Bulgaria, 
       aes(x = cut(Bulgaria$YearsCoding , breaks = 3), y = Bulgaria$Salary)) + 
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
  arrange(desc(.hat)) %>%
  head()

