##Regressions
#fitting multiple models

library(tidyr)
library(purrr)
library(broom)
# Perform a linear regression on each item in the data column
coefficent<-by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)))%>%
  mutate(tidied = map(model , tidy)) %>%
  unnest(tidied)

#mutate(p.adjusted  = p.adjust(p.value))%>%filter(p.adjusted <0.05)
#cut example
ggplot(data = Bulgaria, 
       aes(x = cut(Bulgaria$YearsCoding , breaks = 3), y = Bulgaria$Salary)) + 
  geom_boxplot()

# Compute correlation for all non-missing pairs
Bulgaria %>%
  summarise(N = n(), r = cor(YearsCodingProf, Salary, use = "pairwise.complete.obs"))

cor<-cor( Bulgaria$Salary,Bulgaria$YearsCodingProf, use = "pairwise.complete.obs")
mod<-lm(Salary~YearsCodingProf,data = Bulgaria)
summary(mod)
fitted.values(mod)
augment(mod)

mean(fitted.values(mod)) - mean(Bulgaria$Salary) < 200
mean(residuals(mod))
predict(mod,newdata = Bulgaria[Bulgaria$YearsCodingProf == 12,] %>% select(Salary,YearsCodingProf))
Salary<-c(4800)
YearsCodingProf<-c(11)
test<-data.frame(Salary,YearsCodingProf)
predict(mod,newdata =test)
#fitting the alogorythm