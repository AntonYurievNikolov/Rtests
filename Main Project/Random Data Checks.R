glimpse(Bulgaria)
mean(Bulgaria$Salary)
mean(Bulgaria$YearsCoding)
mean(Bulgaria$Age[!is.na(Bulgaria$Age)])

ByAge <- Bulgaria %>%
          group_by(Age) %>%
          summarise(AVG = mean(Salary), Deviation = sd(Salary), Number = n())

plot(ByAge$Age,ByAge$AVG)
