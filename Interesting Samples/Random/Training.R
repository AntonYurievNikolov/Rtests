tom <- hotdogs[which.max(hotdogs$sodium), ]


#dyplr
Bulgaria%>%group_by(NumberMonitors)%>% 
  summarise(MeanPerMonitor=mean(Salary))

hflights %>% 
  group_by(UniqueCarrier, Dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)

#check new DB
lahmanNames %>%  
  bind_rows(.id="dataframe") %>%
  group_by(var) %>%
  tally() %>%
  filter(n>1) %>% 
  arrange(desc(n))


#use these
#bind_rows(),bind_cols()
# lookupmatching

# The lookup table
lut <- c("A" = "carrier", "B" = "weather", "C" = "FFA", "D" = "security", "E" = "not cancelled")
# Add the Code column
hflights$Code <-lut[hflights$CancellationCode]

#tydir
bmi_long <- gather(bmi,year , bmi_val, -Country)
bmi_wide <- spread(bmi_long, year, bmi_val)
separate(bmi_cc, col = Country_ISO, into = c("Country", "ISO"), sep = "/")
unite(bmi_cc_clean, Country_ISO, Country, ISO, sep = "-")
#widening data
iris$Flower <- 1:nrow(iris)
iris.wide <- iris %>%
  gather(key, value, -Species ) %>%
  separate(key, c("Part", "Measure"), "\\.") %>%
  spread(Measure, value)

#tibble
stage_songs %>% 
  rownames_to_column("song") %>% 
  left_join(stage_writers)

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
#Random
#paste date
c2<-mutate(c1,Date=paste(Year, Month, DayofMonth, sep = "-"))

as.numeric(as.character(year))#coerce factors

#sampling and statistics
tab <- table(Bulgaria$YearsCoding, Bulgaria$FormalEducation)
options(scipen = 999, digits = 3) # Print fewer digits
prop.table(tab)     # Joint proportions
prop.table(tab, 2)  # Conditional on columns
