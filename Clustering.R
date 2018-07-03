#scaling -standartization t value
scale(three_trees)
#Jicard similiarities for binary + dumification
Bulgaria$FormalEducation

dummy_survey <- dummy(as.factor(Bulgaria$DevTy1))

# Calculate the Distance
dist_survey <- dist(dummy_survey,  method = 'binary')


unnest(DevType) %>%
  group_by(DevType) %>%
  summarise(Median_Salary = median(ConvertedSalary,na.rm = TRUE))