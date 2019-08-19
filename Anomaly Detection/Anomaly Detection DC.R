Bulgaria<-Bulgaria%>%
  select(
    Salary,
    # VersionControl,
    # DevType,
    YearsCodingProf,
    YearsCoding,
    # IDE,
    # NumberMonitors,
    # FormalEducation,
    # Age,
    # CompanySize,
    # LanguageWorkedWith ,
    # DatabaseWorkedWith ,
    # FrameworkWorkedWith,
    # PlatformWorkedWith ,
    # Gender
  )

# install.packages('outliers')outliers 
library(outliers)
#Visual Based
hist(Bulgaria$Salary, xlab = "Salary", breaks = 10)
#Grub Test
grubbs.test(Bulgaria$Salary)
n<-which.max(Bulgaria$Salary)
grubbs.test(Bulgaria$Salary[-n])

#ANOMALIES IN TIME SERIES####
plot(Salary ~YearsCodingProf, data = Bulgaria, type = "o")
monthly_mean <- tapply(Bulgaria$Salary, Bulgaria$YearsCodingProf, FUN = mean)
monthly_mean
plot(monthly_mean, type = "o", xlab = "Years", ylab = "Salary mean")
boxplot(Salary ~ YearsCodingProf, data = Bulgaria)

#Anomaly Detection Using Seasonal Hybrid ESD Test
# install.packages('anomalyDetection')
# install.packages("devtools")
 # devtools::install_github("twitter/AnomalyDetection",force=T)
library(AnomalyDetection)
s_anomalies <- AnomalyDetectionVec(x = Bulgaria$Salary, period = 2, direction = 'both', plot = T)
s_anomalies$anoms
print(s_anomalies$plot)


#Multivariate Technigues####
# install.packages('FNN')
library(FNN)
Bulgaria_scale <- scale(Bulgaria)
s_nn <- get.knn(Bulgaria_scale, k = 5)#kmeans(Bulgariascaled,4)
s_nnd <- rowMeans(s_nn$nn.dist)
which.max(s_nnd)
Bulgaria$score <- s_nnd
plot(YearsCoding ~ Salary, data = Bulgaria, cex = sqrt(score), pch = 20)
# 
# #Local Outlier Factor - LOF
# Bulgaria_lof <- lof(scale(Bulgaria), k = 5)
# Bulgaria$score <- Bulgaria_lof
# plot(YearsCoding ~ Salary, data = Bulgaria, cex = score, pch = 20)
#Isolation Forest####
# install.packages('isofor')
# wine_forest <- iForest(wine, nt = 100, phi = 200)
# wine_score <- predict(wine_forest, newdata = wine)
# wine$score <- wine_score
# Sequence of values for pH and alcohol
# ph_seq <- seq(min(wine$pH),  max(wine$pH), length.out = 25)
# alcohol_seq <- seq(min(wine$alcohol),  max(wine$alcohol), length.out = 25)
# wine_grid <- expand.grid(pH = ph_seq, alcohol = alcohol_seq)
# plot(pH ~ alcohol, data = wine_grid, pch = 20)
# wine_grid$score <- predict(wine_forest, wine_grid)
# contourplot(score ~ pH + alcohol, data = wine_grid, region = TRUE)

#Showcase - Still meaningless as all libraries are not part of CRAN (cannot be used properly with PBI)####
# head(thyroid)
# table(thyroid$label)
# prop_disease <- 22 / 1000
# thyroid_forest <- iForest(thyroid[, -1], nt = 200, phi = 100)
# thyroid$iso_score <- predict(thyroid_forest, thyroid[, -1])
# boxplot(iso_score ~ label, data = thyroid, col = "olivedrab4")
# 
# # Scale the measurement columns of thyroid
# scaled_thyroid_measurements <- scale(thyroid[, -1])
# lof_score <- lof(scaled_thyroid_measurements, k = 10)
# high_lof <- quantile(lof_score, probs = 0.98)  
# thyroid$binary_lof <- as.numeric(lof_score >= high_lof)
# high_iso <- quantile(iso_score, probs = 0.98)  
# thyroid$binary_iso <- as.numeric(iso_score >= high_iso)
# table(thyroid$label, thyroid$binary_iso)
# table(thyroid$label, thyroid$binary_lof)
# iso_prop <- (970 + 12) / 1000
# # lof_prop <- (958 + 0) / 1000
# 
# #Working with categorical features
# # Calculate Gower's distance matrix
# thyroid_dist <- daisy(thyroid[, -1], metric = "gower")
# 
# # Generate LOF scores for thyroid data
# thyroid_lof <- lof(thyroid_dist, k = 10)
# 
# # Range of values in the distance matrix
# range(as.matrix(thyroid_dist))