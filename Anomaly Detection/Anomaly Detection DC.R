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
