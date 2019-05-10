library(tidyverse)
library(lubridate)
# setwd("D:\\study\\Rtests\\Anomaly Detection")
data<-read_csv("AD.csv",col_names = TRUE)
#3/5/2019 11:04 . getting the correct lubridate format
data$when<-mdy_hm(data$when)
summary(data)

