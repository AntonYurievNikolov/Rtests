for (i in seq_along(df)) {
  # Change code to store result in output
  print(median(df[[i]]))
}
library(purrr)
#type consistent
map(cyl, ~ lm(mpg ~ wt, data = .))
#pipes
summaries <- models%>%map( summary)%>%
  map_dbl("r.squared")
#multimap
n <- list(5, 10, 20)
mu<-list(1, 5, 10)
map2(n,mu, rnorm)

sd<-list(0.1,1,0.1)
pmap(list(n, mu,sd), rnorm)
#
sims<-invoke_map(functionlist, params,n = 5)
#generate multiple side effects
walk(sims,hist)

#Checks
if (any(map_dbl(class_list, length) > 1)) {
  stop("Some columns have more than one class", call. = FALSE)
}
#Avoiding NSE(non-standardEvaluation)