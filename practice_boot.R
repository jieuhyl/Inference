rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Data Science/Bayesian AB_testing")
set.seed(1337)

library(boot)
#==================================================================================================
### BOOTSTRAP HYPOTHESIS TEST ###

# load in the chicken diet data (save it in "d")
d <- read.table(file="C:/Users/Jie.Hu/Desktop/Data Science/Bayesian AB_testing/ChickData.csv", header=T, sep=",")
# this data is a subset of the "chickwts" data in the
# "R datasets package"

# let's add the data into the "data view"
View(d)


samplemean <- function(x, d) 
{
  return(mean(x[d]))
}

samplemedian <- function(x, d) 
{
  return(median(x[d]))
}


samplemedian1 <- function(x,d) 
{
  return(median(x[d][1:12]) - median(x[d][13:23]))
}

bs = boot(d$weight, samplemedian1, R=10000) 

boot.ci(boot.out = bs, type = c("basic", "perc", "bca"), conf = 0.95)



samplemedian2 <- function(x,d) 
{
  return(abs(median(x[d][1:12]) - median(x[d][13:23])))
}

bs = boot(d$weight, samplemedian2, R=10000) 

boot.ci(boot.out = bs, type = c("basic", "perc", "bca"), conf = 0.95)




set.seed(1234)

diff = function(d1,i){
  d = d1; 
  d$feed <- d$feed[i];  # randomly re-assign groups
  Measure= tapply(X=d$weight, INDEX=d$feed, median)
  Diff = abs(Measure[1]-Measure[2])
  Diff
}

b = boot(data = d, statistic = diff, R = 10000)

pvalue = mean(abs(b$t) >= abs(b$t0))
pvalue 

boot.ci(boot.out = b, type = c("basic", "perc", "bca"), conf = 0.95)

quantile(b$t,c(0.025,0.975))















#==================================================
time = c(14,18,11,13,18,17,21,9,16,17,14,15,
         12,12,14,13,6,18,14,16,10,7,15,10)
group=c(rep(1:2, each=12))
sleep = data.frame(time, group)


diff2 = function(d1,i){
  d = d1; 
  d$group <- d$group[i];  # randomly re-assign groups
  Mean= tapply(X=d$time, INDEX=d$group, mean)
  Diff = Mean[1]-Mean[2]
  Diff
}

set.seed(1234)
b4 = boot(data = sleep, statistic = diff2, R = 5000)

pvalue = mean(abs(b4$t) > abs(b4$t0))
pvalue 

boot.ci(boot.out = b4, type = c("basic", "perc", "bca"), conf = 0.95)

quantile(b4$t,c(0.025,0.975))











