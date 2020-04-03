rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/Data Science/Bayesian AB_testing")
set.seed(1337)


#==================================================================================================
### BOOTSTRAP HYPOTHESIS TEST ###

# load in the chicken diet data (save it in "d")
d <- read.table(file="C:/Users/Jie.Hu/Desktop/Data Science/Bayesian AB_testing/ChickData.csv", header=T, sep=",")
# this data is a subset of the "chickwts" data in the
# "R datasets package"

# let's add the data into the "data view"
View(d)

# check the names, etc
names(d)
levels(d$feed)
# how many observations in each diet?
table(d$feed)

# let's look at a boxplot of weight gain by those 2 diets
boxplot(d$weight~d$feed, las=1, ylab="weight (g)", 
        xlab="feed",main="Weight by Feed")

# calculate the difference in sample means
mean(d$weight[d$feed=="casein"])  # mean for casein
mean(d$weight[d$feed=="meatmeal"])  # mean for meatmeal
# and, a fancier way to do that...
with(d, tapply(weight, feed, mean))
# lets calculate the absolute diff in means
test.stat1 <- abs(mean(d$weight[d$feed=="casein"]) - mean(d$weight[d$feed=="meatmeal"]))  #diff in means
test.stat1
# and, a fanceir way to do that...
abs( diff( with(d, tapply(weight, feed, mean)) ) )

# and, the same for the medians
median(d$weight[d$feed=="casein"])  # median for casein
median(d$weight[d$feed=="meatmeal"])  # median for meatmeal
# and, a fancier way to do that...
with(d, tapply(weight, feed, median))
# lets calculate the absolute diff in medians
test.stat2 <- abs(median(d$weight[d$feed=="casein"]) - median(d$weight[d$feed=="meatmeal"]))  #diff in medians
test.stat2
# and, a fanceir way to do that...
abs( diff( with(d, tapply(weight, feed, median)) ) )

## let's take a look at the 3 "Classic" hyp tests we could 
# consider (each of which comes with their own limitations...)

# let's look at the Independent 2-sample t-test
se1 = sd(d$weight[d$feed=="casein"])^2
se2 = sd(d$weight[d$feed=="meatmeal"])^2
se1/se2       
t.test(d$weight~d$feed, paired=F, var.eq=T)  # tests Ho: means are equal
t.test(d$weight~d$feed, paired=F, var.eq=F)  # tests Ho: means are equal
# let's look at the Wilcoxon aka Mann-Whitney U 
wilcox.test(d$weight~d$feed, paired=F)  # tests Ho: medians are equal
# let's look at the Kolmogorov-Smirnov 2-sample test
ks.test(d$weight[d$feed=="casein"], d$weight[d$feed=="meatmeal"], paired=F)     # tests Ho: distributions are same

########################
##  BOOTSTRAPPING... ###
########################

# let's bootstrap...
set.seed(112358)   # for reproducibility
n <- length(d$feed)  # the number of observations to sample
n
B <- 10000  # the number of bootstrap samples
variable <- d$weight  # the variable we will resample from

# now, get those bootstrap samples (without loops!)
BootstrapSamples <- matrix( sample(variable, size= n*B, replace=TRUE), 
                            nrow=n, ncol=B)
# let's take a moment to discuss what that code is doing...
dim(BootstrapSamples)

# now, calculate the means (Yc and Ym) for each of the bootstrap samples
#  (the inefficeint, but transparent way...best to start simple, and once
#   working well, then make code more efficent)
# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)
Boot.test.stat2 <- rep(0,B)
# run through a loop, each time calculating the bootstrap test.stat
#  NOTE: could make this faster by writing a "function" and then
#        using "apply" to apply it to columns of the "BootSamples"
for (i in 1:B){
  # calculate the boot-test-stat1 and save it
  Boot.test.stat1[i] <- abs( mean(BootstrapSamples[1:12,i]) - 
                               mean(BootstrapSamples[13:23,i]) )
  # calculate the boot-test-stat2 and save it
  Boot.test.stat2[i] <- abs( median(BootstrapSamples[1:12,i]) - 
                               median(BootstrapSamples[13:23,i])  )
}

# before going too far, let's remind ourselves of the OBSERVED TEST STATS
test.stat1; test.stat2
# and, take a look at the first 20 Bootstrap-TEST STATS for 1 and 2
round(Boot.test.stat1[1:20], 1)
round(Boot.test.stat2[1:20], 1)

# and, let's calculate the bootstrap p-value...
# notice how we can ask R a true/false question...(for the first 20)
(Boot.test.stat1 >= test.stat1)[1:20]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
#...calculate the p-value
mean(Boot.test.stat1 >= test.stat1)

# let's calculate the p-value for test statistic 2 (abs diff in medians)
mean(Boot.test.stat2 >= test.stat2)


# now, recall the difference between "statistical significance" and 
# "scientific significance"
### in a "real-world" what would you want to conclude here
table(d$feed)

# let's take a look at a density plot of all the Bootstrap test-stats, and 
# add in our Observed test stat
plot(density(Boot.test.stat1), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Bootstrap Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)

###########################
### Code to run the analysis, using a test stat of diff in 90th percentiles
###########################

# lets calculate the absolute diff in 90th percentiles
test.stat3 <- abs(quantile(d$weight[d$feed=="casein"], prob=0.9) - quantile(d$weight[d$feed=="meatmeal"], prob=0.9))  #diff in medians
test.stat3

# initialize a vector to save the bootstrap test stats in
Boot.test.stat3 <- rep(0,B)

# run thru a loop calculating the bootstrap test statistics
for (i in 1:B){
  # calculate the boot-test-stat3 and save it
  Boot.test.stat3[i] <- abs( quantile(BootstrapSamples[1:12,i], prob=0.9) - 
                               quantile(BootstrapSamples[13:23,i], prob=0.9) )
}

# and, calculate the p-value
mean(Boot.test.stat3 >= test.stat3)



#===================================================================================================
##### BOOTSTRAP CONFIDENCE INTERVAL #####
###  (comparing 2 numeric variables)  ###

# load in the chicken diet data (save it in "d")
d <- read.table(file="C:/Users/Jie.Hu/Desktop/Data Science/Bayesian AB_testing/ChickData.csv", header=T, sep=",")
# this data is a subset of the "chickwts" data in the
# "R datasets package"

# let's add the data into the "data view"
View(d)

# check the names, etc
names(d)
levels(d$feed)
# how many observations in each diet?
table(d$feed)

# let's look at a boxplot of weight gain by those 2 diets
boxplot(d$weight~d$feed, las=1, ylab="weight (g)", 
        xlab="feed",main="Weight by Feed")

# calculate the difference in sample means
mean(d$weight[d$feed=="casein"])  # mean for casein
mean(d$weight[d$feed=="meatmeal"])  # mean for meatmeal
# and, a fancier way to do that...
with(d, tapply(weight, feed, mean))
# lets calculate the diff in means:   (casein - meatmeal)
Obs.Diff.In.Means <- (mean(d$weight[d$feed=="casein"]) - mean(d$weight[d$feed=="meatmeal"]))  #diff in means
Obs.Diff.In.Means
# and, a fanceir way to do that...  (- to have it be casein-meatmeal)
-diff( with(d, tapply(weight, feed, mean)) ) 

# and, the same for the medians
median(d$weight[d$feed=="casein"])  # median for casein
median(d$weight[d$feed=="meatmeal"])  # median for meatmeal
# and, a fancier way to do that...
with(d, tapply(weight, feed, median))
# lets calculate the diff in medians:  (casein - meatmeal)
Obs.Diff.In.Medians <- (median(d$weight[d$feed=="casein"]) - median(d$weight[d$feed=="meatmeal"]))  #diff in medians
Obs.Diff.In.Medians
# and, a fanceir way to do that...  (- to have it be casein-meatmeal)
-diff( with(d, tapply(weight, feed, median)) ) 

###################################
### BOOTSTRAP CONFIDENCE INTERVAL
###################################

# let's run through making conf ints for the difference in means and medians

# let's bootstrap...
set.seed(13579)   # set a seed for consistency/reproducability
n.c <- 12  # the number of observations to sample from casein
n.m <- 11  # the number of observations to sample from meatmeal
B <- 100000  # the number of bootstrap samples...go big or go home right?

# now, get those bootstrap samples (without loops!)
# stick each Boot-sample in a column...
Boot.casein <- matrix( sample(d$weight[d$feed=="casein"], size= B*n.c, 
                              replace=TRUE), ncol=B, nrow=n.c)
Boot.meatmeal <- matrix( sample(d$weight[d$feed=="meatmeal"], size= B*n.m, 
                                replace=TRUE), nrow=n.m, ncol=B)
# check those
dim(Boot.casein); dim(Boot.meatmeal)

# check to make sure they are not empty!
Boot.casein[1:5,1:5]
Boot.meatmeal[1:5,1:5]

# calculate the difference in MEANS for each of the bootsamples
Boot.Diff.In.Means <- colMeans(Boot.casein) - colMeans(Boot.meatmeal)
# check that
length(Boot.Diff.In.Means)
# and, look at the first 10 diff in means
Boot.Diff.In.Means[1:10]

# calculate the difference in MEDIANS for each of the bootsamples
Boot.Diff.In.Medians <- apply(Boot.casein, MARGIN=2, FUN=median) -
  apply(Boot.meatmeal, MARGIN=2, FUN=median)
# check that
length(Boot.Diff.In.Medians)
# and, look at the first 10 diff in medians
Boot.Diff.In.Medians[1:10]

#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

# let's look at the PERCENTILE METHOD
# the "PERCENTILE" bootstrap confidence interval
# first, for the difference in MEANS
quantile(Boot.Diff.In.Means, prob=0.025)
quantile(Boot.Diff.In.Means, prob=0.975)

# and then, the difference in MEDIANS
quantile(Boot.Diff.In.Medians, prob=0.025)
quantile(Boot.Diff.In.Medians, prob=0.975)

### What do you make of the fact that these both cross 0?

### Apart from "statistical significance", what do you think about
###    "scientific significance" here?

# below is code to calculate confidence interval using the BASIC method

# let's look at the BASIC METHOD
# first, for the difference in MEANS
2*Obs.Diff.In.Means - quantile(Boot.Diff.In.Means, prob=0.975)
2*Obs.Diff.In.Means - quantile(Boot.Diff.In.Means, prob=0.025)
# and then, the difference in MEDIANS
2*Obs.Diff.In.Medians - quantile(Boot.Diff.In.Medians, prob=0.975)
2*Obs.Diff.In.Medians - quantile(Boot.Diff.In.Medians, prob=0.025)

########
### Code for confidence interval for difference in 80th percentiles
########

# calculate the observed difference in 80th percentiles
Obs.Diff.In.80per <- (quantile(d$weight[d$feed=="casein"], prob=0.80) - quantile(d$weight[d$feed=="meatmeal"], prob=0.80))
Obs.Diff.In.80per

# calculate the difference in 80th percentile for each of the bootsamples
Boot.Diff.In.80per <- apply(Boot.casein, MARGIN=2, FUN=quantile, prob=0.80) - apply(Boot.meatmeal, MARGIN=2, FUN=quantile, prob=0.80)

# let's look at the PERCENTILE METHOD for the difference in 80th percentile
quantile(Boot.Diff.In.80per, prob=0.025)
quantile(Boot.Diff.In.80per, prob=0.975)


#=================================================================================================
### PERMUTATION HYPOTHESIS TEST ###

# load in the chicken diet data (save it in "d")
d <- read.table(file="C:/Users/Jie.Hu/Desktop/Data Science/Bayesian AB_testing/ChickData.csv", header=T, sep=",")
# this data is a subset of the "chickwts" data in the
# "R datasets package"

# let's add the data into the "data view"
View(d)

# check the names, etc
names(d)
levels(d$feed)
# how many observations in each diet?
table(d$feed)

# let's look at a boxplot of weight gain by those 2 diets
boxplot(d$weight~d$feed, las=1, ylab="weight (g)", 
        xlab="feed",main="Weight by Feed")

# calculate the difference in sample MEANS
mean(d$weight[d$feed=="casein"])  # mean for casein
mean(d$weight[d$feed=="meatmeal"])  # mean for meatmeal
# lets calculate the absolute diff in means
test.stat1 <- abs(mean(d$weight[d$feed=="casein"]) - 
                    mean(d$weight[d$feed=="meatmeal"])) 
test.stat1


# calculate the difference in sample MEDIANS
median(d$weight[d$feed=="casein"])  # median for casein
median(d$weight[d$feed=="meatmeal"])  # median for meatmeal
# lets calculate the absolute diff in medians
test.stat2 <- abs(median(d$weight[d$feed=="casein"]) - median(d$weight[d$feed=="meatmeal"]))  #diff in medians
test.stat2

##########################
###  PERMUTATION TEST  ###
##########################

# let's permute...

# for reproducability of results
set.seed(1979)  
# the number of observations to sample
n <- length(d$feed)  
# the number of permutation samples to take
P <- 100000 
# the variable we will resample from 
#     (note, could use the labels(feed) too, and "shuffle this")
variable <- d$weight  

# initialize a matrix to store the permutation data
PermSamples <- matrix(0, nrow=n, ncol=P)
# each column is a permutation sample of data

# now, get those permutation samples, using a loop
# let's take a moment to discuss what that code is doing...
for(i in 1:P){
  PermSamples[,i] <- sample(variable, size= n, replace=FALSE)
}

# we can take a quick look at the first 5 columns of PermSamples
PermSamples[, 1:5]

# now, let's run thru a loop, calculating the test-stats for 
# each sample
### NOTE: could do all these in ONE step...
###       my suggestion, first write code that works, then 
###       tighten it up and make it more efficient
###       (use an 'apply' statement, and write the function 
###        you want)
###    i keep code transparent for teaching purpose

# initialize vectors to store all of the Test-stats:
Perm.test.stat1 <- Perm.test.stat2 <- rep(0, P)

# loop thru, and calculate the test-stats
for (i in 1:P){
  # calculate the perm-test-stat1 and save it
  Perm.test.stat1[i] <- abs( mean(PermSamples[d$feed=="casein",i]) - 
                               mean(PermSamples[d$feed=="meatmeal",i]) )
  # calculate the perm-test-stat2 and save it
  Perm.test.stat2[i] <- abs( median(PermSamples[d$feed=="casein",i]) - 
                               median(PermSamples[d$feed=="meatmeal",i]) )
}

# before going too far with this, let's remind ourselves of 
# the TEST STATS
test.stat1; test.stat2
# and, take a look at the first 15 permutation-TEST STATS for 1 and 2
round(Perm.test.stat1[1:15], 1)
round(Perm.test.stat2[1:15], 1)

# and, let's calculate the permutation p-value...
# notice how we can ask R a true/false question...(for the first 15)
(Perm.test.stat1 >= test.stat1)[1:15]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
mean((Perm.test.stat1 >= test.stat1)[1:15])

#...calculate the p-value, for all P=100,000
mean(Perm.test.stat1 >= test.stat1)

# and, let's calculate the p-value for 
# option 2 of the test statistic (abs diff in medians)
mean(Perm.test.stat2 >= test.stat2)

# now, remember the difference between "statistical significance" 
# and "scientific significance"


########################
### SOME EXTRA STUFF ###
########################

# calculate the mean for each group
with(d, tapply(weight, feed, mean))
# calculate the difference in means for each group
abs(diff(with(d, tapply(weight, feed, mean)) ) )

# and calculate the median for each group
with(d, tapply(weight, feed, median))
# and, calculate the difference in medians 
abs(diff( with(d, tapply(weight, feed, median)) ) )

## let's take a look at the 3 "Classic" hyp tests we could 
# consider (each of which comes with their own limitations...)

# let's look at the Independent 2-sample t-test
# tests Ho: means are equal
t.test(d$weight~d$feed, paired=F, var.eq=F)  
# let's look at the Wilcoxon aka Mann-Whitney U 
# tests Ho: medians are equal
wilcox.test(d$weight~d$feed, paired=F)  
# let's look at the Kolmogorov-Smirnov 2-sample test
# tests Ho: distributions are same
ks.test(d$weight[d$feed=="casein"], d$weight[d$feed=="meatmeal"], paired=F)     

### produce a plot that shows the sampling distribution, and p-value
### for the Permutation approach, for test.stat1
# let's take a look at a density plot of all the Permutation 
# test-stats, and add in our Observed test stat
plot(density(Perm.test.stat1), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Permutation Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)

######
######

### Below is code to do the same as Permutation test as above, except 
### this time to shuffle the "labels" (the factor feed type), rather 
### than shuffle the observations
###  NOTE: numeric answer will change slightly, as this will end
###        up with a different set of permutations as above...

# for reproducability of results
set.seed(12345)  
# the number of observations to sample
n <- length(d$feed)  
# the number of permutation samples to take
P <- 10000  
# the variable we will resample from 
#     (note, could use the labels(feed) too, and "shuffle this")
variable <- d$feed 

### NOTE, we are going to sample with replacement from "feed" this 
###    time, and not from "weight"

# initialize a matrix to store the permutation data
PermSamplesOther <- matrix(0, nrow=n, ncol=P)
# each column is a permutation sample of data

# now, get those permutation samples, using a loop
for(i in 1:P){
  PermSamplesOther[,i] <- sample(variable, size= n, replace=FALSE)
}
# let's take a moment to discuss what that code is doing...
dim(PermSamplesOther)
# and look to see the first 5 columns of this...
PermSamplesOther[,1:5]
# note that here "1"="casein" and "2"="meatmeal"

# now, let's run thru a loop, calculating the test-stats for 
# each sample.  i keep code transparent for teaching purpose

# initialize vectors to store all of the Test-stats:
Perm.test.stat1b <- Perm.test.stat2b <- rep(0, P)

# loop thru, and calculate the test-stats
for (i in 1:P){
  # calculate the perm-test-stat1 and save it
  Perm.test.stat1b[i] <- abs( mean(d$weight[PermSamplesOther[,i]=="1"]) - 
                                mean(d$weight[PermSamplesOther[,i]=="2"]) )
  # calculate the perm-test-stat2 and save it
  Perm.test.stat2b[i] <- abs( median(d$weight[PermSamplesOther[,i]=="1"]) - 
                                median(d$weight[PermSamplesOther[,i]=="2"]) )
}

# before going too far with this, let's remind ourselves of 
# the TEST STATS
test.stat1; test.stat2
# and, take a look at the first 15 permutation-TEST STATS for 1 and 2
round(Perm.test.stat1b[1:15], 1)
round(Perm.test.stat2b[1:15], 1)

# let's take a look at a density plot of all the Permutation 
# test-stats, and add in our Observed test stat
plot(density(Perm.test.stat1b), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Permutation Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)

# and, let's calculate the permutation-test p-value...
mean(Perm.test.stat1b >= test.stat1)

# and, let's skip the plots, and just calculate the p-value for 
# option 2 of the test statistic (abs diff in medians)
mean(Perm.test.stat2b >= test.stat2)
