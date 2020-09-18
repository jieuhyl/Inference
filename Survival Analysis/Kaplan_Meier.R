# Survival analysis: Kaplan-Meier

library(survival)

set.seed(123)
N      <- 180                  # number of observations
P      <- 3                    # number of groups
sex    <- factor(sample(c("f", "m"), N, replace=TRUE))  # stratification factor
X      <- rnorm(N, 0, 1)       # continuous covariate
IV     <- factor(rep(LETTERS[1:P], each=N/P))  # factor covariate
IVeff  <- c(0, -1, 1.5)        # effects of factor levels (1 -> reference level)
Xbeta  <- 0.7*X + IVeff[unclass(IV)] + rnorm(N, 0, 2)
weibA  <- 1.5                  # Weibull shape parameter
weibB  <- 100                  # Weibull scale parameter
U      <- runif(N, 0, 1)       # uniformly distributed RV
eventT <- ceiling((-log(U)*weibB*exp(-Xbeta))^(1/weibA))   # simulated event time

# censoring due to study end after 120 days
obsLen <- 120                  # length of observation time
censT  <- rep(obsLen, N)       # censoring time = end of study
obsT   <- pmin(eventT, censT)  # observed censored event times
status <- eventT <= censT      # has event occured?
dfSurv <- data.frame(obsT, status, sex, X, IV)          # data frame


# Plot simulated data
plot(ecdf(eventT), xlim=c(0, 200), main="Cumulative survival distribution",
     xlab="t", ylab="F(t)", cex.lab=1.4)
abline(v=obsLen, col="blue", lwd=2)
text(obsLen-5, 0.2, adj=1, labels="end of study", cex=1.4)


## global estimate
KM0 <- survfit(Surv(obsT, status) ~ 1,  type="kaplan-meier", conf.type="log", data=dfSurv)
KM0
summary(KM0)

## separate estimate for all strata
KM1 <- survfit(Surv(obsT, status) ~ IV, type="kaplan-meier", conf.type="log", data=dfSurv)
KM1
summary(KM1)


#Plot estimated survival function
#Global estimate including pointwise confidence intervals.
plot(KM0, main=expression(paste("Kaplan-Meier-estimate ", hat(S)(t), " with CI")),
     xlab="t", ylab="Survival", lwd=2)


#plot of chunk rerSurvivalKM02
#Separate estimates for levels of factor IV
plot(KM1, main=expression(paste("Kaplan-Meier-estimate ", hat(S)[g](t), " for groups g")),
     xlab="t", ylab="Survival", lwd=2, col=1:3)
legend(x="topright", col=1:3, lwd=2, legend=LETTERS[1:3])



#plot of chunk rerSurvival
#Plot cumulative hazard
plot(KM0, main=expression(paste("Kaplan-Meier-estimate ", hat(Lambda)(t))),
     xlab="t", ylab="cumulative hazard", fun="cumhaz", lwd=2)

plot(KM1, main=expression(paste("Kaplan-Meier-estimate ", hat(Lambda)(t))),
     xlab="t", ylab="cumulative hazard", fun="cumhaz", lwd=2, col=1:3)
legend(x="topright", col=1:3, lwd=2, legend=LETTERS[1:3])



# Log-rank-test for equal survival-functions
# Global test
survdiff(Surv(obsT, status) ~ IV, data=dfSurv)

# Stratified for factor sex
survdiff(Surv(obsT, status) ~ IV + strata(sex), data=dfSurv)