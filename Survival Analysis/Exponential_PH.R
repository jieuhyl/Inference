# Survival analysis: Parametric proportional hazards models

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

# Exponential model = restricted Weibull model with shape parameter a=1.
fitExp <- survreg(Surv(obsT, status) ~ X + IV, dist="exponential", data=dfSurv)
summary(fitExp)


# Weibull-model
fitWeib <- survreg(Surv(obsT, status) ~ X + IV, dist="weibull", data=dfSurv)
summary(fitWeib)

# model comparison
AIC(fitExp, fitWeib)       

# restricted model without IV
fitR <- survreg(Surv(obsT, status) ~ X, dist="weibull", data=dfSurv)
anova(fitR, fitWeib) 


# Estimate survival-function
#Apply fit to new data with specific observations. Result from predict() is the estimated distribution function F^???1(t) for given percentiles p.
dfNew <- data.frame(sex=factor(c("m", "m"), levels=levels(dfSurv$sex)),
                    X=c(0, 0),
                    IV=factor(c("A", "C"), levels=levels(dfSurv$IV)))
percs <- (1:99)/100
FWeib <- predict(fitWeib, newdata=dfNew, type="quantile", p=percs, se=TRUE)


#To plot estimated survival function for two new observations, calculate S(t)=1???F(t) and use newdata argument.
matplot(cbind(FWeib$fit[1, ],
              FWeib$fit[1, ] - 2*FWeib$se.fit[1, ],
              FWeib$fit[1, ] + 2*FWeib$se.fit[1, ]), 1-percs,
        type="l", main=expression(paste("Weibull-Fit ", hat(S)(t), " mit SE")),
        xlab="t", ylab="Survival", lty=c(1, 2, 2), lwd=2, col="blue")
matlines(cbind(FWeib$fit[2, ],
               FWeib$fit[2, ] - 2*FWeib$se.fit[2, ],
               FWeib$fit[2, ] + 2*FWeib$se.fit[2, ]), 1-percs, col="red", lwd=2)
legend(x="topright", lwd=2, lty=c(1, 2, 1, 2), col=c("blue", "blue", "red", "red"),
       legend=c("sex=m, X=0, IV=A", "+- 2*SE", "sex=m, X=0, IV=C", "+- 2*SE"))
