# Survival analysis: Cox proportional hazards model

library(survival)

set.seed(123)
N      <- 180
P      <- 3
sex    <- factor(sample(c("f", "m"), N, replace=TRUE))
X      <- rnorm(N, 0, 1)
IV     <- factor(rep(LETTERS[1:P], each=N/P))
IVeff  <- c(0, -1, 1.5)
Xbeta  <- 0.7*X + IVeff[unclass(IV)] + rnorm(N, 0, 2)
weibA  <- 1.5
weibB  <- 100
U      <- runif(N, 0, 1)
eventT <- ceiling((-log(U)*weibB*exp(-Xbeta))^(1/weibA))
obsLen <- 120
censT  <- rep(obsLen, N)
obsT   <- pmin(eventT, censT)
status <- eventT <= censT
dfSurv <- data.frame(obsT, status, sex, X, IV)


# Fit the Cox proportional hazards model
fitCPH <- coxph(Surv(obsT, status) ~ X + IV, data=dfSurv)
fitCPH
summary(fitCPH)


# Assess model fit
# AIC
extractAIC(fitCPH)

#McFadden, Cox & Snell and Nagelkerke pseudo R2
#Log-likelihoods for full model and 0-model without predictors X1, X2
LLf <- fitCPH$loglik[2]
LL0 <- fitCPH$loglik[1]

#McFadden pseudo-R2
as.vector(1 - (LLf / LL0))

#Cox & Snell
as.vector(1 - exp((2/N) * (LL0 - LLf)))

#Nagelkerke
as.vector((1 - exp((2/N) * (LL0 - LLf))) / (1 - exp(LL0)^(2/N)))


# Model comparisons
fitCPH1 <- coxph(Surv(obsT, status) ~ X, data=dfSurv)
anova(fitCPH1, fitCPH)    


# Estimate survival-function und cumulative baseline hazard
# Survival function
(CPH <- survfit(fitCPH))


#More meaningful: Estimated survival function for new specific data
dfNew  <- data.frame(sex=factor(c("f", "f"), levels=levels(dfSurv$sex)),
                     X=c(-2, -2),
                     IV=factor(c("A", "C"), levels=levels(dfSurv$IV)))
CPHnew <- survfit(fitCPH, newdata=dfNew)

par(mar=c(5, 4.5, 4, 2)+0.1, cex.lab=1.4, cex.main=1.4)
plot(CPH, main=expression(paste("Cox PH-estimate ", hat(S)(t), " with CI")),
     xlab="t", ylab="Survival", lwd=2)
lines(CPHnew$time, CPHnew$surv[ , 1], lwd=2, col="blue")
lines(CPHnew$time, CPHnew$surv[ , 2], lwd=2, col="red")
legend(x="topright", lwd=2, col=c("black", "blue", "red"),
       legend=c("pseudo-observation", "sex=f, X=-2, IV=A", "sex=f, X=-2, IV=C"))


# Cumulative baseline hazard
expCoef  <- exp(coef(fitCPH))
Lambda0A <- basehaz(fitCPH, centered=FALSE)
Lambda0B <- expCoef[2]*Lambda0A$hazard
Lambda0C <- expCoef[3]*Lambda0A$hazard
plot(hazard ~ time, main=expression(paste("Cox PH-estimate ", hat(Lambda)[g](t), " per group")),
     type="s", ylim=c(0, 5), xlab="t", ylab="cumulative hazard", lwd=2, data=Lambda0A)
lines(Lambda0A$time, Lambda0B, lwd=2, col="red")
lines(Lambda0A$time, Lambda0C, lwd=2, col="green")
legend(x="bottomright", lwd=2, col=1:3, legend=LETTERS[1:3])


# Model diagnostics
# Proportional hazards assumption
(czph <- cox.zph(fitCPH))

par(mfrow=c(2, 2), cex.main=1.4, cex.lab=1.4)
plot(czph)


# Influential observations
dfbetas <- residuals(fitCPH, type="dfbetas")

par(mfrow=c(2, 2), cex.main=1.4, cex.lab=1.4)
plot(dfbetas[ , 1], type="h", main="DfBETAS for X",    ylab="DfBETAS", lwd=2)
plot(dfbetas[ , 2], type="h", main="DfBETAS for IV-B", ylab="DfBETAS", lwd=2)
plot(dfbetas[ , 3], type="h", main="DfBETAS for IV-C", ylab="DfBETAS", lwd=2)

# Linearity of log hazard
resMart <- residuals(fitCPH, type="martingale")
plot(dfSurv$X, resMart, main="Martingale-residuals for X",
     xlab="X", ylab="Residuen", pch=20)
lines(loess.smooth(dfSurv$X, resMart), lwd=2, col="blue")
legend(x="bottomleft", col="blue", lwd=2, legend="LOESS fit", cex=1.4)



# Predicted hazard ratios
hazRat <- predict(fitCPH, type="risk")
head(hazRat)