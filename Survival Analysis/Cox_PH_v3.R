# http://www.sthda.com/english/wiki/cox-model-assumptions
# Cox Model Assumptions


install.packages(c("survival", "survminer"))
library("survival")
library("survminer")


data("lung")
head(lung)

# Computing a Cox model
res.cox <- coxph(Surv(time, status) ~ age + sex + wt.loss, data = lung)
res.cox

 

# Testing proportional hazards assumption
test.ph <- cox.zph(res.cox)
test.ph

ggcoxzph(test.ph)

plot(test.ph[2])
abline(h=0, col=2)


# Testing linearity
ggcoxfunctional(Surv(time, status) ~ age, data = lung)


plot(x = predict(res.cox), y = residuals(res.cox, type = 'deviance'), main="martingale-residuals Plot",
     xlab="Fitted values", ylab="Residuals", pch=20)
abline(h=0)
lines(smooth.spline(predict(res.cox), residuals(res.cox, type = 'deviance')), lwd=2, col="red")
legend(x="bottomleft", col="blue", lwd=2, legend="LOESS fit", cex=1.4)



# Testing influential observations
ggcoxdiagnostics(res.cox, 
                 type = "dfbeta",
                 linear.predictions = FALSE, 
                 ggtheme = theme_bw())


ggcoxdiagnostics(res.cox, 
                 type = "deviance",
                 linear.predictions = FALSE, 
                 ggtheme = theme_bw())
