# http://www.sthda.com/english/wiki/cox-proportional-hazards-model
# Cox Proportional-Hazards Regression


install.packages(c("survival", "survminer"))
library("survival")
library("survminer")


data("lung")
head(lung)


# Univariate Cox regression
res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
res.cox
summary(res.cox)

covariates <- c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = lung)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)


# Multivariate Cox regression analysis
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
res.cox
summary(res.cox)

# Visualizing the estimated distribution of survival times
# Plot the baseline survival function
ggsurvplot(survfit(res.cox, data = lung), 
           palette = "#2E9FDF",
           ggtheme = theme_minimal())


# Cumulative baseline hazard
expCoef  <- exp(coef(res.cox))
Lambda0A <- basehaz(res.cox, centered=FALSE)
Lambda0B <- expCoef[2]*Lambda0A$hazard
Lambda0C <- expCoef[3]*Lambda0A$hazard
plot(hazard ~ time, main=expression(paste("Cox PH-estimate ", hat(Lambda)[g](t), " per group")),
     type="s", ylim=c(0, 4), xlab="t", ylab="cumulative hazard", lwd=2, data=Lambda0A)
lines(Lambda0A$time, Lambda0B, lwd=2, col="red")
lines(Lambda0A$time, Lambda0C, lwd=2, col="green")
legend(x="bottomright", lwd=2, col=1:3, legend=LETTERS[1:3])



# Create the new data  
sex_df <- with(lung,
               data.frame(sex = c(1, 2), 
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)
sex_df

# Survival curves
fit <- survfit(res.cox, newdata = sex_df)
ggsurvplot(fit, 
           conf.int = TRUE, 
           legend.labs=c("Sex=1", "Sex=2"),
           data = sex_df,
           ggtheme = theme_minimal())

