# Survival analysis in R
# http://www.sthda.com/english/wiki/survival-analysis-basics
# Kaplan-Meier


install.packages(c("survival", "survminer"))
library("survival")
library("survminer")

data("lung")
head(lung)

fit <- survfit(Surv(time, status) ~ sex, data = lung)
print(fit)
# Summary of survival curves
summary(fit)

d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower
)
head(d)

# Visualize survival curves
# Change color, linetype by strata, risk.table color by strata
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))




ggsurvplot(
  fit,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 200,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("Male", "Female"),    # change legend labels.
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)

summary(fit)$table


# For example, to plot cumulative events, type this:
#Note that, three often used transformations can be specified using the argument fun:
#"log": log transformation of the survivor function,
#"event": plots cumulative events (f(y) = 1-y). It's also known as the cumulative incidence,
#"cumhaz" plots the cumulative hazard function (f(y) = -log(y))
ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")


# Kaplan-Meier life table: summary of survival curves
res.sum <- surv_summary(fit)
head(res.sum)


# Log-Rank test comparing survival curves: survdiff()
surv_diff <- survdiff(Surv(time, status) ~ sex, data = lung)
surv_diff




