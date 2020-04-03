# dependent sample
BP <- read.table(file.choose(), header = T, sep = '\t')
attach(BP)


boxplot(Before, After)

plot(Before, After)
abline(0, 1)


# paired t test ======================================
# H0: mean diff is 0
# two sided
#alternative = c("two.sided", "less", "greater"),
t.test(Before, After, mu = 0, alt = 'two.sided', paired = T, conf.level = 0.95)


# wilcoxon signed rank test ==========================
# H0: median diff is 0
# two sided
wilcox.test(Before, After, mu = 0, alt = 'two.sided', paired = T, 
            conf.int = T, conf.level = 0.95)

# approx p-value and ci
wilcox.test(Before, After, mu = 0, alt = 'two.sided', paired = T, 
            conf.int = T, conf.level = 0.95,
            exact = F, correct = F)