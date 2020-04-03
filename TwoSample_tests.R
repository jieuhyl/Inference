# independent 2 samples
LC <- read.table(file.choose(), header = T, sep = '\t')
attach(LC)


boxplot(LungCap ~ Smoke)

# two samples t test ======================================
# H0: mean of lung cap of smoker = of non smoker
# two sided
# assume equal var
t.test(LungCap ~ Smoke, mu = 0, alt = 'two.sided', paired = F, conf = 0.95,
       var.eq = T)

# assume non-equal var - Welch test
t.test(LungCap ~ Smoke, mu = 0, alt = 'two.sided', paired = F, conf = 0.95,
       var.eq = F)

# s1/s1 (0.5, 2)
sd(LungCap[Smoke == 'yes'])
sd(LungCap[Smoke == 'no'])

# Levene test for variance
# H0: equal variances
library(car)
levene.test(LungCap ~ Smoke)



# wilcoxon rank sum test aka Mann Whitney U test
# H0: median of lung cap of smoker = of non smoker
# two sided
wilcox.test(LungCap ~ Smoke, mu = 0, alt = 'two.sided', paired = F,
            conf.int = T, conf.level = 0.95,
            exact = T, correct = T)

# approx p-value and ci
wilcox.test(LungCap ~ Smoke, mu = 0, alt = 'two.sided', paired = F,
            conf.int = T, conf.level = 0.95,
            exact = F, correct = T)



# kolmogorov-smirnov
# H0: distribution of two types are the same
# two sided
ks.test(LungCap[Smoke=='yes'], LungCap[Smoke=='no'], alt = 'two.sided', 
        paired = T, exact = F)

