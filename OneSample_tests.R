# one sample
LC <- read.table(file.choose(), header = T, sep = '\t')
attach(LC)


boxplot(LungCap)

# one sample t test ======================================
# H0: mean of lung cap < 8
# one sided
t.test(LungCap, mu = 8, alt = 'less', conf = 0.95)

# two sided
t.test(LungCap, mu = 8, alt = 'two.sided', conf = 0.95)
