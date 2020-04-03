# independence
LC <- read.table(file.choose(), header = T, sep = '\t')
attach(LC)

tab = table(Gender, Smoke)

barplot(tab, beside = T, legend = T)

help("chisq.test")
# chi-square test
# H0: there is no associate btw gender and smoke
chi = chisq.test(tab)

chi$expected

# fisher exact test
fisher.test(tab, conf.int = T, conf.level = 0.95)



# to test the strength of assocation 
install.packages('epiR')
library(epiR)

epi.2by2(tab, method = 'cohort.count', conf.level = 0.95)

# the odds of a male not smoking are 1.4 times the odds for a female not smoking
1/0.71

tab2 = matrix(c(44, 314, 33, 334), nrow = 2, byrow = T)
tab2

tab3 = cbind(tab[,2], tab[,1])
colnames(tab3) = c('yes', 'no')
epi.2by2(tab3, method = 'cohort.count', conf.level = 0.95)
