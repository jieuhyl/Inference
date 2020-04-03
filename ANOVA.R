# independent 2+ samples
DW <- read.table(file.choose(), header = T, sep = '\t')
attach(DW)


boxplot(WeightLoss ~ Diet)


# ANOVA
# H0: mean weight loss is same for all diets

aov(WeightLoss ~ Diet)
anova1 = aov(WeightLoss ~ Diet)
summary(anova1)

attributes(anova1)
anova1$coefficients


# Multiple-pairwise comparison 
TukeyHSD(anova1)

plot(TukeyHSD(anova1), las = 1)


# kruskal wallis
kruskal.test(WeightLoss ~ Diet)
