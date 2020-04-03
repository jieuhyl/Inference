# correlations
LC <- read.table(file.choose(), header = T, sep = '\t')
attach(LC)

plot(Age, LungCap)

# (x,y) are linearly related ==>. Pearson (parametric)
cor(Age, LungCap, method = 'pearson')

# (x,y) are linear in the ranks ==> Spearman (non-parametric) 
cor(Age, LungCap, method = 'spearman')

# alternative to when sample size is small and has many tied ranks ==> Kendall (non-parametric) 
cor(Age, LungCap, method = 'kendall')


cor.test(Age, LungCap, method = 'pearson')
?cor.test
