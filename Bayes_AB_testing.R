install.packages("bayesAB")
library(bayesAB)
library(ggplot2)


# https://cran.r-project.org/web/packages/bayesAB/vignettes/introduction.html


# trace(bayesAB:::plotDist_, edit=TRUE)
# 
# function (support, hseq, dist, params) 
# {
#   discretes <- c("Poisson")
#   ribbon_or_bar <- ggplot2::geom_ribbon(ymin = 0, 
#                                         mapping = ggplot2::aes(ymax = hseq), # new version, with ymax placed inside aes()
#                                         size = 2, color = I("lightblue"), fill = "lightgreen", 
#                                         alpha = 0.25)
#   # ... omitted
# }
trace(bayesAB:::plotDist_, edit=TRUE)
function (support, hseq, dist, params) 
{
  discretes <- c("Poisson")
  ribbon_or_bar <- ggplot2::geom_ribbon(ymin = 0, 
                                        ymax = hseq,
                                        size = 2, color = I("lightblue"), fill = "lightgreen", 
                                        alpha = 0.25)
  # ... omitted
}

prop.test(x = c(6,10), n = c(16,16))

A_binom <- rbinom (16, 1, 0.375)
B_binom <- rbinom (16, 1, 0.625)


plotBeta(1,1)  # most commonly used as uniform distribution
plotBeta(3,25) # about 20% prior info

AB1 <- bayesTest(A_binom, B_binom,
                priors = c ('alpha' = 3, 'beta' = 25),
                distribution = 'bernoulli', n_samples=1000)
summary(AB1)
plot(AB1)


hist(AB1$posteriors$Probability$A)
hist(AB1$posteriors$Probability$B)
diff <- AB1$posteriors$Probability$B - AB1$posteriors$Probability$A
sum(diff>=0)/length(diff)

profitA = AB1$posteriors$Probability$A*1000 -30
profitB = AB1$posteriors$Probability$B*1000 -300
profit_diff = profitB - profitA
sum(profit_diff>=0) / length(profit_diff)


#==========================================================
#Poisson

A_pois <- rpois(250, 6.5)
B_pois <- rpois(250, 5.5)

plotGamma(30, 5) # 5-6 seem likely enough

AB2 <- bayesTest(A_pois, B_pois, 
                 priors = c('shape' = 30, 'rate' = 5), 
                 n_samples = 1e5, 
                 distribution = 'poisson')

print(AB2)
summary(AB2)
