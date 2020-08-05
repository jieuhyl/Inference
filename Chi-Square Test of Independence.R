#http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

rm(list = ls())

setwd("C:/Users/Jie.Hu/Desktop/TURF/0804")
set.seed(1337)

library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

# read data
df <- read.csv('chiq_test.csv', header=T, stringsAsFactors=FALSE)

df[c(3:7)][df[c(3:7)] > 1] <- 0

a <- table(df$QNATUREVIEWERCODE, df$QCONCEPTSr1)[,2]
b <- table(df$QNATUREVIEWERCODE, df$QCONCEPTSr2)[,2]
c <- table(df$QNATUREVIEWERCODE, df$QCONCEPTSr3)[,2]
d <- table(df$QNATUREVIEWERCODE, df$QCONCEPTSr4)[,2]
e <- table(df$QNATUREVIEWERCODE, df$QCONCEPTSr5)[,2]

tb <- rbind(a,b,c,d,e)
#ttb <- t(tb)
chisq <- chisq.test(tb) 


round(chisq$residuals, 3)


corrplot(chisq$residuals, is.cor = FALSE)

# Contribution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

# Visualize the contribution
corrplot(contrib, is.cor = FALSE)




# example ====================================================================
# Import the data
file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)

chisq <- chisq.test(housetasks)
chisq
# In our example, the row and the column variables are statistically significantly associated (p-value = 0).

round(chisq$residuals, 3)
corrplot(chisq$residuals, is.cor = FALSE)

#Positive residuals are in blue. Positive values in cells specify an attraction (positive association) between 
#the corresponding row and column variables.
#In the image above, it's evident that there are an association between the column Wife and the rows Laundry, Main_meal.
#There is a strong positive association between the column Husband and the row Repair
#Negative residuals are in red. This implies a repulsion (negative association) between the corresponding row and column variables. 
#For example the column Wife are negatively associated (~ "not associated") with the row Repairs. 
#There is a repulsion between the column Husband and, the rows Laundry and Main_meal



# Contribution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

#The relative contribution of each cell to the total Chi-square score give some indication of the nature of the dependency 
#between rows and columns of the contingency table.

#From the image above, it can be seen that the most contributing cells to the Chi-square are Wife/Laundry (7.74%), 
#Wife/Main_meal (4.98%), Husband/Repairs (21.9%), Jointly/Holidays (12.44%).

#These cells contribute about 47.06% to the total Chi-square score and thus account for most of the difference between expected and observed values.

#This confirms the earlier visual interpretation of the data. As stated earlier, visual interpretation may be 
#complex when the contingency table is very large. In this case, the contribution of one cell to the total Chi-square score 
#becomes a useful way of establishing the nature of dependency.
