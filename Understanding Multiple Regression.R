#############################
#IST387
#Emily Korzendorfer

dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

library(ggplot2)
#install.packages("RCurl")
library(RCurl)
#download.file("http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls", destfile = 'mlr01.xls')
#install.packages("readxl")
library(readxl)
library(tidyverse)

df <- data.frame(read_excel('mlr01.xls'))
getwd()

cnames <- c('springCount', 'adultPop', 'precp', 'winterSev')
colnames(df) <- cnames

View(df)

#colnames(df)[colnames(df)=="misspelled column name"] <- "correct column name"

plot(df$springCount, df$adultPop)
#This creates a bivariate plot showing the number of baby fawns versus adult antelope population. There is a dirrect correlation for antelope population to number of baby faws. In order for there to be many baby fawns there needs to be a large adult population

plot(df$springCount, df$precp)
#The lower the precipitation the lower the baby fawn count, and the same for higher

plot(df$springCount, df$winterSev)
#There is no obvious correlation between the two variables. There is a slight trend towards severe winters having lower baby fawn counts and less sever having higher baby fawn counts

single1 <- lm(formula=springCount ~ adultPop, data=df)
summary(single1)
#R-Squared value: 0.8813, Adjusted: 0.8616
#The R-Squared value is relatively close to 1.0 which would be a perfect prediction. This means that this regression model is a good precictor

#The p-value for this model is 0.0005471 which is less than 0.05 amking it statistically significant
#The intercept p-value is also a stistically significant value. It is 0.038152 which is less than 0.05


#Interpretation of the model:
#The adult antelope population accounts for 88.13% of the baby fawn count. The remaining ~12% is accounted for by other independent variables.
#The slope term for this model is saying that for every adult there is 0.49753 more fawns
#This model rejects the null hypothesis because the variables have have a relationship

multipleRegress <- lm(formula=springCount ~ adultPop + precp + winterSev, data=df)
summary(var2)

#Interpretation:
#The three independent variables (adultPop, precp, and winterSev) account for 95.5% of the baby fawn count. The remaining 4.5% is accounted for by unexplained variants
#The significant values are the p-values for all three independent variables and the intercept p-value.
#The p-value for adultPop is 0.0273 which is significant 
#The p-value for precp is 0.0217 which is significant
#The p-value for winterSev is 0.0366 which is significant
#The intercept p-value is 0.0092 which is significant
#The R-Squared p-value is 0.001229 which is significant
#This Multiple Regression model rejects the Null hypothesis because all of the variables have a relationship with the dependent variable (springCount)








