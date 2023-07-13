# Likhit Garimella
# Regression Analysis HW-4

# libraries
#install.packages("faraway")
#install.packages("olsrr")
#install.packages("car")
#install.packages("readr")

library(faraway)
library(olsrr)
library(car)
library(readr)

# importing dataset
myData <- read.csv('C:/Users/91630/Downloads/B5.csv', header = T, sep = ",")

# (a) fitting multiple regression model
fitMod <- lm(y ~ x6 + x7, data = myData)
summ <- summary(fitMod)
fitMod

# (b) test for significance of regression, calculating R^2 and R_adj^2
summ

# (d) constructing 95% CIs on B_6 & B_7
CI <- round(confint(fitMod, level = 0.95), 4)
CI

# (e) refitting the model using only x6 as regressor
fitMod1 <- lm(y ~ x6, data = myData)
summ1 <- summary(fitMod1)
summ1

# (f) constructing a 95% CI on B_6 using the above model fit
CI <- round(confint(fitMod1, level = 0.95), 4)
CI

# (g) comparing values of MS_Res obtained for 2 models from (a) & (e)
anov1 <- anova(fitMod)
anov2 <- anova(fitMod1)
anov1

# analysis
anov2