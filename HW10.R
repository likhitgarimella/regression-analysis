# Likhit Garimella
# Regression Analysis HW-10

# Perform a multiple regression on all 100 predictors from the meatspec dataset in the faraway package and extract the p-values for the coefficients of the 100 variables.

# libraries
#install.packages("faraway")
library(faraway)

data(meatspec,package="faraway")
# perform multiple regression on all 100 variables
lmod <- lm(fat ~ ., data = meatspec)
# extract the p-values for the coefficients of the 100 variables (excluding the intercept)
p_values <- summary(lmod)$coefficients[, 4]
# print the p-values
print(p_values)

# Use the p-values vector to list the variables with coefficients that are significantly different from 0 using the Benjamini-Hochberg procedure with an FDR of 0.2

Benjamini_Hochberg_p <- p.adjust(p_values, method = "BH")
Benjamini_Hochberg_variables = names(Benjamini_Hochberg_p[Benjamini_Hochberg_p<=0.2])
Benjamini_Hochberg_variables