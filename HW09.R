# Likhit Garimella
# Regression Analysis HW-9

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

# Use the p-values vector to list the variables with coefficients that are significantly different from 0

# (a) using NO FWER adjustment (use alpha=.05 for each test of hypothesis).

alpha_no_FWER <- 0.05
No_FWER_variables <- names(p_values[p_values < alpha_no_FWER])
print(No_FWER_variables)

# (b) using the Bonferroni procedure to control the FWER alpha at 0.05.

alpha_bonferroni <- 0.05 / length(p_values)
bonferroni_variables <- names(p_values)[p_values < alpha_bonferroni]
print(bonferroni_variables)

# (c) using the Holm procedure to control the FWER alpha at 0.05.

adjusted_pvalues<- p.adjust(p_values, method = "holm")
holm_variables <- names(p_values)[adjusted_pvalues < alpha_no_FWER]
print(holm_variables)