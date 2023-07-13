# Likhit Garimella
# Regression Analysis HW-5

# libraries
#install.packages("faraway")
#install.packages("olsrr")
#install.packages("car")
#install.packages("readr")

library(faraway)
library(olsrr)
library(car)
library(readr)

# (a) generate a basic scatter plot matrix

data <- read_csv('C:/Users/91630/Downloads/B17.csv',show_col_types = FALSE)
head(data, 5)
pairs(data[,1:4], pch = 19,lower.panel=NULL)

# (b) generate a correlation matrix

corr_matrix = cor(data)
round(corr_matrix,3)

#install.packages("Hmisc")
library("Hmisc")

correlation_matrix1 = rcorr(as.matrix(data))
correlation_matrix1

# (c) obtain least squares prediction equation

model = lm(Satisfaction~.,data=data)
summary(model)

# Based on the coefficients presented in the Model Summary, the prediction equation obtained through the least square method can be expressed as follows:
# Satisfaction = 140.168 - 1.1428 * Age - 0.4699 * Severity + 2.2259 * SurgicalMedical + 1.2673 * Anxiety

# (d) compute VIFs

VIF = vif(model)
print(VIF)

# (e) interpret the Beta estimate of Anxiety in the model

# The estimated Beta coefficient for Anxiety in the model is 1.2673, indicating that, on average, an increase in Anxiety is associated with an increase in Satisfaction.
# However, it's important to note that the p-value for the Anxiety coefficient is 0.4058, which is higher than the commonly accepted threshold of 0.05 for statistical significance.
# This suggests that the observed relationship between Anxiety and Satisfaction may not be statistically significant in the given model, and the estimated effect of Anxiety on Satisfaction may not be reliable.
# Furthermore, the Variance Inflation Factor (VIF) for Anxiety is 1.689768, indicating that there is no significant issue of multicollinearity between Anxiety and the other independent variables in the model.
# To summarize, while the Beta estimate for Anxiety suggests a positive relationship with Satisfaction, it is not statistically significant in the current model.
# Therefore, it is important to interpret this estimate with caution, as it may not provide strong evidence of a meaningful relationship between Anxiety and Satisfaction.

# (f) perform a partial F-test for Anxiety
model_without_anxiety = lm(Satisfaction~Age+Severity+SurgicalMedical, data=data)
anov = anova(model_without_anxiety, model)
print(anov)