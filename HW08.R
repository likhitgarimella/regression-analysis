# Likhit Garimella
# Regression Analysis HW-8

# libraries
#install.packages("faraway")
#install.packages("lmtest")
#install.packages("epiDisplay")
library(faraway)
library(lmtest)
library(epiDisplay)

# import the dataset
p13_7 = read.csv("/Users/ravi/Downloads/p13_7modified.csv",header=TRUE)

# save output in a pdf file
pdf(file="/Users/ravi/Downloads/HW08OutputLikhit.pdf")

head(p13_7)
summary(p13_7)
nullmodel <- glm(y~1,family=binomial(logit),data = p13_7)

# (a) Fit a logistic regression model to the data.
logistic = glm(y~x1+x2+x3+x4,family=binomial(logit),data = p13_7)
summary(logistic)
anova(logistic, test="Chisq")

#b) Test H0: all Beta-parameters are 0 versus at least one Beta-parameter is not 0.
#H0: B1=B2=B3=B4=0
#H1: at least one beta is not 0
lrtest(nullmodel,logistic)
#p-value is 1.124085e-06 (<0.05, very low) Reject null hypothesis, there is a significant relationship

#c) Plot the observed binary responses versus the fitted values.
plot(logistic$fitted.values,p13_7$y)
abline(v=0.5) 

#d) Use the logistic.display function found in the epiDisplay library to show statistics for 
# each individual parameter from the logistic regression performed in part a.
logistic.display(logistic)

dev.off()
