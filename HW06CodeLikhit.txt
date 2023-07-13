# Likhit Garimella
# Regression Analysis HW-6

# libraries
#install.packages("faraway")
#install.packages("olsrr")
#install.packages("car")
#install.packages("readr")

library(faraway)
library(olsrr)
library(car)
library(readr)

# (a)
# importing dataset
p3_1 <- read_csv('C:/Users/91630/Downloads/B1.csv',show_col_types = FALSE)
head(p3_1, 5)

# saving output in a pdf file
pdf(file="C:/Users/91630/Desktop/HW06plotsLikhit.pdf")

# creating data frame with the required columns
df <- data.frame(y = p3_1$y, x2 = p3_1$x2, x7 = p3_1$x7, x8 = p3_1$x8)
lmod = lm(y ~ x2+x7+x8,data=p3_1)

# outputs using summary and anova statements
summary(lmod)
anova(lmod)

# (b) generating a Q-Q plot
qqnorm(residuals(lmod),ylab="Residuals",main="Q-Q plot")
qqline(residuals(lmod))

# (c) residual plots
# (i) x2 plot
plot(p3_1$x2,residuals(lmod),xlab="x2",ylab="Residuals")
abline(h=0)

# (ii) x7 plot
plot(p3_1$x7,residuals(lmod),xlab="x7",ylab="Residuals")
abline(h=0)

# (iii) x8 plot
plot(p3_1$x8,residuals(lmod),xlab="x8",ylab="Residuals")
abline(h=0)

# (d) partial regression plots
avPlots(lmod,print_plot = TRUE)

# (e) standardized and studentized plots
ols_plot_resid_stand(lmod)$data  
ols_plot_resid_stud(lmod)$data

dev.off()