# Likhit Garimella
# Regression Analysis HW-1

# libraries
#install.packages("faraway")
#install.packages("olsrr")
#install.packages("psych")
#install.packages("readr")
library(faraway)
library(olsrr)
library(psych)
library(readr)

# (a) fitting the linear regression model
# import the dataset
myData = read.csv("/Users/likhitgarimella/Desktop/SummerSemester/prob2_18.csv")
myData=myData[order(myData$amount),]
# print the dataset
myData
# extract the columns from dataset to model
lmod = lm(ret_impress ~ amount, data=myData)

# save output in a pdf file
pdf(file="/Users/likhitgarimella/Desktop/SummerSemester/HW01plotsLikhit.pdf")

# (b) graph b/w amount and retained impressions
plot(ret_impress ~ amount, data = myData, xlab = "Amount Spent", ylab = "Retained Impressions", main = "Likhit Garimella lg836 | Scatterplot Linear Regression Line")
# line
abline(lmod)
# graph summary
summary(lmod)
anova(lmod)

# (c) get confidence band
# plot old graph
plot(ret_impress ~ amount, data = myData, xlab = "Amount Spent", ylab = "Retained Impressions")
confband = predict(lmod,interval="confidence",level=0.95)
head(confband)
confband

# plot with confidence bands
lines(myData[,2],confband[,2],col="red",lty=2)
lines(myData[,2],confband[,3],col="red",lty=2)

# (c) get prediction band
predband = predict(lmod,interval="prediction",level=0.95)
head(predband)
predband

# plot with prediction bands
lines(myData[,2],predband[,2],col="green",lty=2)
lines(myData[,2],predband[,3],col="green",lty=2)

newdf = data.frame(amount=seq(5,185.9,by=0.5)) 
head(newdf)
plot(ret_impress ~ amount, data = myData, xlab = "Amount Spent", ylab = "Retained Impressions")
abline(lmod)

confband = predict(lmod,newdata = newdf,interval="confidence",level=0.95)
newdfc = cbind(newdf,confband)

# plot with confidence bands
lines(newdfc[,1],newdfc[,2],col="red",lty=2)
lines(newdfc[,1],newdfc[,3],col="red",lty=2)
lines(newdfc[,1],newdfc[,4],col="red",lty=2)

predband = predict(lmod,newdata = newdf,interval="predict",level=0.95)
newdfp = cbind(newdf,predband)

# plot with prediction bands
lines(newdfp[,1],newdfp[,2],col="green",lty=2)
lines(newdfp[,1],newdfp[,3],col="green",lty=2)
lines(newdfp[,1],newdfp[,4],col="green",lty=2)

# confidence and prediction intervals
confint(lmod) 
predict(lmod,newdata=data.frame(amount=50.4),interval="confidence",level=0.95) 
predict(lmod,newdata=data.frame(amount=50.4),interval="prediction",level=0.95)
# summary
summary(predict(lmod))
summary(residuals(lmod))

# residuals
ei = residuals(lmod)  
SSE=sum(ei^2)   
head(cbind(myData,lmod$fitted.values,ei))
summary(ei)
describe(ei)
(lmod_olsrr = ols_regress(ret_impress ~ amount, data=myData))
ols_plot_resid_hist(lmod)
dev.off()