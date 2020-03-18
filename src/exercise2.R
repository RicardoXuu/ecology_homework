rm(list = ls())

mydata <- read.csv("data/xy.csv")
mydata
head(mydata) 

plot(mydata$y ~ mydata$x, data = mydata, main="y ~ x")
par(mfrow=c(1, 2)) 
boxplot(mydata$x, main="x", sub=paste("Outlier rows: ", 
                                      boxplot.stats(mydata$x)$out))
boxplot(mydata$y, main="y", sub=paste("Outlier rows: ", 
                                      boxplot.stats(mydata$y)$out))

library(e1071)
par(mfrow=c(1, 2))  
plot(density(mydata$x), main="Density Plot: x", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(mydata$x), 2))) 
polygon(density(mydata$x), col="red")
plot(density(mydata$y), main="Density Plot: y", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  
polygon(density(mydata$y), col="red")

cor(mydata$x, mydata$y) 
linearMod <- lm(y ~ x, data= mydata)
print(linearMod)

summary(linearMod) 

