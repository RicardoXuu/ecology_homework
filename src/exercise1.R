rm(list = ls())

library("ade4")
data("doubs")
str(doubs)
doubs$env
head(doubs$env)
tail(doubs$env)
write.csv(doubs$env,"data/doubs_env.csv")

mydata <- read.csv("data/doubs_env.csv")
head(mydata)
mydata <- mydata[,-1]
hist(mydata$alt,
     col = "green",
     main = "altitude distribution",
     xlab = "altitude")

library(corrplot)
res1 <- cor(mydata)
write.table(res1,"results/coeff.txt",sep = "\t")

library(psych)
res2 <- corr.test(mydata)
res2$r
res2$ci
corr_fig <- corrplot(res1)
pairs.panels(mydata[,2:6])

