rm(list = ls())

mydata <- read.csv("data/xy.csv") 
x <- mydata$x
y <- mydata$y

cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

alpha <- 0.00001 
num_iters <- 1000 
cost_history <- rep(0,num_iters) 
theta_history <- list(num_iters) 
theta <-  matrix(c(0,0), nrow = 2) 
X <- cbind(1,x) 
for (i in 1:num_iters) { 
  theta[1] <- theta[1] - alpha * (1/length(y)) * sum(((X%*%theta)- y))
  theta[2] <- theta[2] - alpha * (1/length(y)) * sum(((X%*%theta)- y)*X[,2])
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
} 
print(theta)


plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')

for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) { 
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue') 

