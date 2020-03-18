rm(list = ls())

percep_data <- read.csv("data/percep_data.csv")
head(percep_data)
str(percep_data)
percep_data <- percep_data[order(percep_data$Y),]
head(percep_data)
percepsubdf <- percep_data[c(1:100,901:1000),c(1, 2, 3)]
percepsubdf = percepsubdf[order(percepsubdf$Y),]
head(percepsubdf)

g1_x = percepsubdf[1:100,1]
g1_y = percepsubdf[1:100,2]
g2_x = percepsubdf[101:200,1]
g2_y = percepsubdf[101:200,2]
g_x = c(g1_x, g2_x)
g_y = c(g1_y, g2_y)
N = 100
group = c(rep(-1,N), rep(1,N))
head(group)

print(g_x)
print(g_y)
print(group)
plot(g_x, g_y, type='n', xlab='X', ylab='Y')
points(g1_x, g1_y, col='red')
points(g2_x, g2_y, col='blue')

θ0 = 0.1 
θ1 = 0.2
θ2 = 0.3

M = 15            
eta = 0.005       
th = 0.9         
verbose = F   
for (i in 1:M){
  print(paste('Epoch starts: ', i))
  
  
  index = 1:(2*N)
  index = sample(index)
  
  for (j in index){
    y_j = θ0 + θ1*g_x[j] + θ2*g_y[j]
    if (y_j >= 0){
      pred_j = 1
    }else{
      pred_j = -1}
    
    θ0 = θ0 + eta*(group[j] - pred_j)*1.0
    θ1 = θ1 + eta*(group[j] - pred_j)*g_x[j]
    θ2 = θ2 + eta*(group[j] - pred_j)*g_y[j]
    if (verbose == T){
      print(paste('  -> updating data point ', j, ' : '))
      print(paste('     -> θ0: ' ,θ0))
      print(paste('     -> θ0: ' ,θ1))
      print(paste('     -> θ0: ' ,θ2))
    }
  }  
  y_all = θ0 + θ1*g_x + θ2*g_y
  y_pred = y_all
  y_pred[y_all >= 0] = 1
  y_pred[y_all< 0] = -1
  
  acc = sum(y_pred == group)/length(group)
  print(paste('Epoch ends: ', i, ' WITH accuracy: ', acc))
  if (acc >= th){
    break
  }
}        
y_all = θ0 + θ1*g_x + θ2*g_y
print(y_all)

y_pred = y_all
y_pred[y_all >= 0] = 1
y_pred[y_all< 0] = -1

print(y_pred)

acc = sum(y_pred == group)/length(group)
print(acc)
plot(g_x, g_y, type='n', xlab='X', ylab='Y')
points(g1_x, g1_y, col='red')
points(g2_x, g2_y, col='blue')
abline(a = -1.0*θ0/θ2, b = -1.0*θ1/θ2, col='dark green', lwd=3, lty=2)





