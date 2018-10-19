########################################################
##            Gradient Descent - Demonstração          #
########################################################

# Ilustraremos minimizando x^2
# Derivada de x^2 = 2*x
x = 3
converg = F
lrn = 0.001
plot(function(x){x^2},-5,5)
while(converg == F){
  old_x <- x # old value
  points(old_x,old_x^2) # Plotar no Gráfico cada ponto
  x <- x - lrn*(2*(x)) # Gradient Técnic
  new_x <- x # att value
  if(abs(new_x - old_x) < 0.000000001){
    converg = T
    print(x)
  } # criterio de convergencia
}
######################################################3
theta = c(0,0,0)
x <- data.frame(x0 = rep(1,32), x1 = qsec, x2 = hp)
y <- data.frame(mpg)

gradientDesc(x,y,0.01,0.000001,32,100000)

gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter) {
  theta = c(1,1,1)
  yhat <- theta[1]*x[1] + theta[2]*x[2] + theta[3]*x[3] 
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    t1_new <- theta[1] - learn_rate * ((1 / n) * (sum((y-yhat) * x[1])))
    t2_new <- theta[2] - learn_rate * ((1 / n) * (sum((y-yhat) * x[2])))
    t3_new <- theta[3] - learn_rate * ((1 / n) * (sum((y-yhat) * x[3])))
    theta[1] <- t1_new
    theta[2] <- t2_new
    theta[3] <- t3_new
    
    yhat <- t1_new*x[1] + t2_new*x[2] + t3_new*x[3] 
    MSE_new <- sum((y - yhat) ^ 2) / n
    if(MSE - MSE_new <= conv_threshold) {
      converged = T
      return(paste("theta0:", t1_new, "theta1:", t2_new , "theta2", t3_new))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      converged = T
      return(paste("theta0:", t1_new, "theta1:", t2_new , "theta2", t3_new))
    }
  }
}



