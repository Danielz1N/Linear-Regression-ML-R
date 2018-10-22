data <- read.csv('../teste.csv')
attach(data)
X <- matrix(c(rep(1,5),size,bedrooms),5)
Y <- as.matrix(price)
# define the gradient function dJ/dtheata: 1/m * (h(x)-y))*x where h(x) = x*theta
# in matrix form this is as follows:
grad <- function(x, y, theta) {
  m <- nrow(y)
  gradient <- (1/m)* (t(x) %*% ((x %*% t(theta)) - y))
  return(t(gradient))
}

# define gradient descent update algorithm
grad.descent <- function(x,y, maxit){
  theta <- matrix(0, nrow=1,ncol=ncol(x)) # Initialize the parameters
  
  alpha = .01 # set learning rate
  for (i in 1:maxit) {
    theta <- theta - alpha  * grad(x, y, theta)   
  }
  return(theta)
}


# results without feature scaling
print(grad.descent(x,1000))


