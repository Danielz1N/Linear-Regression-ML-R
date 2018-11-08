normalize <- function(x){
  z=matrix(0,nrow(x),ncol(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      if(is.na(x[i,j])){
        x[i,j]=0
      }
    }
  }
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
        z[i,j] = (x[i,j] - mean(x[i,]))/sd(x[i,]) 
    }
  }
  return(z)
}


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

print(grad.descent(X,Y,1000))

