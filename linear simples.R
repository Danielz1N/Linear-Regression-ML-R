x <- as.matrix(c(4,6,9))
y <- as.matrix(c(5,10,12))
linear_regression <- function(x, y, epochs, learning_rate){
  m_current=0; b_current=0
  N = nrow(y)
  for(i in 1:epochs){
    y_current = (m_current * x) + b_current
    cost = sum((y - y_current)^2) / N
    m_gradient = -(2/N) * sum(crossprod(x,(y - y_current)))
    b_gradient = -(2/N) * sum(y - y_current)
    m_current = m_current - (learning_rate * m_gradient)
    b_current = b_current - (learning_rate * b_gradient)
  }
  return(list(m_current, b_current, cost))
}
linear_regression(x,y,10000,0.01)
model <- lm(y~x)
