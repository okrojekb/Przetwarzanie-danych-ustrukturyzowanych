softmax <- function(X)
{
  R <- (exp(R))/rowSums(R)
  
}

one_hot_decode <- function(X)
{
  a <- softmax(X)
  apply(a, 1, which.max)
}

g <- one_hot_decode(R)
g
