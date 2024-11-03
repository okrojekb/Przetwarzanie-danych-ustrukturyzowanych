one_hot_encode <- function(t)
{
  R <- matrix(data = 0, nrow = length(t), ncol = max(t), byrow = TRUE)
  a <- 1:length(t)
  b <- cbind(a, t)
  R[b] <- 1
  R
    
} 

x <- sample(1:8,7)
y <- one_hot_encode(x) 
y

