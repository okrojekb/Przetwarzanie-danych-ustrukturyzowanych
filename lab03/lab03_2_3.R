
# Zestaw zadan 2



#2.3

x <- c(1, 2, 3, 4)
k <- 10000
random <- function(x, k)
{
  ind <- round(runif(min=0.5, max=length(x)+0.49, k))
  x[ind]
}
y <- random(x,k)
y



