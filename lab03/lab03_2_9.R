
# Zestaw zadan 2



#2.9 approxinvert

approxinvert <- function(f, y, a, b, k=100)
{
  stopifnot(b > a, k > 2) #i reszta warunkow
  
  x <- seq(a, b, len=k)
  fx <- f(x)
  approx_invert_f <- approxfun(fx, x)
  approx_invert_f(y)
  
}

f <- function(x) (x-3)^2
a <- 3
b <- 6
x <- seq(3,6,length.out=10)
y <- f(x)
approxinvert(f,y,a,b)
plot(x,y)
