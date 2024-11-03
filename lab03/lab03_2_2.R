
# Zestaw zadan 2



#2.2 calkowanie Monte Carlo

calkaMonteCarlo <- function(f, a, b, n=1000)
{
  stopifnot(is.function(f))
  stopifnot(is.numeric(a), is.numeric(b), 
            length(a) ==1, length(b) == 1, a < b)
  fa <- f(a)
  fb <- f(b)
  stopifnot(fa >= 0, fb >= 0)
  
  fmin <- min(fa, fb)
  fmax <- max(fa, fb)
  
  randx <- runif(n, min = a, max = b)
  randy <- runif(n, min = fmin, max = fmax)
  
  
  mean(randy <= f(randx)) * (b-a) * (fmax - fmin) + (b - a) * fmin
  
}

set.seed(123)
# przyklad 1
f <- function(x) sin(x)
a <- 0
b <- 1
n <- 10000
calkaMonteCarlo(f,a,b,n)
# przyklad 2

f <- function(x) x + 1
calkaMonteCarlo(f,a,b,n)
# przyklad 3

f <- function(x) x^2 + 2
calkaMonteCarlo(f,a,b,n)


integrate(f, a, b)
