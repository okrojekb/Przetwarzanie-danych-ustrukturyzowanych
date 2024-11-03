distance <- function(x, y)
{
  sqrt( sum( (x-y)^2 ) )
}

x <- c(1,0,3,5)
y <- c(0,3,4,1)

z <- distance(x,y)
z

wector_dist <- function(X, y)
{
  d <- apply(X, 1, distance, y)
  d
}

q <- wector_dist(R,y)
q
# drugi sposob

wect_dis_2 <- function(X,y)
{
  Y <- matrix(y, byrow = TRUE, nrow=nrow(X), ncol= length(y))
  sqrt(rowSums((X-Y)^2))
  X - z
}


