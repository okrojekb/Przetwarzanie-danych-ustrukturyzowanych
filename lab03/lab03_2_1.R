
# Zestaw zadan 2



#2.1

x <- rep(c(1,2,2,3,3,3), each=3)
rle(x)

mode <- function(x)
{
  rle(x)$values[which.max(rle(x)$lengths)]
}


mode(x)
