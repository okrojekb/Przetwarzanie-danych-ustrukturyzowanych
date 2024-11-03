
# Zestaw zadan 2



#2.5

x <- sample(5, 10, TRUE)
x
y <- sample(10,15, TRUE)
y
intersect(x,y)

tabulate(x)
tabulate(y)

iloczyn <- function(x,y)
{
  len <- min(max(x), max(y))
  #num_x <- tabulate(x, len)
  #num_y <- tabulate(y, len)
  which(tabulate(x, len) & tabulate(y, len))

}

iloczyn(x,y)
