x <- matrix(1:6,nrow = 2, ncol = 3)
y <- seq(10L, 60L, 10L)
lst <- list(1:5, letters[1:3], max, list(1:6<c(3,4,5),as.list(1:3)))

z1 <- y[y>mean(y)] + x[1,]
z1

# {1,3,5}
# {2,4,6}

z2 <- rowSums(x) + x[,1,drop=FALSE]
z2


z3 <- sapply(list(x,y),lst[[3]])
z3

z4 <- lst[c(1,2)]
z4

z5 <- y[sapply(lst, length)]
z5
