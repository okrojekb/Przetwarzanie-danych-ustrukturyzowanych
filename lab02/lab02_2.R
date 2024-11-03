set.seed(123)
x <- round(rnorm(20, 0, 1), 2)

#1.1.1 Wypisz na konsole wszystkie wartości ze zbioru [−2, −1] i [1, 2].
print(x[ (x > -2 & x < -1) | (x > 1 & x < 2) ])

#1.1.2 Wypisz na konsolę liczbę oraz frakcję wszystkich wartości nieujemnych.
print(length(x[ x >= 0]))
print(length(x[ x >= 0])/length(x))
# lub
sum(x>=0)
mean(x>=0)

#1.1.3 Wyznacz średnią arytmetyczną wartości bezwzględnych elementów.
print(mean(abs(x)))

#1.1.4 Wyznacz wartość najbliższą i najdalszą od 0 
print(min(abs(x - 0)))
print(max(abs(x - 0)))
# (zachowując jej znak)
print(x[which.max(abs(x - 0))])
print(x[which.min(abs(x - 0))])


#1.1.5 Wyznacz wartość najbliższą i najdalszą od 2 (zachowując jej znak).

print(x[which.max(abs(x - 2))])
print(x[which.min(abs(x - 2))])


#1.1.6 Wypisz na konsolę wektor powstały w wyniku przekształcenia liniowego wartości z x na przedział [0, 1] 
#(najmniejsza wartość staje się równa 0, a największa 1).
x_prim <- x -min(x)
x_bis <- x_prim / (max(x_prim) - min(x_prim))
x_bis
(x - min(x)) / (max(x) - min(x))

# 1.1.7 Utwórz wektor napisów y o długości takiej samej, jaką ma x, dla którego yi przyjmuje wartość
#'nieujemna', jeśli xi jest nieujemne oraz "ujemna" w przeciwnym przypadku.
y <- character(length(x))
y[x>=0] <- "nieujemna"
y[x<0] <- "ujemna"
print(y)
# lub
y <- ifelse(x>=0, "nieujemna", "ujemna")
#lub
c("ujemna", "nieujemna")[(x >= 0) + 1]


#1.1.8 Utwórz wektor napisów y o długości takiej samej, jaką ma x, dla którego  przyjmuje wartość "mały",
#jeśli , "średni", dla  oraz "duży" w przeciwnym przypadku
y <- rep("duzy", length(x))
y[ x < -1] <- "maly"
y[ abs(x) < 1] <- "sredni"
#lub
ifelse ( x < -1, "maly", ifelse ( x > 1, "duzy", "sredni") )

#1.1.9 Utwórz wektor liczbowy y o długości takiej samej, jaką ma x, dla którego yi przyjmuje wartość k + 1/2
#wtedy i tylko wtedy, gdy xi ∈ [k, k + 1), gdzie k ∈ Z (prosty histogram).
y <- floor(x) +0.5
table(y)

#1.2 wartość współczynnika korelacji r Pearsona

r <- (1/(n-1))* sum((((x - mean(x))/sd(x) ) * ((y - mean(y))/sd(y) )))

# a
x <- rnorm(20, 0, 1); y <- 10*x+2
n <- length(x)
# b
x <- rnorm(20, 0, 1); y <- -4*x+1
# c
x <- rnorm(2000, 0, 1); y <- rnorm(2000, 5, 2)

#1.3 Wyznacz wartość próbkowego estymatora współczynnika korelacji rangowej Spearmana

s <- (1 - ((6 * sum((rank(x) - rank(y))**2)) / (n * (n ** 2 -1)) ) )

#1.4 srednią k-winsorowska
k=2
winsor <- function(k, x){
  x <- sort(x)
  mean((x[c(1:k)] <- x[(k+1)]) & (x[c((length(x) - k):length(x))] <- x[(length(x) - k)]))
  
}
winsor(2,1:9)
x <- 1:9
x <- sort(x)
(x[c(1:k)] <- x[(k+1)])
(x[c((length(x) - k):length(x))] <- x[(length(x) - k)])
(length(x) - k)
mean(x)
