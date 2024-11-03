library(nycflights13)
library(stringi)
install.packages('nasaweather')
library(nasaweather)
weather <- as.data.frame(weather)

#5.1
fun1 <- function(n, m){
  n <- min(n,26)
  as.character(outer(letters[1:n], 1:m, FUN = paste0))
}

fun1(10,10)

#5.2

#sposob 1

paste(weather$year, 
      ifelse(weather$month<10, 
             paste("0", weather$month, sep=""), weather$month), 
      ifelse(weather$day <10,
             paste("0",weather$day, sep=""), weather$day), 
      sep="-")

#sposob 2

sprintf("%d-%02d-%02d", weather$year,weather$month, weather$day)

#
N <- 1000
for(i in 1:N){
  cat(sprintf('\r%5.1f%%', i/N * 100))
  Sys.sleep(0.1)
}
#


#sposob 3
strptime("2023-4-12", "%Y-%m-%d")
strftime()

strptime(
  paste(weather$year,weather$month, weather$day, sep="-"), 
  "%Y-%m-%d")
#sposob 4

stringi::stri_datetime_create(weather$year,weather$month, weather$day)


#5.3

#sposob 1
x1 <- substr(glaciers$id, 1,2)
x2 <- substr(glaciers$id, 3,3)
x3 <- substr(glaciers$id, 4,7)
x4 <- substr(glaciers$id, 8,9)
x5 <- substr(glaciers$id, 10, 12)

#sposob 2

x <- unlist(stri_match_all_regex(glaciers$id, "(.{2})(.{1})(.{4})(.{2})(.{2,3})"))
data.frame(matrix(x, ncol=6, byrow=TRUE))



#5.5

fun5 <- function(text, pattern, replacement){
  stri_replace_all_regex(text, pattern = pattern, replacement = replacement)
}

fun5("Ala ma kota.\nKasia ma psa.\nJulia ma zabe.",
     '.\n', ';')

#5.12

fun12 <- function(text){
  stri_replace_all_regex(text, "\\s+", " ")
}

napis <- "i;aghhw\nskhf \n  \t\t\t   lk;a'; \nwajo;a d"

fun12(napis)

#5.13

test <- "12.123, -53, +1e-9, -1.2423e10, 4. oraz .2"

pattern <- "[+-]?((\\.\\d+)|((\\d+)(\\.\\d*)?(e[+-]\\d+)?))"
stri_match_all_regex(test, pattern)

# [+-]? - znak + lub - wystepujacy 0 lub 1 raz
# (\\.\\d+) -- liczby zaczynajace sie od kropki np. .2 lub .987666
# (\\d+)(\\.\\d*)? - liczby skladajace sie z samych cyfr lub z cyfr kropki lub cyfr kropki i cyfr np. 12 lub 30. lub 34.45665
# (e[+-]//d+)? - notacja wykladnicza z e np. e+10 lub e-9
#laczymy to w calosc uzywajac lub | 

