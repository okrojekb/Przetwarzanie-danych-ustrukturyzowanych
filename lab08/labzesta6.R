getwd()
setwd("H:/Documents/pdu_laby")
list.files(".", full.names = FALSE, recursive = TRUE)
file.info("csv-examples//plik_01.csv")

#normalisepath()

#6.1
size <- function(dirpath){
  stopifnot(dir.exists(dirpath))
  files <- list.files(dirpath, full.names = FALSE,  recursive = TRUE)
  sum(file.size(files)) / 10^6
}

size(".")


#6.2
oldest <- function(dirpath, k=10){
  files <- list.files(dirpath, full.names = TRUE)
  x <- file.info(files)
  y <- order(x$mtime)
  a <- y[1:k]
  res <- x[which_files]
  res[order(a$size[which_files])]
}

file.info("labzesta6.R")[,"mtime"]

oldest(".", 2)

#6.5

read.csv("csv-examples\\plik_01.csv")

df1 <- read.csv(file.path("csv-examples", "plik_01.csv"))
df1

df2 <- read.csv(file.path("csv-examples", "plik_02.csv"), comment.char = "#")
df2

df2b <- read.csv(file.path("csv-examples", "plik_02.csv"), skip = 1)
df2b

df3 <- read.csv(file.path("csv-examples", "plik_03.csv"), sep = "\t")
df3

df3b <- read.table(file.path("csv-examples", "plik_03.csv"), header = TRUE)
df3b

df4 <- read.csv(file.path("csv-examples", "plik_04.csv"), sep = ";", dec = ".")
df4

df5 <- read.csv(file.path("csv-examples", "plik_05.csv"), sep = ";", dec = ",")
df5

df5b <- read.csv2(file.path("csv-examples", "plik_05.csv"))
df5b


#6.6

install.packages("XML")
library(XML)

ex1 <- file.path("other-examples", "example_1.xml")
ex1

#parsowanie
doc <- XML::xmlParse(ex1)
doc

#do listy
lista <- XML::xmlToList(doc)
lista

#do ramki danych
data.frame(do.call('rbind', lista))

XML::xmlToDataFrame(doc)



ex2 <- file.path("other-examples", "example_2.xml")

doc2 <- XML::xmlParse(ex2)

lista2 <- XML::xmlToList(doc2)

data.frame(do.call('rbind', lista2))

install.packages("readxl")
library(readxl)

readxl::read_xls( file.path("other-examples", "excel-01.xls"))









