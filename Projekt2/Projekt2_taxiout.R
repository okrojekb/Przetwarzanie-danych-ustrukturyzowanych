#TAXIOUT
install.packages("wesanderson")
library(wesanderson)
library(tidyr)
#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(hrbrthemes)
#install.packages("RColorBrewer")
library(RColorBrewer)


# pusc wczytanie, 40, zrob 89



#getwd()
setwd("C:/Users/okroj/OneDrive/Studia/semestr 2/przetwarzanie danych ustrukturyzowanych/Projekt 2")
airports <- read.csv(file.path("dataverse_files", "airports.csv"))
runways <- read.csv(file.path("dataverse_files", "runways.csv"))
airports2 <- read.csv(file.path("dataverse_files", "airports (1).csv"))
routes <- read.csv(file.path("dataverse_files", "routes.csv"))
planes <- read.csv(file.path("dataverse_files", "plane-data.csv"))

df95 <- read.csv(file.path("dataverse_files", "1995.csv.bz2"))
df96 <- read.csv(file.path("dataverse_files", "1996.csv.bz2"))
df97 <- read.csv(file.path("dataverse_files", "1997.csv.bz2"))
df98 <- read.csv(file.path("dataverse_files", "1998.csv.bz2"))
df99 <- read.csv(file.path("dataverse_files", "1999.csv.bz2"))
df00 <- read.csv(file.path("dataverse_files", "2000.csv.bz2"))
df01 <- read.csv(file.path("dataverse_files", "2001.csv.bz2"))
df02 <- read.csv(file.path("dataverse_files", "2002.csv.bz2"))
df03 <- read.csv(file.path("dataverse_files", "2003.csv.bz2"))
df04 <- read.csv(file.path("dataverse_files", "2004.csv.bz2"))
df05 <- read.csv(file.path("dataverse_files", "2005.csv.bz2"))
df06 <- read.csv(file.path("dataverse_files", "2006.csv.bz2"))
df07 <- read.csv(file.path("dataverse_files", "2007.csv.bz2"))
df08 <- read.csv(file.path("dataverse_files", "2008.csv.bz2"))

#DaneZTaxi <- rbind(df95, df96, df97, df98, df99, df00, df01, df02, df03, df04, df05, df06, df07, df08)

#write.csv(DaneZTaxi, "C:/Users/okroj/OneDrive/Studia/semestr 2/przetwarzanie danych ustrukturyzowanych/Projekt 2/dataverse_files/DaneTaxi.csv", row.names=FALSE)
a <- c(1,2,3)



#a <- read.csv(file.path("dataverse_files", "DaneTaxi.csv"))
DaneZTaxi <- rbind(df95, df96, df97, df98, df99, df00, df01, df02, df03, df04, df05, df06, df07, df08)

######################################################################################################################
#przygotowanie danych z odpowiednimi kolumnami 

j <- select(DaneZTaxi, one_of(c("Year", "Month", "Origin", "TaxiOut")))
j <- na.omit(j)

k <- left_join(j, airports2, by=c("Origin" = "iata_code"))
k <- select(k, one_of(c("Year", "Month", "Origin", "TaxiOut", "id", "ident", "type")))
k <- na.omit(k)
######################################################################################################################

#przygotowanie danych podrupowanych po Year, Month, Origin z srednia Taxi out

a <- select(DaneZTaxi, one_of(c("Year", "Month", "Origin", "TaxiOut")))
a <- na.omit(a)
a <- group_by_at(a, c("Year", "Month", "Origin"))
a <- summarise(a, SredniCzasTaxiOut=mean(TaxiOut))

d <- left_join(a, airports2, by=c("Origin" = "iata_code"))
d <- select(d, one_of(c("Year", "Month", "Origin", "SredniCzasTaxiOut", "id", "ident", "type")))

y <- distinct(runways, airport_ref, .keep_all = TRUE)
e <- left_join(d, y, by = c("id" = "airport_ref"))

f <- select(e, one_of(c("Year", "Month", "Origin", "SredniCzasTaxiOut", "id", "ident", "type", "surface", "lighted")))
g <- na.omit(f)


#wykres miesiac/ taxiout/ kolory - surface

ggplot(h, aes(x=Month, y=SredniCzasTaxiOut, color=surface)) + 
  geom_point(size=1) +
  theme_ipsum()

##wykres surface/ taxiin/ kolory - miesiac

ggplot(h, aes(x=surface, y=SredniCzasTaxiOut, color=Month)) + 
  geom_point(size=0.5) +
  theme_ipsum()

##wykres miesiac/ taxiin/ kolory - rozmiar lotniska

ggplot(h, aes(x=Month, y=SredniCzasTaxiOut, color=type)) + 
  geom_point(size=0.5) +
  theme_ipsum()

# sredni czas TaxiOut w danym rozmiarze lotniska small - 11.6; medium - 9.8; large - 14.1
i <- group_by_at(g, c("type"))
i <- summarise(i, SredniCzasTaxiOut=mean(SredniCzasTaxiOut))

#######################################Policz srednie taxiout z DaneZTaxi po pogrupowaniu wedlug type
# sredni czas TaxiOut - miesiac, rok, typ
l <- group_by_at(k, c("Month", "Year", "type"))
l <- summarise(l, SredniCzasTaxiOut=mean(TaxiOut))

# wykres
ggplot(l, aes(x=Month, y=SredniCzasTaxiOut, color=type)) + 
  geom_point(size=3) +
  theme_ipsum()


## sredni czas TaxiOut w miesiacu
h <- group_by_at(g, c("Month", "Origin"))
h <- summarise(h, SredniCzasTaxiOut=mean(SredniCzasTaxiOut), type, surface)
h <- filter(h, SredniCzasTaxiOut < 200)

# srednie taxiIn w miesiacu # nie wielkie roznice 10-12 min
SrednieTaxiWMiesiacu <- group_by_at(f, c("Month"))
SrednieTaxiWMiesiacu <- summarise(SrednieTaxiWMiesiacu, SredniCzasTaxiOut=mean(SredniCzasTaxiOut))



##################################### ile pasow maja dane lotniska
m <- group_by(runways, by=airport_ref)
m <- summarise(m, IloscPasow=length(id))

n <- left_join(k, m, by= c("id"="by"))
# ile pasow to large, medium a small?
o <- group_by_at(n, c("type"))
p<- summarise(o, minPasow = min(IloscPasow))
q <- summarise(o, maxPasow = max(IloscPasow))
unique(filter(n, type == "large_airport")$IloscPasow)
filter(filter(n, type == "large_airport"), IloscPasow == 1)

# #ODP: Sprawdziłam ile pasów startowych mają dane rozmiary lotnisk:
# Large ->  1-11
# Medium -> 1-5
# Small -> 1-3



################################### ilosc lotow z rozmiarow w roku

r <- group_by_at(k, c("Year", "id"))
r <- summarise(r, iloscPrzelotow = length(id))

s <- group_by_at(r, c("id"))
s <- summarise(s, SredniailoscPrzelotow = ceiling(mean(iloscPrzelotow)))
air2 <- select(airports2, one_of(c("id", "type")))
t <- left_join(s, air2, by="id")

t <- filter(t, id != 3400 & id != 3689 & id != 20890)

o <- group_by_at(t, c("type"))
p<- summarise(o, miniloscprzelotow = (min(SredniailoscPrzelotow)))
q <- summarise(o, maxiloscprzelotow = max(SredniailoscPrzelotow))
qq <- summarise(o, sredniailoscprzelotow = mean(SredniailoscPrzelotow))


sort(unique(filter(t, type == "medium_airport")$SredniailoscPrzelotow))
filter(filter(t, type == "medium_airport"), SredniailoscPrzelotow == 1)

filter(k, id == 3400 | id == 3689 | id == 2)
       
       
       
       
       
       
# ##################################
# donut:
# ilosc lotow/iloscpasow wzgledm ilosc pasow/ kolory - type
m <- group_by(runways, by=airport_ref)
m <- summarise(m, IloscPasow=length(id))

r <- group_by_at(k, c("id"))
r <- summarise(r, iloscPrzelotow = length(id))

n <- inner_join(r, m, by= c("id"="by"))

u <- mutate(n, lotyNaPasy = iloscPrzelotow/IloscPasow)

air2 <- select(airports2, one_of(c("id", "type")))
u <- left_join(u, air2, by="id")

ggplot(u, aes(x=IloscPasow, y=lotyNaPasy, color=type)) + 
  geom_point(size=1) +
  theme_ipsum()

######### wykres osobny dla typow

bb <- filter(u, type == "large_airport")
ggplot(bb, aes(x=IloscPasow, y=lotyNaPasy, color=type)) + 
  geom_point(size=1) +
  theme_ipsum()
bbb <- group_by_at(bb, c("IloscPasow"))
bbb <- summarise(bbb, srednieprzelotynapas = mean(lotyNaPasy))

myPalette <- brewer.pal(8, "Set2") 
mycolors <- c("blue", "red", "cyan", "chocolate", "deepskyblue2", "orange", "cornflowerblue", "yellow" )

pie(bbb$srednieprzelotynapas, labels = bbb$IloscPasow, col = mycolors)



######### ile mamy lotnisk small, medium, large

aa <- group_by_at(u, c("type"))
aa <- summarise(aa, iloscTypu = length(id))



################## 