library(tidyr)
#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(scales)


#getwd()
setwd("C:/Users/okroj/OneDrive/Studia/semestr 2/przetwarzanie danych ustrukturyzowanych/Projekt 2")
airports <- read.csv(file.path("dataverse_files", "airports.csv"))
runways <- read.csv(file.path("dataverse_files", "runways.csv"))
airports2 <- read.csv(file.path("dataverse_files", "airports (1).csv"))
routes <- read.csv(file.path("dataverse_files", "routes.csv"))
planes <- read.csv(file.path("dataverse_files", "plane-data.csv"))
df87 <- read.csv(file.path("dataverse_files", "1987.csv.bz2"))
df88 <- read.csv(file.path("dataverse_files", "1988.csv.bz2"))
df89 <- read.csv(file.path("dataverse_files", "1989.csv.bz2"))
df90 <- read.csv(file.path("dataverse_files", "1990.csv.bz2"))
df91 <- read.csv(file.path("dataverse_files", "1991.csv.bz2"))
df92 <- read.csv(file.path("dataverse_files", "1992.csv.bz2"))
df93 <- read.csv(file.path("dataverse_files", "1993.csv.bz2"))
df94 <- read.csv(file.path("dataverse_files", "1994.csv.bz2"))
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

b <- rbind(df87, df88, df89, df90, df91, df92, df93, df94, df95, df96, df97, df98, df99, df00, df01, df02, df03, df04, df05, df06, df07, df08)

a <- filter(b, Cancelled == 1)

c <- filter(a, CancellationCode == c("A", "B", "C", "D"))

d <- group_by_at(c , c("CancellationCode"))
d <- summarise(d, IloscOdwolanychLotowZDanegoPowodu = length(CancellationCode))
wektorPowodowOdwolania <- c("carrier", "weather", "NAS", "security")
e <- data.frame(wektorPowodowOdwolania)
rownames(e) <- c("A", "B", "C", "D")


pie(d$IloscOdwolanychLotowZDanegoPowodu, labels = e$wektorPowodowOdwolania, col = myPalette, edges = 50, radius = 1) + 
  title('Wykres: ilość odwołanych lotów z danego powodu', font.main=3)

f <- group_by_at(a, c("UniqueCarrier"))
g <- summarise(f, IloscOdwolanychLotowDanegoPrzewodnika = length(CancellationCode))


ggplot(g, aes(x=UniqueCarrier, y=IloscOdwolanychLotowDanegoPrzewodnika)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Wykres: ilość odwołanych lotów danego przewoźnika')


h <- group_by_at(b, c("UniqueCarrier"))
i <- summarise(h, IloscLotowDanegoPrzewodnika = length(TailNum))

j <- left_join(g, i, by = "UniqueCarrier")

k <- mutate(j, ProcentOdwolanychLotowDanegoPrzewodnika = 100*IloscOdwolanychLotowDanegoPrzewodnika/IloscLotowDanegoPrzewodnika)


ggplot(k, aes(x=UniqueCarrier, y=ProcentOdwolanychLotowDanegoPrzewodnika)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Wykres: ilość odwołanych lotów danego przewoźnika przez ilosc lotow tego przewoznika')

############################################3
# loty tylko z lotnisk USA
bb <- select(airports2, one_of(c("iata_code", "type", "iso_country")))
cc <- left_join(a, bb, by =c("Origin" = "iata_code"))
a <- filter(cc, iso_country == "US")

c <- group_by_at(a, c("Year", "Month", "DayofMonth", "Origin"))
d <- summarise(c, IloscOdwolan = length(TailNum))

e <- select(airports2, one_of(c("iata_code", "type")))
f <- left_join(d, e, by = c("Origin" = "iata_code"))

g <- filter(f, type == "large_airport")
g <- filter(g, Origin != "GUM") # sa dane tylko z 7 lat

h <- group_by_at(g, c("Origin"))
h <- summarise(h, ilosc_large = length(type)) # ilosc odwolan na danym otnisku podczas 87-08

i <- group_by_at(b, c("Origin", "Year"))
i <- summarise(i, IloscLotowDanegoLotniska = length(TailNum))
j <- left_join(i, e, by = c("Origin" = "iata_code"))
k <- filter(j, type == "large_airport")
k <- filter(k, Origin != "GUM")

l <- group_by_at(k, c("Origin"))
l <- summarise(l, SrednioLotowWRoku = round(mean(IloscLotowDanegoLotniska)))
l <- arrange(l, desc(SrednioLotowWRoku))

n <- group_by_at(g, c("Year", "Month", "DayofMonth"))
n <- summarise(n, iloscOdwolanychLotowTegoDnia = sum(IloscOdwolan)) # ilosc odwolanych lotow tego dnia

############ czemu tak duzo odwolan 8.01.1996 - wypadek Type:	McDonnell Douglas DC-9-32; Operator:	Valujet Airlines - 7.01.1996

o <- filter(a, Year == 1996)
cc <- select(planes, one_of(c("tailnum", "model")))

p <- left_join(o,cc, by = c("TailNum" = "tailnum"))
r <- filter(p, model == "DC-9-31") # loty samolotow tego samego modelu co ten z wypadku

s <- group_by_at(r, c("Year", "Month", "DayofMonth"))
t <- summarise(s, iloscOdwolanychLotowTegoDnia = length(TailNum))
u <- unite(t, c("Month", "DayofMonth"), col = "Data", sep = "-")
v <- filter(t, Month == 1)

ggplot(u, aes(x=Data, y=iloscOdwolanychLotowTegoDnia)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Wykres: ilość odwołanych lotów danego dnia w 1996 modelu DC-9-32')


c <- filter(b, Year == 1996)
cc <- select(planes, one_of(c("tailnum", "model")))

d <- left_join(c, cc, by = c("TailNum" = "tailnum"))
e <- filter(d,  Cancelled == 1)

f <- group_by_at(e, c("model", "Month", "DayofMonth"))
f <- summarise(f, IloscOdowolanychLotowDaneModeluTegoDnia = length(Year))
f <- na.omit(f)



g <- group_by_at(d, c("model", "Month", "DayofMonth"))
h <- summarise(g, IloscLotowDanegoModeluDanegoDnia96 = length(TailNum))
h <- na.omit(h)

i <- left_join(f, h, by = c("model", "Month", "DayofMonth"))
j<- mutate(i, ProcentOdwolanychLotowDanegoDNiaTEgoDnia = 100*IloscOdowolanychLotowDaneModeluTegoDnia / IloscLotowDanegoModeluDanegoDnia96)




###########czemu tak duzo odwolan 13.03.1993 - zamiec sniezka stulecia w USA
c <- filter(b, Year == 1993)
cc <- filter(c, Cancelled == 1)
d <- group_by_at(cc, c("Month", "DayofMonth"))
e <- summarise(d, IloscOdwolanychLotowTegoDnia = length(Month))

f <- group_by_at(e, c("Month"))
f <- summarise(f, IloscOdwolanychLotowTegoMiesiaca = sum(IloscOdwolanychLotowTegoDnia))

ggplot(f, aes(x=Month, y=IloscOdwolanychLotowTegoMiesiaca)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Wykres: ilość odwołanych lotów danego miesiaca w 1993') # mnostow odwolan w marcu - dlaczego ? kiedy najwiecej?

h <- filter(e, Month == 3)

ggplot(h, aes(x=DayofMonth, y=IloscOdwolanychLotowTegoDnia)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Wykres: ilość odwołanych lotów danego dnia w 03.1993') # mnostow odwolan 13, 14.03.1993 - gdzie bardziej

i <- group_by_at(cc, c("Month", "DayofMonth", "Origin"))
j <- summarise(i, IloscOdwolanychLotowTegoDniaNaDanymLotnisky = length(Month))

k <- filter(j, Month == 3)
l <- filter(k, DayofMonth == 13 | DayofMonth == 14)



ggplot(l, aes(x=DayofMonth, y=IloscOdwolanychLotowTegoDniaNaDanymLotnisky)) + geom_point()


############# pokazac na mapie jak zamiec wplywa na ilosc opoznien

a <- filter(b, Year == 1993)
a <- filter(a, Month == 3)

c <- select(airports2, one_of(c("type", "iata_code", "latitude_deg", "longitude_deg")))
d <- left_join(a, c, by = c("Origin" = "iata_code"))







########## wybieramy lotniska large; 15 z najwyzsza srednia liczba lotow w roku

Najwieksych15LotniskLarge1 <- slice(l, 1:15)

m <- left_join(Najwieksych15LotniskLarge, g, by = "Origin") # ilosc odwolan danego dnia na danym lotnisku





################################################# czy KAtrina miala wplyw?
c <- filter(b, Year == 2005) 
cc <- filter(c, Cancelled == 1)
d <- group_by_at(cc, c("Month", "DayofMonth", "Origin"))
e <- summarise(d, IloscOdwolanychLotowTegoDniaNaLotnisku = length(Month))

f <- group_by_at(e, c("Month", "Origin"))
f <- summarise(f, IloscOdwolanychLotowTegoMiesiaca = sum(IloscOdwolanychLotowTegoDniaNaLotnisku))

ggplot(f, aes(x=Month, y=IloscOdwolanychLotowTegoMiesiaca)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Wykres: ilość odwołanych lotów danego miesiaca w 2005')

bb <- select(airports2, one_of(c("iata_code", "type", "iso_country")))
cc <- left_join(e, bb, by =c("Origin" = "iata_code"))
dd <- filter(cc, type == "large_airport")


ggplot(dd, aes(x=Month, y=IloscOdwolanychLotowTegoDniaNaLotnisku)) + 
  geom_bar(stat = "identity") + 
  ggtitle('Wykres: ilość odwołanych lotów danego miesiaca w 2005')


ggplot(dd, aes(x=Month, y=IloscOdwolanychLotowTegoDniaNaLotnisku)) + geom_point()


##################### 29,30.01.2005 - ATL - duzo odwolan bo byl GEorgia's winter storm i zamiast 4 pasow dzialaly 1/2