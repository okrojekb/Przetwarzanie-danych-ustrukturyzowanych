# przgotowanie danych
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(leaflet)

runways <- read.csv(file.path("dataverse_files", "runways.csv"))
airports2 <- read.csv(file.path("dataverse_files", "airports (1).csv"))

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
wszystkieDane <- rbind(df87, df88, df89, df90, df91, df92, df93, df94, df95, df96, df97, df98, df99, df00, df01, df02, df03, df04, df05, df06, df07, df08)

######################## Rokład ilość lotów względem typu lotniska #################################################################################
a <- select(wszystkieDane, one_of(c("Origin", "Year")))
c <- select(airports2, one_of(c("iata_code", "type", "iso_country")))

d <- left_join(a, c, by =c("Origin" = "iata_code"))
d <- filter(d, iso_country == "US")

e <- group_by_at(d, c("Origin", "Year"))
e <- summarise(e, IloscLotow = length(type))

f <- group_by_at(e, c("Origin"))
f <- summarise(f, SredniaIloscLotowWCiaguRoku = round(mean(IloscLotow)))

g <- left_join(f, c, by =c("Origin" = "iata_code"))

h <- group_by_at(g, c("type"))
h <- summarise(h, SredniaIloscLotowWCiaguRokuNaLotniskachTegoTypu = sum(SredniaIloscLotowWCiaguRoku))

h <- filter(h, type != "closed")

myPalette <- brewer.pal(3, "PuRd") 

pie(h$SredniaIloscLotowWCiaguRokuNaLotniskachTegoTypu, col = myPalette, edges = 100000, 
    labels = c("Large airports", "Medium airports", "Small airports"), radius = 1)


######################################### Przepustowość pasów #####################################################################################################
#przygotowanie danych z odpowiednimi kolumnami 
j <- select(wszystkieDane, one_of(c("Year", "Origin")))
j <- na.omit(j)
a <- select(airports2, one_of(c("id", "type", "iata_code")))
k <- left_join(j, a, by=c("Origin" = "iata_code"))
k <- na.omit(k)

# ile pasow maja dane lotniska
m <- group_by(runways, by=airport_ref)
m <- summarise(m, IloscPasow=length(id))

r <- group_by_at(k, c("id", "Year"))
r <- summarise(r, iloscPrzelotowWRoku = length(id))

n <- group_by_at(r, c("id"))
n <- summarise(n, SredniaIloscPrzelotowWRoku = mean(iloscPrzelotowWRoku))
n <- inner_join(n, m, by= c("id"="by"))

u <- mutate(n, lotyNaPasy = SredniaIloscPrzelotowWRoku/IloscPasow)
u <- left_join(u, a, by="id")
u <- filter(u, type != "closed")

ggplot(u, aes(x=IloscPasow, y=lotyNaPasy, col = type)) +   
  geom_point(size = 3) +
  scale_color_manual(values=wes_palette(n=3, name="BottleRocket2")) + 
  theme_dark() +
  geom_rug() + labs(x ="Ilosc pasow startowych na lotnisku", 
                    y = "Ilosc lotow z jedenego pasa startowego w ciagu roku") +
  theme(axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")) +
  theme(legend.position = c(0.83, 0.84)) +
  theme(legend.title = element_text(size=15, face="bold"),
  legend.text = element_text(size=13),
  legend.background = element_rect(fill="grey", size=3, linetype="solid"),
  legend.key.size = unit(1.5, 'cm'))



####################### Jaki procent lotow danego przewoznika jest odwolany? #########################################################################################
a <- filter(wszystkieDane, Cancelled == 1)

f <- group_by_at(a, c("UniqueCarrier"))
g <- summarise(f, IloscOdwolanychLotowDanegoPrzewodnika = length(CancellationCode))

h <- group_by_at(wszystkieDane, c("UniqueCarrier"))
i <- summarise(h, IloscLotowDanegoPrzewodnika = length(TailNum))

j <- left_join(g, i, by = "UniqueCarrier")

k <- mutate(j, ProcentOdwolanychLotowDanegoPrzewodnika = IloscOdwolanychLotowDanegoPrzewodnika/IloscLotowDanegoPrzewodnika)


ggplot(k, aes(x= reorder(UniqueCarrier, -ProcentOdwolanychLotowDanegoPrzewodnika), y=ProcentOdwolanychLotowDanegoPrzewodnika)) + 
  geom_bar(stat = "identity", aes(fill = ProcentOdwolanychLotowDanegoPrzewodnika)) + 
  scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
  labs(x ="Kod przewoźnika", 
       y = "Procent odwołanych lotów") +
  theme(axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"), 
        legend.position='none') +
  scale_y_continuous(labels = percent) +
  theme(axis.text.y = element_text(face="bold", color="black", 
                             size=14))


########################################## Rozkład  powodow odwolan lotow ###############################################################################################
a <- filter(wszystkieDane, Cancelled == 1)

c <- filter(a, CancellationCode == c("A", "B", "C", "D"))

d <- group_by_at(c , c("CancellationCode"))
d <- summarise(d, IloscOdwolanychLotowZDanegoPowodu = length(CancellationCode))
wektorPowodowOdwolania <- c("Problemy operacyjne przewoźnika", "Niekorzystne warunki pogodowe", 
                            "Obostrzenia ruchu i obostrzenia \nwieży kontroli lotów", "Zgrożenie bezpieczenstwa")

e <- data.frame(wektorPowodowOdwolania)
rownames(e) <- c("A", "B", "C", "D")

myPalette <- brewer.pal(4, "PuRd") 
pie(d$IloscOdwolanychLotowZDanegoPowodu, labels = e$wektorPowodowOdwolania, col = myPalette, 100000, radius = 1)


################################# ilosc odwolanych lotow w roku 1993 w danych miesiacach #####################################################################
c <- filter(b, Year == 1993)
cc <- filter(c, Cancelled == 1)
d <- group_by_at(cc, c("Month", "DayofMonth"))
e <- summarise(d, IloscOdwolanychLotowTegoDnia = length(Month))
f <- group_by_at(e, c("Month"))
f <- summarise(f, IloscOdwolanychLotowTegoMiesiaca = sum(IloscOdwolanychLotowTegoDnia))

ggplot(f, aes(x=Month, y=IloscOdwolanychLotowTegoMiesiaca, fill=factor(ifelse(Month== 3,"Highlighted","Normal")))) + 
  geom_bar(stat = "identity") + # mnostow odwolan w marcu - dlaczego ? kiedy najwiecej?
  labs(x = "Miesiąc",
       y = "Ilość odwołanych lotów") +
  theme(axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"), 
        legend.position='none') +
  scale_x_continuous(breaks = seq_along(month.name), labels = month.name) +
  theme(axis.text.x = element_text(hjust = 0.5, size = 14))


################################# ilosc odwolanych lotow w marcu 1993 w danych dniach #####################################################################
h <- filter(e, Month == 3)

ggplot(h, aes(x=DayofMonth, y=IloscOdwolanychLotowTegoDnia)) + 
  geom_bar(stat = "identity", aes(fill = IloscOdwolanychLotowTegoDnia)) + 
  scale_fill_gradient(low = "#00BFC4", high = "#F8766D", na.value = NA) +
  labs(x = "Dzień miesiąca",
       y = "Ilość odwołanych lotów") +
  theme(axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"), 
        legend.position='none') +
  scale_x_continuous(breaks = c(1, 5, 10, 13, 15, 20, 25, 30)) +
  theme(axis.text.x = element_text(hjust = 0.5, size = 14))+
  geom_rug()

############################## ramka danych do animacji w shiny #############################################################################
a <- filter(b, Year == 1993)
a <- filter(a, Month == 3)
a <- filter(a, DayofMonth == 12 | DayofMonth == 13 | DayofMonth == 14)

c <- select(airports2, one_of(c("type", "iata_code", "latitude_deg", "longitude_deg", "iso_country")))
d <- left_join(a, c, by = c("Origin" = "iata_code"))

d <- filter(d, iso_country == "US")

e <- select(d, one_of(c("Year", "Month", "DayofMonth", "Origin", "CRSDepTime", "Cancelled")))

f <- mutate(e, CzasWylotu = format(strptime(substr(as.POSIXct(sprintf("%04.0f", CRSDepTime), 
                                                              format="%H%M"), 12, 16), '%H:%M'), "%H"))
h <- group_by_at(f, c("Year", "Month", "DayofMonth", "CzasWylotu", "Origin"))
h <- summarise(h, IloscLotowNaDanymLotniskuWDanejGodzinie = length(Origin))
h1 <- group_by_at(f, c("Year", "Month", "DayofMonth", "CzasWylotu", "Origin"))

h1 <- summarise(h1, IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie = sum(Cancelled))

i <- left_join(h1, h, by = c("Year", "Month", "DayofMonth", "CzasWylotu", "Origin"))

j <- mutate(i, ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku = 100*IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie /  IloscLotowNaDanymLotniskuWDanejGodzinie)

k <- unite(j, col = data, c("Year", "Month", "DayofMonth"), sep = "-")
k <- mutate(k, Date = as.Date(data, "%Y-%m-%d"), .keep = "unused")

kk <- unite(k, col = czas, c("Date", "CzasWylotu"), sep = " ")
kk <- mutate(kk, Czas = as.POSIXct(czas, format="%Y-%m-%d%H"), .keep = "unused")

l <- left_join(kk, c, by = c("Origin" = "iata_code"))
data <- as.POSIXct("1993-03-12 13", format="%Y-%m-%d%H")
data2 <- as.POSIXct("1993-03-14 10", format="%Y-%m-%d%H")

l <- filter(l, Czas >= data)
l <- filter(l, Czas <= data2)

l <- filter(l, IloscLotowNaDanymLotniskuWDanejGodzinie >1)
m <- filter(l, ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku == 0)

l <- filter(l, ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku >0)
m <- l

write.csv(l, "C:/Users/okroj/OneDrive/Studia/semestr 2/przetwarzanie danych ustrukturyzowanych/Projekt 2/Animacja - odwołania w 1993/ramkaDanych.csv", row.names=FALSE)
