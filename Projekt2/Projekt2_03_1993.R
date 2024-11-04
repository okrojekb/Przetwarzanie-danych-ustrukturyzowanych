library(tidyr)
#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
#install.packages("leaflet")
library(leaflet)
#getwd()
#install.packages("gganimate")
#library(gganimate)
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

m <- leaflet()
# Then we Add default OpenStreetMap map tiles
m <- addTiles(m)
m

############# pokazac na mapie jak zamiec wplywa na ilosc opoznien

a <- filter(b, Year == 1993)
a <- filter(a, Month == 3)

c <- select(airports2, one_of(c("type", "iata_code", "latitude_deg", "longitude_deg", "iso_country")))
d <- left_join(a, c, by = c("Origin" = "iata_code"))



e <- group_by_at(a, c("DayofMonth", "Origin"))
f <- summarise(e, IloscLotowNaDanymLotniskuDanegoDnia = length(Origin))
g <- filter(d, Cancelled == 1)
g <- group_by_at(g, c("DayofMonth", "Origin"))
g <- summarise(g, IloscOdwolanychLotowNaDanymLotniskuDanegoDnia = length(Origin))

h <- left_join(f, g, by =c("DayofMonth", "Origin"))
h <- na.omit(h)
h <- mutate(h, ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku = 100*IloscOdwolanychLotowNaDanymLotniskuDanegoDnia /  IloscLotowNaDanymLotniskuDanegoDnia)

i <- left_join(h, c, by = c("Origin" = "iata_code"))

Dzien13 <- filter(i, DayofMonth == 13)
Dzien13BezSmallICLosed <- filter(Dzien13, type == "large_airport" | type == "medium_airport")
Dzien13BezSmallICLosedUSA <- filter(Dzien13BezSmallICLosed, iso_country == "US")
piersze10 <- slice(Dzien13BezSmallICLosedUSA, 1:100)

p <- mutate(Dzien13, popup_info = paste("Kod IATA lotniska: ", Origin, "<br/>", 
                                          "Ilość lotów z tego lotniska: ", IloscLotowNaDanymLotniskuDanegoDnia, "<br/>",
                                          "Ilość odwołanych lotów: ", IloscOdwolanychLotowNaDanymLotniskuDanegoDnia, "<br/>",
                                          "Procent odwołanych lotów: ", ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku, "<br/>",
                                          "Rozmiar lotniska", type, "<br/>",
                                          "Lattitude:", latitude_deg, "<br/>", "Longitude:", longitude_deg) )

colors <- c("green", "blue", "red", "yellow")
pal <- colorFactor(colors, p$ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku)

pal1 <- colorNumeric(colors, p$ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku)

leaflet() %>% 
  addTiles()  %>% 
  setView( lat=35, lng=-95 , zoom=4) %>%
  addCircleMarkers(data = p, lat = ~latitude_deg, lng = ~longitude_deg, radius = ~3, opacity = 5,
                   popup = ~popup_info, color = ~pal1(ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku)) %>%
  addLegend( pal=pal1, values=p$ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku, title = "Procent odwolanych lotow 13.03.1993", position = "bottomright" )

####################################### 14.03.1993
a <- filter(b, Year == 1993)
a <- filter(a, Month == 3)

c <- select(airports2, one_of(c("type", "iata_code", "latitude_deg", "longitude_deg", "iso_country")))
d <- left_join(a, c, by = c("Origin" = "iata_code"))



e <- group_by_at(a, c("DayofMonth", "Origin"))
f <- summarise(e, IloscLotowNaDanymLotniskuDanegoDnia = length(Origin))
g <- filter(d, Cancelled == 1)
g <- group_by_at(g, c("DayofMonth", "Origin"))
g <- summarise(g, IloscOdwolanychLotowNaDanymLotniskuDanegoDnia = length(Origin))

h <- left_join(f, g, by =c("DayofMonth", "Origin"))
h <- na.omit(h)
h <- mutate(h, ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku = 100*IloscOdwolanychLotowNaDanymLotniskuDanegoDnia /  IloscLotowNaDanymLotniskuDanegoDnia)

i <- left_join(h, c, by = c("Origin" = "iata_code"))

Dzien13 <- filter(i, DayofMonth == 14)
Dzien13BezSmallICLosed <- filter(Dzien13, type == "large_airport" | type == "medium_airport")
Dzien13BezSmallICLosedUSA <- filter(Dzien13BezSmallICLosed, iso_country == "US")
piersze10 <- slice(Dzien13BezSmallICLosedUSA, 1:100)

p <- mutate(Dzien13, popup_info = paste("Kod IATA lotniska: ", Origin, "<br/>", 
                                        "Ilość lotów z tego lotniska: ", IloscLotowNaDanymLotniskuDanegoDnia, "<br/>",
                                        "Ilość odwołanych lotów: ", IloscOdwolanychLotowNaDanymLotniskuDanegoDnia, "<br/>",
                                        "Procent odwołanych lotów: ", ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku, "<br/>",
                                        "Rozmiar lotniska", type, "<br/>",
                                        "Lattitude:", latitude_deg, "<br/>", "Longitude:", longitude_deg) )

colors <- c("green", "blue", "red", "yellow")
pal <- colorFactor(colors, p$ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku)

pal1 <- colorNumeric(colors, p$ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku)

leaflet() %>% 
  addTiles()  %>% 
  setView( lat=35, lng=-95 , zoom=4) %>%
  addCircleMarkers(data = p, lat = ~latitude_deg, lng = ~longitude_deg, radius = ~3, opacity = 5,
                   popup = ~popup_info, color = ~pal1(ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku)) %>%
  addLegend( pal=pal1, values=p$ProcentOdwolanychLotowDanegoDNiaTEgoDniaNaDanymLotnisku, title = "Procent odwolanych lotow 14.03.1993", position = "bottomright" )


######################################################## animacja
a <- filter(b, Year == 1993)
a <- filter(a, Month == 3)
a <- filter(a, DayofMonth == 12 | DayofMonth == 13 | DayofMonth == 14)

c <- select(airports2, one_of(c("type", "iata_code", "latitude_deg", "longitude_deg", "iso_country")))
d <- left_join(a, c, by = c("Origin" = "iata_code"))

d <- filter(d, iso_country == "US")

e <- select(d, one_of(c("DayofMonth", "Origin", "CRSDepTime", "Cancelled")))


g <- mutate(e, KtoraGodzina = CRSDepTime%/%100)

frmt <- "d"   # d --> numbers formatted as integers
minWidth <- 2 # minimum characters length of each number e.g. 42 --> "00042"

chMx <- formatC(g$KtoraGodzina, width = minWidth, format = frmt, flag = "0")
f <- select(g, one_of(c("DayofMonth", "Origin", "CRSDepTime", "Cancelled")))
f <- cbind(f, chMx)


h <- group_by_at(f, c("DayofMonth", "chMx", "Origin"))
h <- summarise(h, IloscLotowNaDanymLotniskuWDanejGodzinie = length(Origin))
g1 <- filter(f, Cancelled == 1)
h1 <- group_by_at(g1, c("DayofMonth", "chMx", "Origin"))

h1 <- summarise(h1, IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie = length(Origin))

i <- left_join(h1, h, by = c("DayofMonth", "chMx", "Origin"))

j <- mutate(i, ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku = 100*IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie /  IloscLotowNaDanymLotniskuWDanejGodzinie)
# czasy <- e$CRSDepTime
# dlugosc <- nchar(czasy)
# czasy[dlugosc ==1] <- czasy[dlugosc == 1]*100
# czasy
# dlugosc1 <- nchar(czasy)
# czasy[dlugosc ==2]
k <- unite(j, col = data, c("DayofMonth", "chMx"))

l <- left_join(k, c, by = c("Origin" = "iata_code"))
data <- as.POSIXct("1993-03-12 13", format="%Y-%m-%d%H")
l <- filter(l, Czas >= data)



transition_states(l, transition_length = 1, state_length = 1, wrap = TRUE)


colors <- c("green", "blue", "red", "yellow")
pal <- colorFactor(colors, l$ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku)

pal1 <- colorNumeric(colors, l$ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku)

leaflet() %>% 
  addTiles()  %>% 
  setView( lat=35, lng=-95 , zoom=4) %>%
  addCircleMarkers(data = l, lat = ~latitude_deg, lng = ~longitude_deg, radius = ~3, opacity = 5,
                   color = ~pal1(ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku)) %>%
  addLegend( pal=pal1, values=l$ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku, title = "Procent odwolanych lotow 14.03.1993", position = "bottomright" ) +
  transition_time(data) +
  ease_aes('linear')
  
