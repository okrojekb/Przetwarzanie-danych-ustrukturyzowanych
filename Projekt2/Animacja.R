a <- filter(b, Year == 1993)
a <- filter(a, Month == 3)
a <- filter(a, DayofMonth == 12 | DayofMonth == 13 | DayofMonth == 14)

c <- select(airports2, one_of(c("type", "iata_code", "latitude_deg", "longitude_deg", "iso_country")))
d <- left_join(a, c, by = c("Origin" = "iata_code"))

d <- filter(d, iso_country == "US")

e <- select(d, one_of(c("Year", "Month", "DayofMonth", "Origin", "CRSDepTime", "Cancelled")))

# 
# g <- mutate(e, KtoraGodzina = CRSDepTime%/%100)
# 
# frmt <- "d"   # d --> numbers formatted as integers
# minWidth <- 2 # minimum characters length of each number e.g. 42 --> "00042"
# 
# chMx <- formatC(g$KtoraGodzina, width = minWidth, format = frmt, flag = "0")
# f <- select(g, one_of(c("DayofMonth", "Origin", "CRSDepTime", "Cancelled")))
# f <- cbind(f, chMx)

f <- mutate(e, CzasWylotu = format(strptime(substr(as.POSIXct(sprintf("%04.0f", CRSDepTime), 
                                                              format="%H%M"), 12, 16), '%H:%M'), "%H"))


h <- group_by_at(f, c("Year", "Month", "DayofMonth", "CzasWylotu", "Origin"))
h <- summarise(h, IloscLotowNaDanymLotniskuWDanejGodzinie = length(Origin))
#g1 <- filter(f, Cancelled == 1)
h1 <- group_by_at(f, c("Year", "Month", "DayofMonth", "CzasWylotu", "Origin"))

h1 <- summarise(h1, IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie = sum(Cancelled))
#h2 <- filter(h1, IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie == 0)

# ii <- left_join(h, h1, by = c("Year", "Month", "DayofMonth", "CzasWylotu", "Origin"))
# iii <- left_join(ii, c, by = c("Origin" = "iata_code"))
# iiii <- filter(iii, type == "large_airport")
# jj<- mutate(ii, IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie = coalesce(IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie, 0))
# 
# jj <- mutate(jj, ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku = 100*IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie /  IloscLotowNaDanymLotniskuWDanejGodzinie)
# czasy <- e$CRSDepTime


i <- left_join(h1, h, by = c("Year", "Month", "DayofMonth", "CzasWylotu", "Origin"))


j <- mutate(i, ProcentOdwolanychLotowDanejGodzinyTEgoDniaNaDanymLotnisku = 100*IloscOdwolanychLotowNaDanymLotniskuWDanejGodzinie /  IloscLotowNaDanymLotniskuWDanejGodzinie)
# czasy <- e$CRSDepTime
# dlugosc <- nchar(czasy)
# czasy[dlugosc ==1] <- czasy[dlugosc == 1]*100
# czasy
# dlugosc1 <- nchar(czasy)
# czasy[dlugosc ==2]

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

####################################################################################################

format(strptime(substr(as.POSIXct(sprintf("%04.0f", 0140), 
                                  format="%H%M"), 12, 16), '%H:%M'), "%H")
                                  
                                  






####################
#przykładowa
set.seed(0)         
data <- data.frame(Lon = -119.5, Lat = 49.3, Group = letters[1:10]) %>%
  crossing(Date = seq(as.Date("2020-01-01"), as.Date("2020-01-10"), 1)) %>%
  mutate(Lon = rnorm(n(), Lon, 0.1),
         Lat = rnorm(n(), Lat, 0.1))
data <- data[sample(1:nrow(data), 40),]

ui <- fluidPage(
  sidebarLayout(sidebarPanel(selectInput(inputId = "Var", label = "select", 
                                         choices = letters[1:6], multiple = TRUE, selected = c("a", "b", "c"))),
                mainPanel(sliderInput("animationSlider", "Date:", 
                                      min = min(data$Date), max = max(data$Date), value = min(data$Date), step = 1,
                                      animate = animationOptions(interval = 600, loop = FALSE)),
                          leafletOutput("MapAnimate", width="1100px", height="650px")))) 


server <- function(input, output, session) {
  df <- reactive({
    data %>%
      filter(Group %in% input$Var)
  })
  
  points <- reactive({
    req(input$animationSlider)
    df() %>%
      filter(Date == input$animationSlider)
  })
  
  output$MapAnimate <- renderLeaflet({
    df.in <- df()
    pal <- colorFactor("RdYlBu", df.in$Group)
    
    leaflet(data) %>%
      setView(lng = -119.5, lat = 49.3, zoom = 9) %>%
      addProviderTiles("Esri.WorldImagery", layerId = "basetile") %>%
      addLegend(title = "ID", position = "topleft", pal = pal, values = ~df.in$Group)     
  }) 
  
  observe({
    df.in <- points()
    
    pal <- colorFactor("RdYlBu", df.in$Group)
    
    leafletProxy("MapAnimate", data = points()) %>%
      clearShapes() %>%
      addCircles(lng = ~Lon, lat = ~Lat, fillOpacity = 1, color = ~pal(df.in$Group), popup = ~Group) 
  })
}

shinyApp(ui = ui, server = server)