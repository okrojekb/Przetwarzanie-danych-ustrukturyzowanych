###########czemu tak duzo odwolan 13.03.1993 - zamiec sniezka stulecia w USA


################ dane
c <- filter(b, Year == 1993)
cc <- filter(c, Cancelled == 1)
d <- group_by_at(cc, c("Month", "DayofMonth"))
e <- summarise(d, IloscOdwolanychLotowTegoDnia = length(Month))

################################# ilosc odwolanych lotow w roku 1993 w danych miesiacach #####################################################################


########################################## wykres - ilosc odwolanych lotow 1993 w miesiacach
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
  
  
############# pokazac na mapie jak zamiec wplywa na ilosc odwolan ###############################################

bb <- select(b,c("Cancelled", "Year"))
bb <- na.omit(bb)
c <- group_by_at(bb, c("Year"))
c <- summarise(c, IloscOdwolanychLotow = sum(Cancelled))
ggplot(c, aes(x=Year, y=IloscOdwolanychLotow)) + 
  geom_bar(stat = "identity")
