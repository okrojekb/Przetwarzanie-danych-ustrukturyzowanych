### Przetwarzanie Danych Ustrukturyzowanych 2023L
### Praca domowa nr. 3
###
### UWAGA:
### nazwy funkcji oraz ich parametrow powinny pozostac niezmienione.
###  
### Wskazane fragmenty kodu przed wyslaniem rozwiazania powinny zostac 
### zakomentowane
###

# -----------------------------------------------------------------------------#
# Wczytanie danych oraz pakietow.
# !!! Przed wyslaniem zakomentuj ten fragment

# library("sqldf")
# library("dplyr")
# library('data.table')
# Posts <- read.csv("C:\\Users\\okroj\\OneDrive\\Studia\\semestr 2\\przetwarzanie danych ustrukturyzowanych\\Projekt 1\\Posts.csv.gz")
# head(Posts, 5)
# 
# Comments <- read.csv("C:\\Users\\okroj\\OneDrive\\Studia\\semestr 2\\przetwarzanie danych ustrukturyzowanych\\Projekt 1\\Comments.csv.gz")
# head(Posts, 5)
# 
# Users <- read.csv("C:\\Users\\okroj\\OneDrive\\Studia\\semestr 2\\przetwarzanie danych ustrukturyzowanych\\Projekt 1\\Users.csv.gz")
# head(Users)
# 
# library("microbenchmark")
# -----------------------------------------------------------------------------#


# -----------------------------------------------------------------------------#
# Zadanie 1
# -----------------------------------------------------------------------------#



sql_1 <- function(Users){
    # Tu umiesc rozwiazanie oraz komentarze
    sqldf("SELECT Location, SUM(UpVotes) as TotalUpVotes   
    FROM Users
    WHERE Location != ''
    GROUP BY Location
    ORDER BY TotalUpVotes DESC
    LIMIT 10")
  
    #Uzywamy danych z "Users"
    #wybieramy kolumne "Location" i tworzymy kolumne z suma glosow o nazwie "TotalUpVotes".
    #wybieramy tylko te wiersze, w ktorych lokolizacje jest rozna "".
    #grupujemy dane po lokalizacji
    #wiersze ustawiamy malejaco wedlug "TotalUpVotes"
    #wyswietlemy tylko pierszych 10 wierszy
}



base_1 <- function(Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
    x <- Users[Users$Location != "", ]
    #wybieramy tylko te wiersze, w ktorych lokolizacje jest rozna ""
    
    x<- aggregate(x$UpVotes, by = x["Location"], FUN = sum)
    #grupujemy dane po lokalizacji, wyswietamy kolumne "Location" i tworzymy kolumne z suma glosow w kazdej grupie
    
    colnames(x)[2] = "TotalUpVotes"
    #zmieniamy nazwe kolumny z suma glosow na "TotalUpVotes"
    
    x <- x[order(x$TotalUpVotes, decreasing = TRUE),]
    #wiersze ustawiamy malejaco wedlug "TotalUpVotes"
    
    rownames(x) <- NULL
    #zmieniamy nazwy wierszy, tak aby byly one ponumerowane wedlug ich obecnej kolejnosci
    
    x <- head(x, 10)
    #wyswietlemy tylko pierszych 10 wierszy
    # 
}



dplyr_1 <- function(Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
    x <- dplyr::filter(Users, Location != "")
    #wybieramy tylko te wiersze, w ktorych lokolizacje jest rozna ""
    
    x <- dplyr::group_by(x, Location)
    #grupujemy dane po lokalizacji
  
    x <- dplyr::summarise(x, TotalUpVotes = sum(UpVotes))
    #wyswietamy kolumne "Location" i tworzymy kolumne z suma glosow w kazdej grupie o nazwie "TotalUpVotes"

    x <- dplyr::arrange(x,desc(TotalUpVotes))
    #wiersze ustawiamy malejaco wedlug "TotalUpVotes"
    
    c <- dplyr::slice(x,1:10)
    #wyswietlemy tylko pierszych 10 wierszy
    # 
}



table_1 <- function(Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
    x <- data.table(Users)
    #zmiana typu Users na typ: data.table

    x <- x[Location!=""]
    #wybieramy tylko te wiersze, w ktorych lokolizacje jest rozna ""
    
    x <- x[,.(TotalUpVotes = sum(UpVotes)), by=Location]
    #grupujemy dane po lokalizacji, wyswietamy kolumne "Location" i tworzymy kolumne z suma glosow w kazdej grupie o nazwie TotalUpVotes
  
    data.table::setorderv(x, cols="TotalUpVotes", -1)
    #wiersze ustawiamy malejaco wedlug "TotalUpVotes"
    
    x <- x[1:10,]
    #wyswietlemy tylko pierszych 10 wierszy
    # 
}



# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# df1 <- sql_1(Users)
# 
# x1 <- base_1(Users)
# 
# dplyr::all_equal(df1,x1) #TRUE
# 
# y1 <- dplyr_1(Users)
# 
# dplyr::all_equal(df1,y1) #TRUE
# 
# z1 <- table_1(Users)
# 
# dplyr::all_equal(df1,z1) #TRUE



# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark::microbenchmark(
#   sqldf = sql_1(Users),
#   base = base_1(Users),
#   dplyr = dplyr_1(Users),
#   data.table = table_1(Users)
# )

#wynik:
#Unit: milliseconds
#expr         min          lq        mean        median        uq       max      neval
#sqldf       227.4588  232.91190   250.91394   237.59870   250.39105   404.8919   100
#base        147.8727  154.62105   176.27760   161.99915   190.65380   329.3544   100
#dplyr       33.2421   36.71585    48.29985    38.72705    44.11220    298.6800   100
#data.table  21.0531   24.88985    40.07948    27.86850    33.82675    122.2144   100



# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#



sql_2 <- function(Posts){
    # Tu umiesc rozwiazanie oraz komentarze
  
    sqldf("SELECT STRFTIME('%Y', CreationDate) AS Year, STRFTIME('%m', CreationDate) AS Month,
    COUNT(*) AS PostsNumber, MAX(Score) AS MaxScore
    FROM Posts
    WHERE PostTypeId IN (1, 2)
    GROUP BY Year, Month
    HAVING PostsNumber > 1000
    ")
  
    #Uzywamy danych z "Posts"
    #Tworzymy kolumne o nazwie "Year", która zawiera rok z kolumny "CreationDate"
    #Tworzymy kolumne o nazwie "Month", która zawiera miesiac z kolumny "CreationDate"
    #Tworzymy kolumne o nazwie "PostsNumber", która zawiera ilosc postow w grupie
    #Tworzymy kolumne o nazwie "MaxScore", która zawiera maksymalna wartosc z kolumny "Score" w grupie
    #Wybieramy tylko te wiersze, w ktorych PostTypeId jest rowne 1 lub 2
    #grupujemy dane po wartosciach kolumn "Year" i "Month"
    #Wybieramy tylko te wiersze, ktore w kolumnie PostsNumber maja wartosci wieksza niz 1000
    # 
}



base_2 <- function(Posts){
    # Tu umiesc rozwiazanie oraz komentarze
    
    x <- Posts[Posts$PostTypeId==1 | Posts$PostTypeId==2, ]
    #Wybieramy tylko te wiersze, w ktorych PostTypeId jest rowne 1 lub 2
    
    y <- as.Date(x$CreationDate)
    # tworzymy wektor z kolumny "CreationDate" w formacie date
    
    Year <- format(y, format = "%Y")
    # tworzymy wektor z wektora y zawierajacy rok
    
    Month <- format(y, format = "%m")
    # tworzymy wektor z wektora y zawierajacy miesiac
    
    x <- cbind(x, Year)
    #dodajemy wektor z latami jako kolumne
    
    x <- cbind(x, Month)
    #dodajemy wektor z miesiacami jako kolumne
    
    x <- x[,c("Year","Month","Score")]
    #wybieramy tylko kolumny "Year", "Month" i "Score"
    
    fun2 <- function(w){
      a <- length(w)
      b <- max(w)
      c <- c(a,b)
      c
    } # funkcja, ktora przyjmuje wektor i zwraca wektor 2-elemntowy, gdzie na pierszym miejscu 
    #jest dlugosc wektora, a na drugim wartosc maksymalna
    
    x <- aggregate(x$Score, by = x[c("Year", "Month")], FUN=fun2)
    #grupujemy dane po wartosciach kolumn "Year" i "Month" i tworzymy nowa kolumne, zawierajaca wartosci funckcji fun2
    
    PostsNumber <- x$x[,1]
    # z kolumny z wartosciami zwroconymi przez fun2 tworzymy wektor zawierajacy pierwszy element 
    # wektora w kazdym wierszu(zawierajacy ilosc postow z tej grupie)
    
    MaxScore <- x$x[,2]
    # z kolumny z wartosciami zwroconymi przez fun2 tworzymy wektor zawierajacy drugi element 
    # wektora w kazdym wierszu(zawierajacy maksymalna wartosc z kolumny "Scores" w tej grupie)
    
    x <- cbind(x, PostsNumber)
    #dodajemy wektor z iloscia postow jako kolumne
    
    x <- cbind(x, MaxScore)
    #dodajemy wektor z maksymalna wartoscia z kolumny "Scores" jako kolumne
    
    x <- x[,c("Year","Month","PostsNumber","MaxScore")]
    #wybieramy tylko kolumny "Year", "Month" "PostsNumber" i "MaxScore"
    
    x <- x[x$PostsNumber > 1000, ]
    #Wybieramy tylko te wiersze, ktore w kolumnie PostsNumber maja wartosci wieksza niz 1000
    
    rownames(x) <- NULL
    #zmieniamy nazwy wierszy, tak aby byly one ponumerowane wedlug ich obecnej kolejnosci
    x
    #
}


dplyr_2 <- function(Posts){
    # Tu umiesc rozwiazanie oraz komentarze
  
    x <- dplyr::filter(Posts, PostTypeId ==1 | PostTypeId ==2)
    #Wybieramy tylko te wiersze, w ktorych PostTypeId jest rowne 1 lub 2
    
    x <- tidyr::separate(x, CreationDate, c("Year", "Month", "Rest"), sep="-")
    #rozdzielamy kolumne "CreationDate" na kolumny: "Year" - zawierajaca rok - i "Month" - zawierajaca miesiac - oraz "Rest" - zawierajaca reszte daty
    
    y <- tidyr::unite(x[, c("Year","Month")], Date, sep=";")
    #tworzymy kolumne o nazwie "Date", ktora laczy kolumny "Year" i Month"

    x <- dplyr::select(x, Score)
    #wybieramy tylko kolumne "Score"
    
    x <- dplyr::bind_cols(y,x)
    #laczymy kolumne "Date" i "Score" w jedna data.frame
    
    y <- dplyr::count(x, Date)
    #zliczamy ilosc wierszy, ktore maja dana wartosc w kolumnie Date
    
    x <- dplyr::group_by(x, Date)
    #grupujemy po kolumnie "Date"
    
    x <- dplyr::summarise(x, MaxScore = max(Score))
    #wyswietamy kolumne "Date" i tworzymy kolumne z maksymalna wartoscia z kolumny "Score"
    #w kazdej grupie o nazwie "MaxScore"
    
    x <- dplyr::bind_cols(x, "PostsNumber"=y$n)
    #laczymy ramke x z kolumna z ramki y
    
    x <- tidyr::separate(x, col="Date", into=c("Year", "Month"), sep=";")
    #rozdzielamy kolumne "Date" na kolumny "Year" i "Month"
    
    x <- dplyr::filter(x, PostsNumber>1000)
    #Wybieramy tylko te wiersze, ktore w kolumnie PostsNumber maja wartosci wieksza niz 1000
    
    rownames(x) <- NULL
    #zmieniamy nazwy wierszy, tak aby byly one ponumerowane wedlug ich obecnej kolejnosci
    x
    # 
}



table_2 <- function(Posts){
    # Tu umiesc rozwiazanie oraz komentarze
    
    x <- data.table(Posts)
    #zmiana typu Posts na typ: data.table
    
    x <- x[PostTypeId %in% c(1,2)]
    #wybieramy tylko te wiersze, w ktorych wartosc w kolumnie "PostTypeId" jest rowna 1 lub 2
    
    setDT(x)[,c("Year", "Month", "Rest") := tstrsplit(CreationDate, "-", type.convert = FALSE)]
    #tworzymy trzy nowe kolumny o nazwach: "Year", "Month" i "Rest", gdzie znajduja sie wartosci
    #z kolumny CreationDate
    
    y <- x[, .(MaxScore=max(Score)), by = .(Year, Month)]
    #Zostawiamy kolumny "Year" i "Month" i wedlug nich grupujemy dane i tworzymy kolumne 
    #"MaxScore", w ktorej jest najwyzsza wartosc z kolumny "Score" w danej grupie
    
    x <- x[,.N,by = .(Year, Month)]
    #Zostawiamy kolumny "Year" i "Month" i wedlug nich grupujemy dane i tworzymy kolumne 
    #"N", w ktorej jest ilosc wierszy z danej grupy
    
    y[, PostsNumber := x$N]
    #do y dodajemy kolumne "PostsNumber", do ktorej przypisujemy wartosci z kolumny "N" z x
    
    x <- y[PostsNumber > 1000]
    #Wybieramy tylko te wiersze, ktore w kolumnie PostsNumber maja wartosci wieksza niz 1000
    # 
}




# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
# 
# df2 <- sql_2(Posts)
# 
# x2 <- base_2(Posts)
# 
# dplyr::all_equal(df2,x2) #TRUE
# 
# y2 <- dplyr_2(Posts)
# 
# dplyr::all_equal(df2,y2) #TRUE
# 
# z2 <- table_2(Posts)
# 
# dplyr::all_equal(df2,z2) #TRUE


# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem

# microbenchmark::microbenchmark(
#   sqldf = sql_2(Posts),
#   base = base_2(Posts),
#   dplyr = dplyr_2(Posts),
#   data.table = table_2(Posts)
# )

#wyniki
# Unit: milliseconds
# expr            min         lq        mean        median        uq         max     neval
# sqldf        1481.4247   1638.989   2246.0390   2191.3392   2531.3774   4629.7009   100
# base         1531.7686   1659.868   2153.2488   2116.3252   2459.4221   4537.2006   100
# dplyr        2001.7758   2366.898   3100.9738   3044.7713   3586.5813   5626.3295   100
# data.table   235.2788    340.341    402.2612    380.2267    479.3674    841.6903    100
########################################################################

# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#



sql_3 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    
    sqldf("SELECT Id, DisplayName, TotalViews
    FROM (
    SELECT OwnerUserId, SUM(ViewCount) as TotalViews
    FROM Posts
    WHERE PostTypeId = 1
    GROUP BY OwnerUserId
    ) AS Questions
    JOIN Users
    ON Users.Id = Questions.OwnerUserId
    ORDER BY TotalViews DESC
    LIMIT 10")
  
#tworzymy ramke "Questions" korzystajac z "Posts"
    #wybieramy kolumne "OwnerUserId"
    #tworzymy kolumne, w ktorej jest suma wartosci z kolumny "ViewCount" i nazywamy ja "TotalViews"
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    #grupujemy po kolumnie "OwnerUserId"
  
#laczymy "Users" i "Questions" 
    #z "Users" wybieramy tylko kolumny: "Id" i "DisplayName"
    #laczymy je wedlug wartosci w kolumnie "Id" w "Users" i "OwnerUserId" w "Questions"
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "TotalViews"
    #wybieramy pierwsze 10 wierszy
}



base_3 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
    
# tworzymy ramke "Questions" korzystajac z "Posts"
    x <- Posts[Posts$PostTypeId ==1,]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1

    Questions <- aggregate(x$ViewCount, by=x["OwnerUserId"], FUN=sum)
    #wybieramy kolumne "OwnerUserId" i tworzymy kolumne, w ktorej jest suma wartosci 
    #z kolumny "ViewCount" w kazdej grupie. Grupujemy po kolumnie "OwnerUserId"
    
    colnames(Questions)[2] <- "TotalViews"
    #kolumne z sumami wartosci z kolumny "ViewCount" nazywamy "TotalViews"
    
    
#laczymy "Users" i "Questions" 
    x <- Users[,c("Id","DisplayName")]
    #z "Users" wybieramy tylko kolumny: "Id" i "DisplayName"
    
    x <- merge(x, Questions, by.x = "Id", by.y="OwnerUserId")
    #laczymy x i y wedlug wartosci w kolumnie "Id" w x i "OwnerUserId" w "Questions"
    
    x <- x[order(x$TotalViews, decreasing = TRUE),]
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "TotalViews"
    
    rownames(x) <- NULL
    #zmieniamy nazwy wierszy, tak aby byly one ponumerowane wedlug ich obecnej kolejnosci
    
    x <- x[1:10,]
    #wybieramy pierwsze 10 wierszy
    # 
}



dplyr_3 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
#tworzymy ramke "Questions" korzystajac z "Posts"
    Questions <- filter(Posts, PostTypeId == 1)
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    
    Questions <- group_by(Questions, OwnerUserId)
    #grupujemy po kolumnie "OwnerUserId"
    
    Questions <- summarise(Questions, TotalViews = sum(ViewCount))
    #wybieramy kolumne "OwnerUserId" i tworzymy kolumne,
    #w ktorej jest suma wartosci z kolumny "ViewCount" i nazywamy ja "TotalViews"
    
    x <- select(Users, Id, DisplayName)
    #z "Users" wybieramy tylko kolumny: "Id" i "DisplayName"
    
    
#laczymy x i "Questions" 
    y <- inner_join(x, Questions, by = c("Id" = "OwnerUserId"))
    #laczymy je wedlug wartosci w kolumnie "Id" w x i "OwnerUserId" w "Questions"
    
    y <- arrange(y, desc(TotalViews))
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "TotalViews"
    
    y <- slice(y, 1:10)
    #wybieramy pierwsze 10 wierszy
    # 
}



table_3 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
#tworzymy ramke "Questions" korzystajac z "Posts"
    Questions <- data.table(Posts)
    #zmiana typu Posts na typ: data.table
    
    Questions <- Questions[PostTypeId == 1]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    
    Questions <- Questions[,.(TotalViews = sum(ViewCount)), by = OwnerUserId]
    #grupujemy po kolumnie "OwnerUserId" i wybieramy kolumne "OwnerUserId" i tworzymy
    #kolumne, w ktorej jest suma wartosci z kolumny "ViewCount" i nazywamy ja "TotalViews"
    
    y <- data.table(Users)
    #zmiana typu "Users" na typ: data.table
    
    y <- y[,.(Id, DisplayName)]
    #z y wybieramy tylko kolumny: "Id" i "DisplayName"
    
    
#laczymy "Users" i "Questions"
    x <- y[Questions, on = c(Id = "OwnerUserId"), nomatch = NULL]
    #laczymy je wedlug wartosci w kolumnie "Id" w "Users" i "OwnerUserId" w "Questions"
    
    setorder(x, col = - "TotalViews")
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "TotalViews"
    
    x <- x[1:10, ]
    #wybieramy pierwsze 10 wierszy
    # 
}



# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# df3 <- sql_3(Posts, Users)
# 
# x3 <- base_3(Posts, Users)
# 
# dplyr::all_equal(df3,x3) #TRUE
# 
# y3 <- dplyr_3(Posts, Users)
# 
# dplyr::all_equal(df3,y3) #TRUE
# 
# z3 <- table_3(Posts,Users)
# 
# dplyr::all_equal(df3,z3) #TRUE


# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem

# microbenchmark::microbenchmark(
#     sqldf = sql_3(Posts, Users),
#     times=25L
# )
#wyniki:
# Unit: seconds
# expr           min       lq        mean       median      uq         max     neval
# sqldf       142.5393  153.2906   167.0423    173.5588  175.4305    181.7758   25
                                                                                              
                                                                                          
# microbenchmark::microbenchmark(
#       base = base_3(Posts, Users),
#       dplyr = dplyr_3(Posts, Users),
#       data.table = table_3(Posts, Users))
#                                                                                            
# #wyniki:                                                                                                                        
# Unit: milliseconds                                                                           
# expr          min        lq        mean       median      uq         max     neval
# base        358.9066  385.7320   419.52609   402.2014  452.27055   707.6826   100
# dplyr       67.4474   76.8021    93.91481    79.9215   91.30795    171.9306   100
# data.table  32.0183   39.6618    73.64622    52.8162   107.37550   310.4273   100


# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#



sql_4 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
    sqldf("SELECT DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes
          FROM (
          SELECT *
          FROM (
          SELECT COUNT(*) as AnswersNumber, OwnerUserId
          FROM Posts
          WHERE PostTypeId = 2
          GROUP BY OwnerUserId
          ) AS Answers
          JOIN
          (
          SELECT COUNT(*) as QuestionsNumber, OwnerUserId
          FROM Posts
          WHERE PostTypeId = 1
          GROUP BY OwnerUserId
          ) AS Questions
          ON Answers.OwnerUserId = Questions.OwnerUserId
          WHERE AnswersNumber > QuestionsNumber
          ORDER BY AnswersNumber DESC
          LIMIT 5
          ) AS PostsCounts
          JOIN Users
          ON PostsCounts.OwnerUserId = Users.Id")
  
#tworzymy ramke "Answers" korzystajac z "Posts"
    #wybieramy kolumne "OwnerUserId"
    #tworzymy kolumne, w ktorej jest ilosc wierszy z danej grupy i nazywamy ja "AnswersNumber"
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 2
    #grupujemy po kolumnie "OwnerUserId"
    
#tworzymy ramke "Questions" korzystajac z "Posts"
    #wybieramy kolumne "OwnerUserId"
    #tworzymy kolumne, w ktorej jest ilosc wierszy z danej grupy i nazywamy ja "QuestionsNumber"
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    #grupujemy po kolumnie "OwnerUserId"
    
#laczymy "Answers" i "Questions" tworzac ramke "PostsCounts"
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId"
    #wybieramy wiersze, w ktorych wartosci w kolumnie "AnswersNumber" jest wieksza niz w kolumnie "QuestionsNumber"
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "AnswersNumber"
    #wybieramy pierwsze 5 wierszy
  
#laczymy "PostsCounts" i "Users" 
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId" w "PostsCounts" i Id" w "Users"
    #wybieramy tylko kolumny: "DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes" i "DownVotes"
    # 
}



base_4 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
#tworzymy ramke "Answers" korzystajac z "Posts"
    Answers <- Posts[Posts$PostTypeId == 2, ]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 2
    
    Answers <- aggregate(Answers$OwnerUserId, by=Answers["OwnerUserId"], FUN = length)
    #wybieramy kolumne "OwnerUserId" i tworzymy kolumne, w ktorej jest ilosc wierszy z danej grupy
    #grupujemy po kolumnie "OwnerUserId"
    
    colnames(Answers)[2] <- "AnswersNumber"
    #kolumne, w ktorej jest ilosc wierszy z danej grupy nazywamy "AnswersNumber"


#tworzymy ramke "Questions" korzystajac z "Posts"
    Questions <- Posts[Posts$PostTypeId == 1, ]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    
    Questions <- aggregate(Questions$OwnerUserId, by=Questions["OwnerUserId"], FUN = length)
    #wybieramy kolumne "OwnerUserId" i tworzymy kolumne, w ktorej jest ilosc wierszy z danej grupy
    #grupujemy po kolumnie "OwnerUserId"
    
    colnames(Questions)[2] <- "QuestionsNumber"
    #kolumne, w ktorej jest ilosc wierszy z danej grupy nazywamy "QuestionsNumber"
    
    
#laczymy "Answers" i "Questions" tworzac ramke "PostsCounts"
    PostsCounts <- merge(Answers, Questions, by = "OwnerUserId")
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId"
    
    PostsCounts <- PostsCounts[PostsCounts$AnswersNumber > PostsCounts$QuestionsNumber,]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "AnswersNumber" jest wieksza niz w kolumnie "QuestionsNumber"
    
    PostsCounts <- PostsCounts[order(PostsCounts$AnswersNumber, decreasing = TRUE),]
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "AnswersNumber"
    
    PostsCounts <- head(PostsCounts,5)
    #wybieramy pierwsze 5 wierszy
    

#laczymy "PostsCounts" i "Users" 
    x <- merge(Users, PostsCounts, by.x="Id", by.y="OwnerUserId")
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId" w "PostsCounts" i "Id" w "Users"
    
    x <- x[c("DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes", "DownVotes")]
    #wybieramy tylko kolumny: "DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes" i "DownVotes"
    # 
}



dplyr_4 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
#tworzymy ramke "Answers" korzystajac z "Posts"
    Answers <- filter(Posts, PostTypeId == 2)
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 2
    
    Answers <- group_by(Answers, OwnerUserId)
    #grupujemy po kolumnie "OwnerUserId"
    
    Answers <- summarise(Answers, AnswersNumber=length(OwnerUserId))
    #wybieramy kolumne "OwnerUserId" i tworzymy kolumne,
    #w ktorej jest ilosc wierszy z danej grupy i nazywamy ja "QuestionsNumber"
    
    
#tworzymy ramke "Questions" korzystajac z "Posts"
    Questions <- filter(Posts, PostTypeId == 1)
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    
    Questions <- group_by(Questions, OwnerUserId)
    #grupujemy po kolumnie "OwnerUserId"
    
    Questions <- summarise(Questions, QuestionsNumber=length(OwnerUserId))
    #wybieramy kolumne "OwnerUserId" i tworzymy kolumne,
    #w ktorej jest ilosc wierszy z danej grupy i nazywamy ja "QuestionsNumber"
    
    
#laczymy "Answers i "Questions" 
    PostsCounts <- inner_join(Answers, Questions, by = "OwnerUserId", na_matches = "never")
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId"
    
    PostsCounts <- filter(PostsCounts, AnswersNumber > QuestionsNumber)
    #wybieramy wiersze, w ktorych wartosci w kolumnie "AnswersNumber" jest wieksza niz w kolumnie "QuestionsNumber"
    
    PostsCounts <- arrange(PostsCounts, desc(AnswersNumber))
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "AnswersNumber"
    
    PostsCounts <- slice(PostsCounts, 1:5)
    #wybieramy pierwsze 10 wierszy
    
    
#laczymy "PostsCounts" i "Users" 
    x <- inner_join(PostsCounts, Users, by = c("OwnerUserId" = "Id"))
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId" w "PostsCounts" i "Id" w "Users"
    
    x <- select(x, one_of(c("DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes", "DownVotes")))
    #wybieramy tylko kolumny: "DisplayName", "QuestionsNumber", "AnswersNumber", "Location", "Reputation", "UpVotes" i "DownVotes"
    # 
}



table_4 <- function(Posts, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
#tworzymy ramke "Answers" korzystajac z "Posts"
    Answers <- data.table(Posts)
    #zmiana typu Posts na typ: data.table
    
    Answers <- Answers[PostTypeId == 2]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 2
    
    Answers <- Answers[,.N, by = OwnerUserId]
    #grupujemy po kolumnie "OwnerUserId" i wybieramy kolumne "OwnerUserId" i tworzymy
    #kolumne, w ktorej jest ilosc wierszy z danej grupy
    
    setnames(Answers, "N", "AnswersNumber")
    #kolumne, w ktorej jest ilosc wierszy z danej grupy nazywamy "AnswersNumber"
    
    
#tworzymy ramke "Questions" korzystajac z "Posts"
    Questions <- data.table(Posts)
    #zmiana typu Posts na typ: data.table
    
    Questions <- Questions[PostTypeId == 1]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    
    Questions <- Questions[,.N, by = OwnerUserId]
    #grupujemy po kolumnie "OwnerUserId" i wybieramy kolumne "OwnerUserId" i tworzymy
    #kolumne, w ktorej jest ilosc wierszy z danej grupy
    
    setnames(Questions, "N", "QuestionsNumber")
    #kolumne, w ktorej jest ilosc wierszy z danej grupy nazywamy "QuestionsNumber"
    
    
#laczymy "Answers" i "Questions" tworzac ramke "PostsCounts"
    
    PostsCounts <- na.omit(Answers[Questions, on = "OwnerUserId", nomatch = NULL])
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId" (omijamy wiersze z na)
    
    PostsCounts <- PostsCounts[AnswersNumber > QuestionsNumber]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "AnswersNumber" jest wieksza niz w kolumnie "QuestionsNumber"
    
    setorder(PostsCounts, col = - "AnswersNumber")
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "AnswersNumber"
    
    PostsCounts <- PostsCounts[1:5, ]
    #wybieramy pierwsze 5 wierszy
    
    
#laczymy "PostsCounts" i "Users" 
    y <- data.table(Users)
    #zmiana typu "Users" na typ: data.table
    
    x <- PostsCounts[y, on = c(OwnerUserId = "Id"), nomatch = NULL]
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId" w "PostsCounts" i "Id" w y
    
    x <- x[,.(DisplayName, QuestionsNumber, AnswersNumber, Location, Reputation, UpVotes, DownVotes)]
    #z y wybieramy tylko kolumny: "DisplayName", "QuestionsNumber", "AnswersNumber", "Location", 
    #"Reputation", "UpVotes" i "DownVotes"
    # 
}



# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem


# df4 <- sql_4(Posts, Users)
# 
# x4 <- base_4(Posts, Users)
# 
# dplyr::all_equal(df4,x4) #TRUE
# 
# y4 <- dplyr_4(Posts, Users)
# 
# dplyr::all_equal(df4,y4) #TRUE
# 
# z4 <- table_4(Posts,Users)
# 
# dplyr::all_equal(df4,z4) #TRUE



# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem


# microbenchmark::microbenchmark(
#   sqldf = sql_4(Posts, Users),
#   base = base_4(Posts, Users),
#   dplyr = dplyr_4(Posts, Users),
#   data.table = table_4(Posts, Users)
# )
#wyniki
# Unit: milliseconds 
# expr            min         lq         mean       median        uq         max      neval
# sqldf        1771.0159   1866.6812   3354.3973   4508.5452   4553.3075   4996.2182   100
# base         510.5675    617.7029    1068.5847   1372.5435   1477.2001   1893.6591   100
# dplyr        113.2258    153.4382    244.1659    258.6149    270.3804    544.7556    100
# data.table   72.5920     154.0689    206.2060    224.4683    251.2815    567.7276    100



# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#



sql_5 <- function(Posts, Comments, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
    sqldf("SELECT Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location
          FROM (
          SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount, Posts.ViewCount,
          CmtTotScr.CommentsTotalScore
          FROM (
          SELECT PostId, SUM(Score) AS CommentsTotalScore
          FROM Comments
          GROUP BY PostId
          ) AS CmtTotScr
          JOIN Posts ON Posts.Id = CmtTotScr.PostId
          WHERE Posts.PostTypeId=1
          ) AS PostsBestComments
          JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
          ORDER BY CommentsTotalScore DESC
          LIMIT 10
          ")
  
#tworzymy ramke "CmtTotScr" korzystajac z "Comments"
    #wybieramy kolumne "PostId"
    #tworzymy kolumne, w ktorej jest suma wartosci z kolumny "Score" w danej grupie i nazywamy ja "CommentsTotalScore"
    #grupujemy po kolumnie "PostId"
    
#laczymy "CmtTotScr" i "Posts" tworzac ramke "PostsBestComments"
    #laczymy je wedlug wartosci w kolumnie ""PostId" w "CmtTotScore" i "Id" w "Posts"
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    #wybieramy tylko kolumny: "OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore"
  
    
#laczymy "PostsBestComments" i "Users" 
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId" w "PostsBestComments" i Id" w "Users"
    #wybieramy tylko kolumny: "Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location"
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "CommentsTotalScore"
    #wybieramy pierwsze 10 wierszy
    # 
}



base_5 <- function(Posts, Comments, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
#tworzymy CmtTotScr
    CmtTotScr  <- aggregate(Comments$Score, by=Comments["PostId"], FUN = sum)
    #wybieramy kolumne "PostId" i tworzymy kolumne z suma wartosci z kolumny "Score" w kazdej grupie
    #grupujemy po kolumnie "PostId"
    
    colnames(CmtTotScr)[2] <- "CommentsTotalScore"
    #zmieniamy nazwe kolumny z suma wartosci z kolumny "Score" w kazdej grupie na "CommentsTotalScore"
    
  
#laczymy "CmtTotScr" i "Posts" tworzac ramke "PostsBestComments"
    PostsBestComments <- merge(CmtTotScr, Posts, by.x = "PostId", by.y = "Id")
    #laczymy je wedlug wartosci w kolumnie "PostId" w "CmtTotScore" i "Id" w "Posts"
    
    PostsBestComments <- PostsBestComments[PostsBestComments$PostTypeId == 1,]
    #wybieramy wiersze, w ktorych wartosć w kolumnie "PostTypeId" jest rowna 1
    
    PostsBestComments <- PostsBestComments[c("OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore")]
    #wybieramy tylko kolumny: "OwnerUserId", "Title", "CommentCount", "ViewCount" i "CommentsTotalScore"
    
    
#laczymy "PostsBestsComments" i "Users"
    x <- merge(PostsBestComments, Users, by.x = "OwnerUserId", by.y = "Id")
    #laczymy je wedlug wartosci w kolumnie "Id" w "CmtTotScore" i "PostId" w "Posts"
    
    x <- x[c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location")]
    #wybieramy tylko kolumny: "Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation" i "Location"
    
    x <- x[order(x$CommentsTotalScore, decreasing = TRUE),]
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "CommentsTotalScore"
    
    rownames(x) <- NULL
    #zmieniamy nazwy wierszy, tak aby byly one ponumerowane wedlug ich obecnej kolejnosci
    
    x <- head(x, 10)
    #wybieramy pierwsze 10 wierszy
    # 
}



dplyr_5 <- function(Posts, Comments, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
#tworzymy CmtTotScr
    CmtTotScr <- group_by(Comments, PostId)
    #grupujemy po kolumnie "OwnerUserId"
    
    CmtTotScr <- summarise(CmtTotScr, CommentsTotalScore=sum(Score))
    #wybieramy kolumne "OwnerUserId" i tworzymy kolumne,
    #w ktorej jest suma wartosci z kolumny "Score" w danej grupie i nazywamy ja "CommentsTotalScore"
    
    
#laczymy "CmtTotScore" i "Posts" tworzac ramke "PostsBestComments"
    PostsBestComments <- inner_join(CmtTotScr, Posts, by = c("PostId" = "Id"), na_matches = "never")
    #laczymy je wedlug wartosci w kolumnie "PostId" w "CmtTotScore" i "Id" w "Posts"
    
    PostsBestComments <- filter(PostsBestComments, PostTypeId == 1)
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    
    PostsBestComments <- select(PostsBestComments, one_of(c("OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore")))
    #wybieramy tylko kolumny: "OwnerUserId", "Title", "CommentCount", "ViewCount", "CommentsTotalScore"
    
    
#laczymy "PostsBestsComments" i "Users"
    x <- inner_join(PostsBestComments, Users, by = c("OwnerUserId" = "Id"), na_matches = "never")
    #laczymy je wedlug wartosci w kolumnie "PostId" w "PostsBestComments" i "Id" w "Users"
    
    x <- select(x, one_of(c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location")))
    #wybieramy tylko kolumny: "Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location"
    
    x <- arrange(x, desc(CommentsTotalScore))
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "CommentsTotalScore"
    
    x <- slice(x, 1:10)
    #wybieramy pierwsze 10 wierszy
    # 
}



table_5 <- function(Posts, Comments, Users){
    # Tu umiesc rozwiazanie oraz komentarze
  
#tworzymy CmtTotScr
    CmtTotScr <- data.table(Comments)
    #zmiana typu Comments na typ: data.table
    
    CmtTotScr <- CmtTotScr[,.(CommentsTotalScore = sum(Score)), by = PostId]
    #grupujemy po kolumnie "PostId" i wybieramy kolumne "PostId" i tworzymy
    #kolumne, w ktorej jest suma wartosci z kolumny "Score" w danej grupie i nazywamy ja "CommentsTotalScore"
    
    
#laczymy "CmtTotScore" i "Posts" tworzac ramke "PostsBestComments"
    PostsBestComments <- CmtTotScr[Posts, on = c(PostId = "Id"), nomatch = NULL]
    #laczymy je wedlug wartosci w kolumnie "PostId" w "CmtTotScore" i "Id" w "Posts"
    
    PostsBestComments <- PostsBestComments[PostTypeId == 1]
    #wybieramy wiersze, w ktorych wartosci w kolumnie "PostTypeId" jest rowna 1
    
    PostsBestComments <- PostsBestComments[,.(OwnerUserId, Title, CommentCount, ViewCount, CommentsTotalScore)]
    #wybieramy tylko kolumny: "OwnerUserId", "Title", "CommentCount", "ViewCount" i "CommentsTotalScore"
    
    
#laczymy "PostsBestComments" i "Users" 
    x <- PostsBestComments[Users, on = c(OwnerUserId = "Id"), nomatch = NULL]
    #laczymy je wedlug wartosci w kolumnie "OwnerUserId" w "PostsBestComments" i "Id" w "Users"
    
    x <- x[,.(Title, CommentCount, ViewCount, CommentsTotalScore, DisplayName, Reputation, Location)]
    #wybieramy tylko kolumny: ""Title", "CommentCount", "ViewCount", "CommentsTotalScore", "DisplayName", "Reputation", "Location"
    
    setorder(x, col = - "CommentsTotalScore")
    #ustawiamy otrzymane wiersze w kolejnosci malejacej wedlug wartosci z kolumny "CommentsTotalScore"
    
    x <- x[1:10, ]
    #wybieramy pierwsze 10 wierszy
    # 
}



# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem


# df5 <- sql_5(Posts, Comments, Users)
# 
# x5 <- base_5(Posts, Comments, Users)
# 
# dplyr::all_equal(df5,x5) #TRUE
# 
# y5 <- dplyr_5(Posts, Comments, Users)
# 
# dplyr::all_equal(df5,y5) #TRUE
# 
# z5 <- table_5(Posts, Comments, Users)
# 
# dplyr::all_equal(df5,z5) #TRUE



# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem

# microbenchmark::microbenchmark(sqldf = sql_5(Posts, Comments, Users), times = 25L)

#wyniki:
# Unit: seconds
# expr            min         lq         mean       median        uq         max      neval
# sqldf        1293.534    1335.134    2680.904    1508.686    1545.566    32085.59    25


# microbenchmark::microbenchmark(base = base_5(Posts, Comments, Users), 
#                                dplyr = dplyr_5(Posts, Comments, Users), 
#                                data.table = table_5(Posts, Comments, Users))
# #wyniki:
# Unit: milliseconds
# expr            min         lq         mean       median        uq          max     neval
# base         1186.6671   1312.5122   1354.6563   1351.3472   1383.3147   1658.4999   100
# dplyr        190.5712    212.2458    252.0555    259.0391    271.1394    512.4280    100
# data.table   75.0846     82.0878     119.5607    133.8983    144.6281    368.5164    100

