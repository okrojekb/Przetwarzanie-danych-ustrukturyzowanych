install.packages("nycflights13")

library("nycflights13")


class(airlines)

options(stringsAsFactors = FALSE)

airports <- as.data.frame(airports)
flights <- as.data.frame(flights)
planes <- as.data.frame(planes)
weather <- as.data.frame(weather)
airlines <- as.data.frame(airlines)

unique(c(5,2,6,4,2,5,1))

airports

#sprawdzenie wymiarow
ncol(airports)
nrow(airports)
dim(airports)

colnames(airports)
rownames(airports)

airports["name"]
airports$"name"
airports[1:10,"faa"]

# zadanie 4.2

# a
df <- sqldf("SELECT DISTINCT engine FROM planes")

m <- unique(planes["engine"])
rownames(m) <- 1:6
all(df == m)
dplyr::all_equal(df, m)


f <- data.frame(engine = unique(planes$engines))
all.equal(df, f)

# b
sqldf("SELECT DISTINCT type, manufacturer FROM planes")

v <- unique(planes[c("type","manufacturer")])
rownames(v) <- NULL
v

# c
sqldf("SELECT COUNT(*), engine FROM planes GROUP BY engine")
df <- sqldf("SELECT COUNT(*) AS count, engine FROM planes GROUP BY engine")
df


x <- aggregate(planes$engine, by = planes["engine"], FUN = length)
df
x
colnames(x)[2] <- "count"
x<-x[c("count", "engine")]
all.equal(df,x)


x <- as.data.frame(table(planes$engine), stringsAsFactors = FALSE)
x
colnames(x) <- c("engine","count")
x<-x[c("count", "engine")]
x
all.equal(df,x)



groups <- split(planes$engine, planes["engine"])
lapply(groups, length)
sapply(groups, length)


# d

df <- sqldf("SELECT COUNT(*), engine, type FROM planes GROUP BY engine, type")
df

x <- aggregate(planes$engine, by=planes[c("engine", "type")], FUN = length)

colnames(x)[3] <- "count"

all.equal(df,x)


x <- as.data.frame(table(planes[c("engine", "type")]))
x
x <- x[x$Freq > 0, ]
x
colnames(x)[3] <- "count"
x

# e
df <- sqldf("SELECT MIN(year) AS minYear, engine,
manufacturer
FROM planes
GROUP BY engine, manufacturer")
df


x <- aggregate(x = planes["year"], by = planes[c("engine", "manufacturer")], FUN = min, na.rm = TRUE)
colnames(x)[3] <- "minYear"
x[!is.finite(x$minYear), "minYear"] <- NA
x





# 4.3

# a

df <- sqldf("SELECT * FROM planes WHERE speed IS NOT NULL")

x <- planes[!is.na(planes$speed), ]
x
rownames(x) <- NULL


# b


df <- sqldf("SELECT tailnum 
FROM planes
WHERE seats BETWEEN 150 AND 190 AND year >= 2012")
df

x <- na.omit(planes[planes$seats > 150 & planes$seats <= 190 & planes$year >= 2012, "tailnum" , drop = FALSE])
rownames(x) <- NULL

x

# c
df <- sqldf("SELECT * FROM planes
WHERE manufacturer IN ('BOEING', 'AIRBUS', 'EMBRAER') AND seats > 390")

x <- planes[planes$seats > 390 & planes$manufacturer %in% c('BOEING', 'AIRBUS', 'EMBRAER'),]
rownames(x) <- NULL
x
dplyr::all_equal(df,x)


# d
df <- sqldf("SELECT DISTINCT year, seats
FROM planes
WHERE year >= 2012
ORDER BY year ASC, seats DESC")
df

x <- unique(na.omit(planes[planes$year >= 2012, c("year", "seats")]))
x <- x[order(x$seats, decreasing = TRUE),]
x <- x[order(x$year, decreasing = FALSE),]
x
dplyr::all_equal(df,x)

# drugi sposob
x <- unique(na.omit(planes[planes$year >= 2012, c("year", "seats")]))
x
x <- x[order(x$year, x$seats, decreasing = c(TRUE, FALSE)),]
x
dplyr::all_equal(df,x)


# e
df <- sqldf("SELECT DISTINCT year, seats
FROM planes
WHERE year >= 2012
ORDER BY seats DESC, year ASC")
df
x <- unique(na.omit(planes[planes$year >= 2012, c("year", "seats")]))
x
x<- x[order(x$seats, x$year, decreasing = c(TRUE, FALSE)),]
x




#zadanie 4.4

# a
df <- sqldf("SELECT manufacturer, COUNT(*)
FROM planes
WHERE seats > 200
GROUP BY manufacturer")
df
x <- planes[planes$seats > 200,]
x <- aggregate(x$manufacturer, by=x["manufacturer"], FUN = length)
colnames(x)[2] <- "COUNT(*)"
x
dplyr::all_equal(df,x)

#drugi sposob

x <- as.data.frame(table(planes[planes$seats > 200, "manufacturer"]), stringsAsFactors = FALSE)
colnames(x) <- c("manufacturer", "COUNT(*)")



# b
df <- sqldf("SELECT manufacturer, COUNT(*)
FROM planes
GROUP BY manufacturer
HAVING COUNT(*) > 10")
df
x <- aggregate(planes$manufacturer, by=planes["manufacturer"], FUN = length)
x <- x[x$x > 10,]
colnames(x)[2] <- "COUNT(*)"
rownames(x) <- NULL
x

#drugi sposob
x <- as.data.frame(table(planes["manufacturer"]), stringsAsFactors = FALSE)
x <- x[x$Freq > 10,]
colnames(x)[2] <- "COUNT(*)"
rownames(x) <- NULL
x




# d
df <- sqldf("SELECT manufacturer, COUNT(*) AS howmany
FROM planes
GROUP BY manufacturer
ORDER BY howmany
DESC LIMIT 5")
df

x <- aggregate(planes$manufacturer, by=planes["manufacturer"], FUN = length)
colnames(x)[2] <- "howmany"
x <- x[order(x$howmany, decreasing = TRUE),]
#[order(-x$howmany),] rownowazne

x <- head(x, 5)
x


#zadanie 4.5


# a
df <- sqldf("SELECT flights.tailnum, flights.year, flights.month, flights.day,
flights.carrier, planes.manufacturer, planes.type
FROM flights
LEFT JOIN
planes
ON flights.tailnum=planes.tailnum")

x <- flights[,c("tailnum", "year", "month", "day", "carrier")]
y <- planes[c("manufacturer", "type")]
df2 <- merge(x = x, y = y, by = 'tailnum', all.x = TRUE, all.y=FALSE)
df2      









