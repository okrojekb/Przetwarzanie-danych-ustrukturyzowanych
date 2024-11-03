#7.11


set.seed(1234)
g <- factor(c(rep('M', 15), rep('K', 15)))
h <- factor(sample(c('wysoki', 'sredni', 'niski'), replace=TRUE, 30))


#sposob 1 - bazowy R

tab <- table(g,h)
barplot(tab, beside =TRUE, col = c("red", "green"))
mean(par("usr"))
legend(7, 9, -1, fill = c("red", "green"), legend = c("K", "M"))

