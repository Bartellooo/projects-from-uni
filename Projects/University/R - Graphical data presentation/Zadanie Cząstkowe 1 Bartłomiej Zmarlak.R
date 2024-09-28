# Zaladowanie pakietu z danymi Stat2Data
install.packages("Stat2Data")
library(Stat2Data)

# ladowanie zbioru danych/ramki danych NFLStandings 2016 z pakietu
data("NFLStandings2016")
attach(NFLStandings2016)

# wykres slupkowy - liczba Zwyciestw oraz liczba klub?w kt?ra je odniosla - Sezon Zasadniczy NFL 2016

table(NFLStandings2016$Wins)
barplot(table(NFLStandings2016$Wins), main="Liczba zwyciestw odniesiona przez kluby NFL w sezonie zasadniczym 2016",
        cex.main=1, col=3, xlab="liczba zwyciestw",
        ylab= "Liczba druzyn, kt?ra odniosla dana liczbe zwyciestw")

# histogram - liczba zdobytych punkt?w w przedzialach oraz liczba klub?w kt?ra zdobyla liczbe punkt?w mieszczaca sie w danym przedziale


hist(PointsFor, ylim=c(0,13),
     main="Histogram lacznej liczby zdobytych punkt?w w sezonie zasadniczym NFL 2016",
     xlab="Laczna liczba zdobytych punkt?w (przedzial)",
     ylab="Liczba druzyn, kt?ra zdobyla ilosc punkt?w mieszczaca sie w danym przedziale", cex.lab=.7, cex.main=.9, col="orange")


# Korelacja

# Wyliczenie korelacji pomiedzy liczba zwyciestw, a laczna liczba zdobytych i straconych punkt?w
NFLStandings2016[,c(2,6,7)]
cor(NFLStandings2016[,c(2,6,7)])
# ladowanie biblioteki ellipse
install.packages("ellipse")
library(ellipse)
# wykres korelacji
plotcorr(cor(NFLStandings2016[,c(2,6,7)]), col = "yellow",
         main="korelacja pomiedzy liczba zwyciestw, a laczna liczba zdobytych i straconych punkt?w",
         cex.main=.9)
# interpretacje - 1. im wieksza liczba zwyciestw, tym zazwyczaj wieksza
# laczna liczba zdobytych punkt?w, 2. zas im wiecej zwyciestw, tym mniejsza jest laczna liczba straconych punkt?w
# 3. slaba zaleznosc miedzy laczna liczba zdobytych punkt?w a straconych


# Wykres punktowy

# Wykres punktowy lacznej liczby przylozen w zaleznosci od druzyny NFL
# celem lepszego widoku, prosze kliknac na zoom
dotchart(TDs,Team, xlab="laczna liczba przylozen w sezonie zasadniczym",
         cex=.7, xlim = c(20,65), lcolor = "Red", color=46,
         main = "laczna liczba przylozen w sez. zasadniczym 2016 wg klub?w NFL",
         col.main= 45)


# Wykres pudelkowy
# obrazujacy laczna liczbe zdobytych i straconych jard?w w sezonie zas. 2016


summary(YardsFor)
summary(YardsAgainst)
boxplot(YardsFor,YardsAgainst, ylab="liczba zdobytych/straconych jard?w og?lem",
        col="brown", main="Statystyki dotyczace zdobytych i straconych jard?w og?lem w sez. zas. NFL 2016",
        xlab= "1 pudelko - jardy zdobyte
2 pudelko - jardy stracone",
        cex.main=.8, cex.lab=.8, col.axis=10, las=1, ylim=range(4000,7000))



