
# ladowanie pakietów 
library(ggplot2)
library(readr)
library(tidyverse)

#ladowanie pliku CSV
Dane <- read.csv("Life_Expectancy_00_15.csv", sep = ";")
colnames(Dane)

# Filtrowanie zbioru danych, aby pokazywal tylko dane dla Polski 

Poland <- Dane %>% filter(Country == "Poland") 

# Wykres numer 1 - Populacja Polski w latach 2000-2015
ggplot(Poland, aes(Year,Population,))+geom_point(size=3, color ='black')+geom_line(color='red')+
  labs(title = "Populacja Polski w latach 2000-2015", x = "Rok", y="liczba mieszkanców")+theme(panel.background = element_rect(fill="lightblue"), aspect.ratio = 5/12)

#instalowanie pakietu map - maps i mapproj

install.packages("maps")
install.packages("mapproj")
library(maps)
library(mapproj)

# stworzenie ramki danych na rok 2013

Zalesienie <- Dane %>% filter(Year == "2013") %>% data.frame(Country = tolower(rownames(Dane)), Dane)

# stworzenie mapy obejmujacej kraje Ameryki Poludniowej

mapka <- map_data("world", regions=c("Brazil","Uruguay", "Colombia", "Argentina", "Peru", "Venezuela", "Bolivia",
                                     "Ecuador","Chile","Paraguay"))

# Polaczenie i posegregowanie danych "Zalesienie" oraz "mapka"

Mapka_zalesienia <- merge(mapka, Zalesienie, by.x = "region", by.y = "Country")
Mapka_zalesienia <- arrange(Mapka_zalesienia, group, order)


# Wykres numer 2 - Zalesienie w krajach Ameryki Poludniowej w Roku 2013
# UWAGA - wykres nie zawiera wszystkich Panstw (Brak Surinamu, Gujany czy tez terytorium zamorskiego Francji - Gujany Francuskiej)
# Wynika to z faktu, iz kraje te sa róznie nazywane w tych dwóch oddzielnych zbiorach!!!
ggplot(Mapka_zalesienia, aes(x = long, y = lat, group = group, fill = Forest.area)) +
  geom_polygon(colour="black") +
  coord_map() + scale_fill_gradient2(low="#559999", mid = "grey90", high = "#BB650B",
                                   midpoint=median(Zalesienie$Forest.area))+
  ggtitle("Zalesienie w krajach Ameryki Poludniowej w roku 2013" ,"(% calkowitej powierzchni kraju)")

# Przefiltrowanie danych do roku 2015 i utworzenie ramki danych zmiennej sredniego PKB per Capita w roku 2015 wedlug kontynentów
dane2015 <- Dane %>% filter(Year == 2015)
Srednie_PKB_2015 <- aggregate(GDP.per.capita ~ Continent, data = dane2015, FUN = mean)

# Wykres numer 3 - PKB Per Capita w Parytecie sily nabywczej (Biezace $) w roku 2015 wedlug kontynentów
ggplot(Srednie_PKB_2015, aes(Continent, GDP.per.capita))+geom_bar(stat = 'identity', color = 'yellow', fill ='red')+scale_fill_brewer(palette = 3)+
  theme(panel.background = element_rect(fill="black"), panel.grid.major = element_line(color = "gray60", size = 0.8), aspect.ratio = 1, plot.title = element_text(size=12))+coord_flip()+
  ggtitle("PKB per capita w podziale na kontynenty w roku 2015 \n(wedlug biezacych USD)")+
  theme(plot.title = element_text(size = rel(1), lineheight = .9, face = "bold.italic", colour = "red"))+
  labs(x="Kontynent", y = "Wartosc w USD")

# Wyselekcjonowanie pojedynczych panstw ze zbioru "Dane"
kraje <- c('United States','Poland','Sudan','Bahrain','Ecuador')
subset(Dane, Country %in% kraje)

ggplot(subset(Dane, Country %in% kraje), aes(Year, Health.expenditure, color = Country))+
  geom_line()+geom_point()+facet_wrap(~Country)+ggtitle("Wydatki na ochrone zdrowia w poszczególnych krajach w latach 2000-2015")+
  theme(title=element_text(color="Black", size = 10, face = "bold"), legend.position = "none")+
  labs(x = "Rok", y ="% PKB")

#instalacja pakietu Gganimate do wykresów animowanych
install.packages("gganimate")
library(gganimate)

# Wykres 5 - zaleznosc miedzy emisja dwutlenku wegla a oczekiwana dlugoscia zycia w krajach Europejskich w Latach 2000-2015
Europa <- Dane %>% filter(Continent == "Europe")
ggplot(Europa, aes(x=CO2.emissions,y=Life.Expectancy, colour = Country))+
  geom_point(alpha=0.7, aes(size = Population))+transition_time(Year)+
  labs(x="Emisja CO2", y="Oczekiwana dlugosc zycia", title = 'Rok:{frame_time}')
