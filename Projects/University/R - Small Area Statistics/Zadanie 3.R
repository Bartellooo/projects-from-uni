set.seed(124)

library(sampling)
library(samplingbook)
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/populacja.RData")
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/proba.RData")
dane_popul <- as.data.frame(dane_popul)
dane_proba <- as.data.frame(dane_proba)
attach(dane_popul)
attach(dane_proba)

n=35 #liczebność próby
N=nrow(dane_popul)  #liczebność populacji
Suma_domena7=sum(dane_popul$id==7)  #liczba elementów domeny 7 w populacji

#wybór próby LOSOWANIE METODĄ POISSONA
pik<-inclusionprobabilities(dane_popul$x,n)
## s <-UPpoisson(pik)

s<- sample

id_s <- as.numeric(s)
ys<-dane_popul$y[id_s==1]
n<-sum(id_s)


## Prawdopodobieństwa inkluzji w planie losowania Poissona
pk<-pik[id_s==1] # pierwszego rzędu (czyli tutaj bierze z pik które należą do próbki)
PI<-pk%*%t(pk) # drugiego rzędu
diag(PI)=pk

## dane tylko dla domeny 7
ys<-dane_popul$y[id_s==1] ## Wyznaczenie wartości zmiennej objaśnianej 
## dla wylosowanych elementów próby
ysd<-ys
domena7<-dane_popul$id[id_s==1]
ysd[domena7!=7]=0


#ESTYMATOR H-T -----
################################################################################
## oszacowanie w. średniej w domenie ---- 

### Estymator H-T

estHT_srd<-(N/Suma_domena7)*htestimate(ysd,N,PI,pk,method='yg')$mean
estHT_srd

## INTERPRETACJA Szacuje się że wartość średnia estymatora
## zmiennej y w 7 domenie wynosi 5599,181.


## Oszacowanie w. globalnej w domenie ----

## Estymator H-T

estHT_wgd<-N*htestimate(ysd,N,PI,pk,method='yg')$mean
estHT_wgd

## INTERPRETACJA Szacuje się że wartość globalna estymatora
## zmiennej y w 7 domenie wynosi 207169,7.

