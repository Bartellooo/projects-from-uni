set.seed(124)

library(sampling)
library(samplingbook)
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/populacja.RData")
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/proba.RData")
dane_popul <- as.data.frame(dane_popul)
dane_proba <- as.data.frame(dane_proba)

## zmienna objaśniana - y

n=35 #liczebność próby
N=nrow(dane_popul)  #liczebność populacji
Nd=sum(dane_popul$id==7)  #liczba powiatów w regionie 7 w populacji

## Plan losowania Poisssona
pik <- inclusionprobabilities(dane_popul$x, n)
s <- sample

id_s<-as.numeric(s) #identyfikatory przynależności do próby (1-należy do próby)
ys<-dane_popul$y[id_s==1] ## Wyznaczenie wartości zmiennej objaśnianej 
## dla wylosowanych elementów próby

## Prawdopodobieństwa inkluzji w planie Losowania Poissona

pk<-pik[id_s==1] # pierwszego rzędu
PI<-pk%*%t(pk) # drugiego rzędu
diag(PI)=pk

## dane tylko dla domeny 7
ysd<-ys
domenyprobka<-dane_popul$id[id_s==1]  ### określenie domen, które występują w próbce
ysd[domenyprobka!=7]=0

#ESTYMATOR H-T -----
################################################################################
## oszacowanie w. średniej w domenie ---- 

### Estymator H-T

estHT_srd<-(N/Nd)*htestimate(ysd,N,PI,pk,method='ht')$mean
estHT_srd

## INTERPRETACJA Szacuje się że wartość średnia estymatora dla zmiennej y
## w 7 domenie wynosi 5599,181.


### Średni Błąd Szacunku 

eD.ht_estHT_srd<-(N/Nd)*htestimate(ysd,N,PI,pk,method='ht')$se
eD.ht_estHT_srd

## INTERPRETACJA Szacuje się, że wartości estymatora wartości średniej zmiennej y w 7 domenie 
## odchylają się od jego wartości oczekiwanej przeciętnie o 2585,379.

### Średni względny błąd szacunku

egamma.ht_estHT_srd<-100*eD.ht_estHT_srd/estHT_srd
egamma.ht_estHT_srd

## INTEPRETACJA - szacuje się że ocena średniego względnego błędu 
## szacunku stanowi 46,17% wartości estymatora wartości średniej zmiennej y w domenie 7.


## Oszacowanie w. globalnej w domenie ----

## Estymator H-T

estHT_wgd<-N*htestimate(ysd,N,PI,pk,method='ht')$mean
estHT_wgd

## INTERPRETACJA Szacuje się że wartość globalna estymatora
## zmiennej y w 7 domenie wynosi 207169,7.

# wartość estymatora p-średniego błędu szacunku estymatora

eD.estHT_wgd<-N*htestimate(ysd,N,PI,pk,method='ht')$se
eD.estHT_wgd

## INTERPRETACJA Szacuje się, że wartości estymatora wartości globalnej zmiennej y w 7 domenie 
## odchylają się od jego wartości oczekiwanej przeciętnie o 95659,04.


# wartość estymatora względnego p-średniego błędu szacunku estymatora

egamma.ht_estHT_wgd<-100*eD.estHT_wgd/estHT_wgd
egamma.ht_estHT_wgd

## INTEPRETACJA - szacuje się że ocena średniego względnego błędu 
## szacunku stanowi 46,17% wartości estymatora wartości globalnej zmiennej y w domenie 7.