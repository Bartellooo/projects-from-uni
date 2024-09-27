set.seed(124)

library(sampling)
library(samplingbook)
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/populacja.RData")
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/proba.RData")
dane_popul <- as.data.frame(dane_popul)
dane_proba <- as.data.frame(dane_proba)

attach(dane_popul)
attach(dane_proba)

## zmienna objaśniana - y

n=35 #liczebność próby
N=nrow(dane_popul)  #liczebność populacji
Nd=sum(dane_popul$id==7)  #liczba elementów domeny 7 w populacji


#wybór próby LOSOWANIE PRÓBY BEZ ZWRACANIA
### Ale Nie ma sensu jak jest dany zbiór dane_proba (???) s<-srswor(n,N)
## s <- srswor(n,N)
s <- sample


id_s<-as.numeric(s) #identyfikatory przynależności do próby (1-należy do próby)
ys<-dane_popul$y[id_s==1] ## Wyznaczenie wartości zmiennej objaśnianej 
## dla wylosowanych elementów próby


#prawdopodobieństwa inkluzji
pk<-rep(n/N,n) # pierwszego rzędu
PI<-matrix((n*(n-1))/(N*(N-1)),ncol=n,nrow=n) # drugiego rzędu
diag(PI)=n/N

## dane tylko dla domeny 7
ysd<-ys
domena7<-dane_popul$id[id_s==1]
ysd[domena7!=7]=0

#ESTYMATOR H-T -----
################################################################################
## oszacowanie w. średniej w domenie ---- 

### Estymator H-T

estHT_srd<-(N/Nd)*htestimate(ysd,N,PI,pk,method='ht')$mean
estHT_srd

## INTERPRETACJA Szacuje się że wartość średnia estymatora
## w 7 domenie wynosi 6096,81.


### Estymator p-średniego Błędu Szacunku 

eD.ht_estHT_srd<-(N/Nd)*htestimate(ysd,N,PI,pk,method='ht')$se
eD.ht_estHT_srd

## INTERPRETACJA Szacuje się, że wartości estymatora wartości średniej populacji w 7 domenie 
## odchylają się od jego wartości oczekiwanej przeciętnie o 3643,25

### Estymator p-średniego względnego błędu szacunku

egamma.ht_estHT_srd<-100*eD.ht_estHT_srd/estHT_srd
egamma.ht_estHT_srd

## INTEPRETACJA - szacuje się że ocena średniego względnego błędu 
## szacunku stanowi 59,76% wartości estymatora wartości średniej w domenie 7.


## Oszacowanie w. globalnej w domenie ----

## Estymator H-T

estHT_wgd<-N*htestimate(ysd,N,PI,pk,method='ht')$mean
estHT_wgd

## INTERPRETACJA Szacuje się że wartość globalna estymatora
## w 7 domenie wynosi 225582,1.

# wartość estymatora p-średniego błędu szacunku estymatora

eD.estHT_wgd<-N*htestimate(ysd,N,PI,pk,method='ht')$se
eD.estHT_wgd

## INTERPRETACJA Szacuje się, że wartości estymatora wartości globalnej populacji w 7 domenie 
## odchylają się od jego wartości oczekiwanej przeciętnie o 134800,42.


# wartość estymatora względnego p-średniego błędu szacunku estymatora

egamma.ht_estHT_wgd<-100*eD.estHT_wgd/estHT_wgd
egamma.ht_estHT_wgd

## INTEPRETACJA - szacuje się że ocena średniego względnego błędu 
## szacunku stanowi 59,75% wartości estymatora wartości globalnej w domenie 7.