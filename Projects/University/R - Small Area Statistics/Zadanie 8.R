set.seed(124)
library(sampling)
library(samplingbook)

load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/populacja.RData")
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/proba.RData")
dane_popul <- as.data.frame(dane_popul)
dane_proba <- as.data.frame(dane_proba)

attach(dane_popul)
attach(dane_proba)

n=35#liczebność próby
N=nrow(dane_popul)#liczebność populacji
Nd=sum(dane_popul$id==7) #liczebność 7 domeny

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

ysd<-ys
dane_probki<-dane_popul$id[id_s==1] ### Przynależność dla domen dla elementów wylosowanych w próbce
ysd[dane_probki!=7]=0  ### Zastępowanie wszystkich wartości w próbce oprócz 
# domeny 7 zerami !!!

#ESTYMATOR KALIBROWANY

Xs<-matrix(0,nrow=n, ncol=1)
Xs[,1]<-dane_popul$x[id_s==1]  ## Cechy dodatkowe (wartośći dla których identyf. próby = 1)
d<-1/pk
total<-c(sum(dane_popul$x))  # Suma 
g<-calib(Xs,d,total,method=c("linear"))

## oszacowanie w. średniej w domenie -----

estCAL_srd<-(1/Nd)*calibev(ysd,Xs,total,PI,d,g)$calest

eD2.ge_estCAL_srd<-(1/Nd^2)*calibev(ysd,Xs,total,PI,d,g)$evar
eD.ge_estCAL_srd<-sqrt(eD2.ge_estCAL_srd)
egamma.ge_estCAL_srd<-100*eD.ge_estCAL_srd/estCAL_srd

# wartość estymatora kalibrowanego wartości średniej w domenie 7.
estCAL_srd

## INTERPRETACJA
## Szacuje się że wartość średnia zmiennej y
## w domenie 7 wynosi 5727,821.

# wartość estymatora p-wariancji estymatora
eD2.ge_estCAL_srd
# wartość estymatora p-średniego błędu szacunku estymatora
eD.ge_estCAL_srd

## INTERPRETACJA
## Szacuje się że wartości estymatora kalibrowanego wartości średniej
## zmiennej y w domenie 7 odchylają się jego wartości oczekiwanej 
## przeciętnie o 2507,462.

# wartość estymatora względnego p-średniego błędu szacunku estymatora
egamma.ge_estCAL_srd

## INTERPRETACJA
## Ocena średniego błędu szacunku estymatora stanowi 43,77% wartości estymatora
## kalibrowanego wartości średniej zmiennej y w domenie 7.


## oszacowanie w. globalnej w domenie -----
estCAL_wgd<-calibev(ysd,Xs,total,PI,d,g)$calest

eD2.ge_estCAL_wgd<-calibev(ysd,Xs,total,PI,d,g)$evar
eD.ge_estCAL_wgd<-sqrt(eD2.ge_estCAL_wgd)
egamma.ge_estCAL_wgd<-100*eD.ge_estCAL_wgd/estCAL_wgd

# wartość estymatora kalibrowanego wartości globalnej w domenie 7
estCAL_wgd

## INTERPRETACJA
## Szacuje się że wartość globalna zmiennej y
## w domenie 7 wynosi 211929,4.

# wartość estymatora p-wariancji estymatora
eD2.ge_estCAL_wgd


# wartość estymatora p-średniego błędu szacunku estymatora kalibrowanego 
# wartości globalnej w domenie 7.
eD.ge_estCAL_wgd

## INTERPRETACJA
## Szacuje się że wartości estymatora wartości globalnej
## zmiennej y w domenie 7 odchylają się jego wartości oczekiwanej 
## przeciętnie o 92776,08. 

# wartość estymatora względnego p-średniego błędu szacunku estymatora
egamma.ge_estCAL_wgd

## INTERPRETACJA
## Ocena średniego błędu szacunku estymatora stanowi 43,77% wartości estymatora
## kalibrowanego wartości globalnej zmiennej y w domenie 7.


