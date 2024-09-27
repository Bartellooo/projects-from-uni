

library(sampling)
library(samplingbook)
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/populacja.RData")
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/proba.RData")
dane_popul <- as.data.frame(dane_popul)
dane_proba <- as.data.frame(dane_proba)

attach(dane_popul)
attach(dane_proba)

n=35
N=nrow(dane_popul)

#próba
set.seed(111)    
s <- sample
dane_s<- dane_popul[s == 1,]
# liczebności domen
Nd <-as.matrix(table(dane_popul$id))
Nd

#prawdopodobieństwa inkluzji I rzędu
pi <- rep(n/N,n)
#prawdopodobieństwa inkluzji II rzędu
pi2 <- matrix((n * (n - 1)) / (N * (N - 1)), ncol = n, nrow = n)
diag(pi2) <- rep(n/N,n)


# wart. średnia cechy dodatkowej w populacji
xsr_teta_pop <- mean(dane_popul$x)

# estymator bezpośredni y
ysr_s <- htestimate(dane_s$y, N = N,  pk =pi, PI = pi2, method = 'ht')$mean
# estymator bezpośredni x
xsr_s <- htestimate(dane_s$x, N = N, pk = pi, PI = pi2, method = 'ht')$mean

model <- lm(dane_s$y ~ dane_s$x, weights = pi ^ (-1))
ebeta <- as.numeric(model$coefficients[2])

# ocena precyzji estymacji
b2 <- ebeta 
b1 <- ysr_s - b2 * xsr_s
e <- dane_s$y -  dane_s$x * b2 - b1

-------
### UWAGA -> WAŻNE DO WYZNACZANIA ESTYMATORÓW W DOMENACH !!!!!! -----
# estymator wariancji estymatora regresyjnego średniej w populacji
eD2_ysr_reg<- varHT(y = e,pikl = pi2, method = 1) / N ^ 2
# estymator wariancji estymatora regresyjnego wartości globalnej w populacji
eD2_ywg_reg <- varHT(y = e,pikl = pi2, method = 1) 

-------

# wektor znanych wartości średniej cechy dodatkowej w podpopulacjach
xsr_teta_d <-c(
  mean(dane_popul$x[dane_popul$id==1]),
  mean(dane_popul$x[dane_popul$id==2]),
  mean(dane_popul$x[dane_popul$id==3]),
  mean(dane_popul$x[dane_popul$id==4]),
  mean(dane_popul$x[dane_popul$id==5]),
  mean(dane_popul$x[dane_popul$id==6]),
  mean(dane_popul$x[dane_popul$id==7]),
  mean(dane_popul$x[dane_popul$id==8]),
  mean(dane_popul$x[dane_popul$id==9]),
  mean(dane_popul$x[dane_popul$id==10]),
  mean(dane_popul$x[dane_popul$id==11]),
  mean(dane_popul$x[dane_popul$id==12]),
  mean(dane_popul$x[dane_popul$id==13]),
  mean(dane_popul$x[dane_popul$id==14]),
  mean(dane_popul$x[dane_popul$id==15]),
  mean(dane_popul$x[dane_popul$id==16]))

# syntetyczny estymator regresyjny
EstSynreg<- function(y_est, x_est, x_teta_d, ebeta) {
  y_est + ebeta * (x_teta_d -  x_est)
}



#### 1. SZACOWANIE WARTOŚCI ŚREDNIEJ W DOMENIE ----


# wartości syntetycznego estymatora regresyjnego wartości średniej w podpopulacjach
ysr_sreg_d<- EstSynreg(ysr_s, xsr_s, xsr_teta_d, ebeta)
ysr_sreg_d[Nd=7]

###  INTERPRETACJA: Szacuje się, że wartość średnia y dla elementów z domeny 7
## wynosi 3962,883.



# ocena wariancji syntetycznego estymatora regresyjnego wartości średniej w podpopulacjach
eD2_ysr_sreg_d<- (xsr_teta_d / as.numeric(xsr_teta_pop)) ^ 2 * eD2_ysr_reg



# ocena średniego błędu szacunku syntetycznego estymatora regresyjnego wartości średniej w podpopulacjach
eD_ysr_sreg_d<- sqrt(eD2_ysr_sreg_d)
eD_ysr_sreg_d[Nd=7]

### INTEPRETACJA 
### Szacuje się, że wartości estymatora wartośći średniej zmiennej y
### w domenie 7 odchylają się od jego wartości oczekiwanej
### przeciętnie o 423,2716.

# ocena średniego względnego błędu szacunku syntetycznego estymatora regresyjnego wartości średniej w podpopulacjach
egamma_ysr_sreg_d<-eD_ysr_sreg_d/ysr_sreg_d
egamma_ysr_sreg_d[Nd=7]

### INTERPRETACJA
### Ocena średniego błędu szacunku stanowi 10,68% wartości estymatora
### wartości średniej y w domenie 7.


#### 2. SZACOWANIE WARTOŚCI GLOBALNEJ W DOMENIE ----

# wartości syntetycznego estymatora regresyjnego w podpopulacjach
ywg_sreg_d<- Nd*EstSynreg(ysr_s, xsr_s, xsr_teta_d, ebeta)
ywg_sreg_d[Nd=7]

###  INTERPRETACJA: Szacuje się, że wartość średnia y dla elementów z domeny 7
## wynosi 146626,68


# ocena wariancji syntetycznego estymatora regresyjnego w podpopulacjach
eD2_ywg_sreg_d<- (Nd / N) ^ 2 * eD2_ywg_reg

# ocena średniego błędu szacunku syntetycznego estymatora re w podpopulacjach
eD_ywg_sreg_d<- sqrt(eD2_ywg_sreg_d)
eD_ywg_sreg_d[Nd=7]

### INTEPRETACJA 
### Szacuje się, że wartości estymatora wartośći globalnej zmiennej y
### w domenie 7 odchylają się od jego wartości oczekiwanej
### przeciętnie o 15848,8.

# ocena średniego względnego błędu szacunku syntetycznego estymatora regresyjnego w podpopulacjach
egamma_ywg_sreg_d<-eD_ywg_sreg_d/ywg_sreg_d
egamma_ywg_sreg_d[Nd=7]

### INTERPRETACJA
### Ocena średniego błędu szacunku stanowi 10,81% wartości estymatora
### wartości globalnej y w domenie 7.
