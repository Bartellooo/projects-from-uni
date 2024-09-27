
library(sampling)
library(samplingbook)
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/populacja.RData")
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/proba.RData")
dane_popul <- as.data.frame(dane_popul)
dane_proba <- as.data.frame(dane_proba)

attach(dane_popul)
attach(dane_proba)

#próba
set.seed(111)    
s <- sample 
dane_s<- dane_popul[s == 1,]
# liczebności domen
Nd <-as.matrix(table(dane_popul$id))
Nd
n = 35
N = nrow(dane_popul)

#prawdopodobieństwa inkluzji I rzędu
pi <- rep(n/N,n)
#prawdopodobieństwa inkluzji II rzędu
pi2 <- matrix((n * (n - 1)) / (N * (N - 1)), ncol = n, nrow = n)
diag(pi2) <- n/N

# wart. globalna cechy dodatkowej w populacji
xwg_teta_pop <- sum(dane_popul$x)

# estymator bezpośredni y
ywg_s <- N*htestimate(dane_s$y,N = N,pk = pi,PI = pi2,method = 'ht')$mean
# estymator bezpośredni x
xwg_s <- N*htestimate(dane_s$x,N = N,pk = pi, PI = pi2,method = 'ht')$mean

# estymator ilorazowy w globalnej w populacji
ywg_ratio <- (ywg_s/xwg_s)*xwg_teta_pop

# ocena precyzji estymacji
b <- ywg_s / xwg_s
e <- dane_s$y -  dane_s$x * b

## WAŻNE DO DALSZEGO SZACOWANIA !!!! ----
# estymator wariancji estymatora ilorazowego w. globalnej w populacji
eD2_ywg_ratio <- varHT(y = e,pikl = pi2,method = 1)
# ocena średniego błędu szacunku
### eD_ywg_ratio <- sqrt(eD2_ywg_ratio)
# ocena średniego względnego błędu szacunku
### e_gamma_ywg_ratio<-eD_ywg_ratio / ywg_ratio

# znane wartości w globalnej cechy dodatkowej w podpopulacjach
xwg_teta_d <-c(
  sum(dane_popul$x[dane_popul$id==1]),
  sum(dane_popul$x[dane_popul$id==2]),
  sum(dane_popul$x[dane_popul$id==3]),
  sum(dane_popul$x[dane_popul$id==4]),
  sum(dane_popul$x[dane_popul$id==5]),
  sum(dane_popul$x[dane_popul$id==6]),  
  sum(dane_popul$x[dane_popul$id==7]),  
  sum(dane_popul$x[dane_popul$id==8]),  
  sum(dane_popul$x[dane_popul$id==9]),  
  sum(dane_popul$x[dane_popul$id==10]),
  sum(dane_popul$x[dane_popul$id==11]),
  sum(dane_popul$x[dane_popul$id==12]),
  sum(dane_popul$x[dane_popul$id==13]),
  sum(dane_popul$x[dane_popul$id==14]),
  sum(dane_popul$x[dane_popul$id==15]),
  sum(dane_popul$x[dane_popul$id==16]))

# syntetyczny estymator ilorazowy
EstSynRatio <- function(y_est, x_est, x_teta_d) {
  (y_est / x_est) * x_teta_d
}

D <- length(Nd) # liczba domen
est_ywg_sratio_d <- numeric(D)
eD_yeg_sratio_d <- numeric(D)

#### 1. Szacowanie wartości globalnej w domenie -------

# wartości syntetycznego estymatora ilorazowego w podpopulacjach
for (d in 1:D) {
  est_ywg_sratio_d[d] <- EstSynRatio(ywg_s, xwg_s, xwg_teta_d[d])
}

## Wartość globalna syntetycznego estymatora ilorazowego dla domeny 7.
est_ywg_sratio_d[7]

### INTEPRETACJA DLA DOMENY 7
### Szacuje się, że wartość globalna zmiennej y dla domeny 7 wynosi 135875,4.

# ocena wariancji syntetycznego estymatora ilorazowego wartości globalnej w podpopulacjach
eD2_ywg_sratio_d <- (xwg_teta_d/xwg_teta_pop)^2*eD2_ywg_ratio

# ocena średniego błędu szacunku syntetycznego estymatora ilorazowego wartości globalnej 
# w podpopulacjach (domenach)
eD_ywg_sratio_d <- sqrt(eD2_ywg_sratio_d)
eD_ywg_sratio_d[7]

### INTEPRETACJA DLA DOMENY 7
### Szacuje się, że wartośći estymatora wartości globalnej zmiennej y
### dla domeny 7 odchylają się od jego wartości oczekiwanej
### przeciętnie o 16748,91.

# ocena średniego względnego błędu szacunku syntetycznego estymatora ilorazowego w podpopulacjach
egamma_ywg_sratio_d<-eD_ywg_sratio_d/est_ywg_sratio_d
egamma_ywg_sratio_d[7]

### INTERPRETACJA
### Ocena średniego błędu szacunku stanowi 12,33% wartości estymatora
### wartości globalnej zmiennej y w domenie 7.


--------------------------------------------------------------
#### 2. Szacowanie wartości średniej w domenie -------

# średnia cechy dodatkowej w populacji
xsr_teta_pop <- mean(dane_popul$x)

# estymator bezpośredni y
ysr_s <- htestimate(dane_s$y,N = N,pk = pi,PI = pi2,method = 'ht')$mean
# estymator bezpośredni x
xsr_s <- htestimate(dane_s$x,N = N,pk = pi, PI = pi2,method = 'ht')$mean

# estymator ilorazowy wartości średniej w populacji
ysr_ratio <- (ysr_s/xsr_s)*xsr_teta_pop

# ocena precyzji estymacji
b <- ysr_s / xsr_s
e <- dane_s$y -  dane_s$x * b

## WAŻNE DO DALSZEGO SZACOWANIA !!!! ----
# estymator wariancji estymatora ilorazowego średniej w populacji 
eD2_ysr_ratio <- varHT(y = e,pikl = pi2,method = 1) / N ^ 2
# ocena średniego błędu szacunku
### eD_ysr_ratio <- sqrt(eD2_ysr_ratio)
# ocena średniego względnego błędu szacunku
### e_gamma_ysr_ratio<-eD_ysr_ratio / ysr_ratio 

# znane wartości średniej cechy dodatkowej w podpopulacjach
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

# syntetyczny estymator ilorazowy
EstSynRatio <- function(y_est, x_est, x_teta_d) {
  (y_est / x_est) * x_teta_d
}

D <- length(Nd) # liczba domen
est_ysr_sratio_d <- numeric(D)
eD_ysr_sratio_d <- numeric(D)

# wartości syntetycznego estymatora ilorazowego w podpopulacjach
for (d in 1:D) {
  est_ysr_sratio_d[d] <- EstSynRatio(ysr_s, xsr_s, xsr_teta_d[d])
}

# wartość syntetycznego estymatora ilorazowego wartości średniej w podpopulacjach (domenach)
## dla domeny 7
est_ysr_sratio_d[7]

### INTEPRETACJA DLA DOMENY 7
### Szacuje się, że wartość średnia zmiennej y dla domeny 7 wynosi 3672,309.


# ocena wariancji syntetycznego estymatora ilorazowego wartości średniejw podpopulacjach
eD2_ysr_sratio_d <- (xsr_teta_d/xsr_teta_pop)^2*eD2_ysr_ratio

# ocena średniego błędu szacunku syntetycznego estymatora ilorazowego w podpopulacjach
eD_ysr_sratio_d <- sqrt(eD2_ysr_sratio_d)
## dla domeny 7
eD_ysr_sratio_d[7]

### INTEPRETACJA DLA DOMENY 7
### Szacuje się, że wartośći estymatora wartości średniej zmiennej y
### dla domeny 7 odchylają się od jego wartości oczekiwanej
### przeciętnie o 452,6732.

# ocena średniego względnego błędu szacunku syntetycznego estymatora ilorazowego wartości średniej w podpopulacjach
egamma_ysr_sratio_d<-eD_ysr_sratio_d/est_ysr_sratio_d
## dla domeny 7
egamma_ysr_sratio_d[7]

### INTERPRETACJA
### Ocena średniego błędu szacunku stanowi 12,33% wartości estymatora
### wartości średniej zmiennej y w domenie 7.
