set.seed(124)

library(sampling)
library(samplingbook)
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/populacja.RData")
load("C:/Users/barte/Desktop/Studia/Magisterka 2 rok/Statystyka Małych Obszarów/pod kolosa/proba.RData")
dane_popul <- as.data.frame(dane_popul)
dane_proba <- as.data.frame(dane_proba)

n <- 35  # Liczebność próby
N <- nrow(dane_popul)  # Liczebność populacji
Nd <- sum(dane_popul$id == 7)

# Wybór próby
s <- sample
id_s <- as.numeric(s)
ys <- dane_popul$y[id_s == 1]

# Prawdopodobieństwa inkluzji
pk <- rep(n / N, n)  # Pierwszego rzędu
PI <- matrix((n * (n - 1)) / (N * (N - 1)), ncol = n, nrow = n)  # Drugiego rzędu
diag(PI) <- pk

dane_probki <- dane_popul$id[id_s == 1]  # Przynależność dla domen dla elementów wylosowanych w próbce

# Zmienne dodatkowe - próba
Xs <- matrix(0, nrow = 1, ncol = n)
Xs[1, ] <- dane_popul$x[id_s == 1]

# Wartości globalne populacja i domena (z całej populacji)
total <- sum(dane_popul$x)
totald <- sum(dane_popul$x[dane_popul$id == 7])

# Szacowanie wartości globalnej w domenie
# ESTYMATOR MGREG ----
MGREG <- function(ys, Xs, dane_probki, domena, totald, PI, q = rep(1, length(Xs))) {
  pk <- diag(PI)
  d <- 1 / pk  ## <- Wagi
  
  ysd <- ys
  ysd[dane_probki != domena] = 0
  Xsd <- Xs
  Xsd[, dane_probki != domena] = 0
  
  g <- calib(t(Xs), d, total, method = c("linear"))
  
  skladnik1 <- sum(d * ysd)
  skladnik2 <- totald - sum(d * Xsd)
  
  suma1 <- sum(d * q * Xs^2)
  suma2 <- sum(d * q * Xs * ys)
  
  B <- suma2 / suma1
  estMGREG <- skladnik1 + skladnik2 * B
  
  pk <- matrix(pk, nrow = length(pk))
  P <- (pk %*% t(pk) - PI) / PI
  diag(P) <- 0
  
  r <- d * (ys - Xs * B)
  R1 <- matrix(r, ncol = n, nrow = n, byrow = TRUE)
  R1[, dane_probki != domena] = 0
  R2 <- (R1 - t(R1))^2
  estvar <- sum(P * R2) / 2
  
  list(est = estMGREG, evar = estvar)
}

domena <- 7
wyniki <- MGREG(ys, Xs, dane_probki, domena, totald, PI)

# Wyświetlenie wyników
print(wyniki)

## Szacowanie wartości globalnej w domenie ----
#estymator wartości globalnej w domenie
est_wg<-wyniki$est
est_wg

## INTERPRETACJA
## Szacuje się że wartość globalna zmiennej y
## w domenie 7 wynosi 208206,6

#ocena wariancji
eD2_wg<-wyniki$evar
#ocena średniego błędu szacunku
eD_wg<-sqrt(eD2_wg)
eD_wg

## INTERPRETACJA
## Szacuje się że wartości estymatora wartości globalnej 
## zmiennej y w domenie 7 odchylają się od jego wartości oczekiwanej 
## przeciętnie o 71800,68S


#ocena średniego względnego błędu szacunku
egamma_wg<-100*eD_wg/est_wg
egamma_wg

## INTERPRETACJA
## Ocena średniego błędu szacunku estymatora stanowi 34,48% wartości estymatora
## wartości globalnej zmiennej y w domenie 7.


## Szacowanie wartości średniej w domenie ----
#estymator wartości średniej w domenie
est_sr<-(wyniki$est)/Nd ### - Nd to liczebność domeny !!!
est_sr

## INTERPRETACJA
## Szacuje się że wartość średnia zmiennej y
## w domenie 7 wynosi 5627,206

#ocena wariancji
eD2_sr<-(wyniki$evar)/(Nd^2)
#ocena średniego błędu szacunku
eD_sr<-sqrt(eD2_sr)
eD_sr

## INTERPRETACJA
## Szacuje się że wartości estymatora wartośći średniej
## zmiennej y w domenie 7 odchylają się od jego wartości oczekiwanej 
## przeciętnie o 1940,559


#ocena średniego względnego błędu szacunku
egamma_sr<-100*eD_sr/est_sr
egamma_sr

## INTERPRETACJA
## Ocena średniego błędu szacunku estymatora stanowi 34,48% wartości estymatora
## wartości średniej zmiennej y w domenie 7.

