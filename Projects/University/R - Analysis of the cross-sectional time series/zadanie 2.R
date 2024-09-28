
powiaty <- data.frame(powiaty)
i <- powiaty$id_i
d <- powiaty$id_d
t <- powiaty$id_t
x <- powiaty$x
y <- powiaty$y

library(nlme)
library(lme4)

# model mieszany z efektem losowym dla podpopulacji ----

m1_d_lmer <- lmer(y~x + (1|d), REML = T)
summary(m1_d_lmer)

## test ilorazu wiarygodności

# log. f. wiarygodności dla oryginalnego modelu -----

L1 <- logLik(lmer(y~x + (1|d), REML = T))

# log. f. wiarygodności dla modelu bez efektu losowego --- 

L2 <- logLik(lm(y~x))

# wartość statystyki testowej

stat_test <- -2 * (L2 - L1)
stat_test

# wartość p

p_wartosc <- 0.5 * pchisq(stat_test, df = 1, lower.tail = FALSE)
p_wartosc

# Macierz X -----

X <- cbind(1,x)

# macierz Z ----

Z <- as.matrix(getME(m1_d_lmer, name = "Z"))
Zt <- t(Z)
# Macierz G -----

e_sigma2_vd <- as.numeric(VarCorr(m1_d_lmer)$d)
e_sigma2_vd


eG <- diag(e_sigma2_vd, 6)

# Macierz R ----

e_sigma2_e <- sigma(m1_d_lmer)^2

eR <- diag(e_sigma2_e, 1256)

# macierz V ----

eV <- Z %*% eG %*% t(Z) + eR
dim(eV)

# Odwrotność macierzy V ----
inv_eV <- solve(eV)

# Macierz eBeta ----

eBeta <- getME(m1_d_lmer, "beta")

# macierz wektora efektów losowych ----

ev <- eG %*% Zt %*% inv_eV %*% (y - X %*% eBeta)

# wektor l ----

tl <- matrix(c(1,200000), ncol = 2)
dim(tl)

# wektor m ----

tm <- matrix(c(rep(0,3),1,rep(0,2)), ncol = 6)
m <- t(tm)

# Wartość predyktora ----

eTeta <- as.numeric(tl %*% eBeta + tm %*% ev)
eTeta
