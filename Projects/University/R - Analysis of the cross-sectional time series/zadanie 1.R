
## Dane

powiaty <- data.frame(powiaty)

i <- powiaty$id_i
d <- powiaty$id_d
t <- powiaty$id_t
x <- powiaty$x
y <- powiaty$y

## Model z dwoma efektami losowymi dla podpopulacji oraz dla profili z jedną zmienną objaśniającą i stałą.

library(nlme)
library(lme4)

m1_id_lmer <- lmer(y~x + (1|d) + (1|i), REML = T)
summary(m1_id_lmer)

AIC(m1_id_lmer)

## Model mieszany z losowym parametrem kierunkowym z efektem losowym dla czasu

m2_t_lmer <- lmer(y~x-1 + (0+x|t), REML = T)
summary(m2_t_lmer)

AIC(m2_t_lmer)
