## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 4: Variables Aleatorias"

######################################################################
# Variables aleatorias discretas.
######################################################################


muestra = sample(0:3, size = 10, replace = TRUE, prob = c(64, 48, 12, 1))

library(viridisLite)
muestra = sample(0:3, size = 1000, replace = TRUE, prob = c(64, 48, 12, 1))
barplot(table(muestra), col=viridis(4))



## Variable aleatoria binomial.
library(tidyverse)
fhs = read_csv("./data/framingham.csv")
tablaHyp = prop.table(table(fhs$prevalentHyp))
p = unname(tablaHyp[2])

set.seed(2019)
n = 7
N = 50000
X = replicate(N, {
  pacientes = sample(fhs$prevalentHyp, n, replace = TRUE)
  (exitos = (pacientes == 1))
  sum(exitos)
})
prop.table(table(X))
dbinom(x = 0:n, size = n, prob = p)

## La binomial con R.
dbinom(x = 3, size = 7, prob = p)

signif(dbinom(x = 0:7, size = 7, prob = p), digits = 3)

signif(pbinom(q = 0:7, size = 7, prob = p), digits = 3)

rbinom(n = 25, size = 7, prob = p)

## Representación gráfica de la variable binomial.
probabilidades = dbinom(x = 0:7, size = 7, prob = p)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:7)
arrows(seq(0.5, 7.5, by = 1), 0, seq(0.5, 7.5, by = 1), prop.table(table(X)), col="red", lwd = 2)

## El zoo de las binomiales.
probabilidades = dbinom(x = 0:12, size = 10, prob = 2/3)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:12)

## Binomiales con $n$ grande y $p$ moderado.
probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
bp = barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)

## Cálculos de probabilidad en binomiales con $n$ muy grande.
probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)
par(new = T)
probabilidades[0:25] = 0
probabilidades[36:100] = 0
barplot(probabilidades/4, space = 0, col="blue", names.arg = 0:100)

## Otra vez la discusión "*discreto frente a continuo*".
probabilidades = dbinom(x = 0:100, size = 100, prob = 1/3)
barplot(probabilidades, space = 0, col="tan", names.arg = 0:100)
par(new = T)
probabilidades[0:25] = 0
probabilidades[36:100] = 0
barplot(probabilidades/4, space = 0, col="blue", names.arg = 0:100)

######################################################################
# Variables aleatorias continuas.
######################################################################







## Distribuciones normales en R.
pnorm(10.5, mean=10, sd=2)

  1 - pnorm(11, mean=10, sd=2)
  pnorm(11, mean = 10, sd = 2, lower.tail = FALSE)



## Probabilidad de un intervalo con `pnorm`.
pnorm(12, mean=10, sd=2) - pnorm(7, mean=10, sd=2)


## Problema inverso de probabilidad. La función `qnorm`.
qnorm(p = 1/3, mean = 10, sd=2)




# La función rnorm
set.seed(2019)
x1 = rnorm(1000)
y1 = rnorm(1000)
ggplot(data.frame(x1, y1)) +
  geom_point(mapping = aes(x1, y1), col="red")

x2 = runif(1000, min = -1, max = 1)
y2 = runif(1000, min = -1, max = 1)
ggplot(data.frame(x2, y2)) +
  geom_point(mapping = aes(x2, y2), col="blue")



## Suma (y mezcla) de normales independientes.
set.seed(2019)
pob1 = rnorm(30000, mean = -3, sd = 1)
pob2 = rnorm(30000, mean = 2, sd = 0.5)
pobSuma = 3 * pob1 + 4 * pob2
plot(density(pobSuma, adjust = 1.6), main="", lwd=5, col="red", xlab="")
