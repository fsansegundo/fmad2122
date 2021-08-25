## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 3: Probabilidad"

######################################################################
# Población y muestra. 
######################################################################




## Poblaciones y muestras aleatorias simples con vectores usando R.
set.seed(2019)
N = 158000
poblacion = as.integer(2 * rchisq(N, df = 13), 0)

require(tidyverse)
poblacion %>% 
  as_tibble %>% 
ggplot() + 
  geom_histogram(aes(x = value), bins = 15, fill="orange", color="black") + 
  geom_vline(xintercept = mean(poblacion), 
             col="blue", linetype="dashed", size=2)
# hist(poblacion, main="", col="orange")
# abline(v = mean(poblacion), lty=2, lwd=5, col="blue")

## Medias muestrales
options(width= 90)
n = 20
(muestra = sample(poblacion, n, replace = TRUE))
options(width= 70)

options(width= 90)
(muestra2 = sample(poblacion, n, replace = TRUE))
mean(muestra2)

## Muestras buenas y malas.
options(width= 90)
(muestra3 = sort(poblacion)[1:20])

options(width= 90)
mean(muestra3)

## La distribución de las medias muestrales.
k = 10000
# replicate repite k veces los comandos entre llaves y guarda el resultado
# del último comando en el vector mediasMuestrales
mediasMuestrales = replicate(k, { 
  muestra = sample(poblacion, n, replace = TRUE)
  mean(muestra)
})
head(mediasMuestrales, 10)

# Representación gráfica de la distribución de las medias muestrales
hist(mediasMuestrales, breaks = 40, main="", 
     col="peachpuff", probability = TRUE, xlim=range(poblacion))
lines(density(mediasMuestrales), lwd=4, col="red")
lines(density(poblacion), lwd=4, col="blue")
abline(v = mean(poblacion), lty=2, lwd=5, col="blue")

poblacion = sample(0:20, 20000, replace = TRUE)

## Otra población, mismos resultados.
k = 10000
mediasMuestrales = replicate(k, { 
  muestra = sample(poblacion, n, replace = TRUE)
  mean(muestra)
})



#####################################################################
# Otras poblaciones por si quieres experimentar
#####################################################################  
# Uniforme continua


tamPoblacion = 100000
poblacion = runif(tamPoblacion, min = 0, max = 10)
head(poblacion, 100)


hist(poblacion)
plot(density(poblacion))

mean(poblacion)

Tmuestra = 20

# ¿Cuántas muestras distintas hay?
choose(tamPoblacion, Tmuestra)

# La población es moderadamente grande, pero el espacio de muestras es enorme.

# Vamos a tomar muchas muestras y en cada una calculamos una media muestral.
numMuestras = 100000

# Repetiremos esto varias veces para hacernos una idea.
(muestra = sample(poblacion, size = Tmuestra, replace = TRUE))
mean(muestra)

mediasMuestrales = replicate(numMuestras, {
  muestra = sample(poblacion, size = Tmuestra, replace = TRUE)
  mean(muestra)
})

# ¿Cómo se distibuyen esas medias muestrales?
head(mediasMuestrales)
hist(mediasMuestrales)
hist(mediasMuestrales, breaks = 40, main="")
plot(density(mediasMuestrales, adjust = 1.5), main="")

# ¿Cuál es la media de las me?
(mu = mean(poblacion))
mean(mediasMuestrales)

# ¿Cuál es su desviación típica?
(desvTipPob = sqrt(sum((poblacion - mu)^2) / tamPoblacion))

sd(mediasMuestrales)
desvTipPob / sqrt(Tmuestra)

######################################################################
# Probabilidad básica.
######################################################################



## La paradoja del cumpleaños.
n = 366 # Número de personas en la sala

# Vamos a repetir el experimento N veces (N salas de n personas)
N = 10000
pruebas = replicate(N, {
fechas = sort(sample(1:366, n, replace=TRUE)) 
max(table(fechas)) # si el máximo es mayor que 1 es que 2 fechas coinciden
})
mean(pruebas > 1) # ¿qué proporción de salas tienen coincidencias?



# Librería spam de Kernlab y Teorema de Bayes
library(kernlab)
data(spam)
spam[1:4, c(1:10, 58)]

## Tablas de contingencia 2x2
library(tidyverse)
spam = spam %>%
  select(order, type) %>% 
  mutate(hasOrder = factor(order > 0, # Creamos el factor hasOrder
                            levels = c(TRUE, FALSE), 
                            labels = c("order", "no order")),
         type = relevel(type, ref = "spam"), # Reordenamos los niveles
         -order) # y elminamos el factor order original

table(spam$hasOrder, spam$type)




download.file(lnkScriptGitHub, destfile = paste0(getwd(),"/", ScriptName))
