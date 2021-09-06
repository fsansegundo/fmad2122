## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 2: Tipos de Variables y Análisis Exploratorio"



# Ficheros csv con R, Lectura ficheros movies
movies = read.csv(file = "./data/movies.csv", header = TRUE, sep=",")

# Repaso de operaciones con data.frames.
movies[7, ]

tail(movies$Year, 20) # se muestran las 20 últimas

movies$Genre[movies$Year == 2010]

# Usando readr para leer y escribir ficheros csv.
library(tidyverse)
movies2 <- read_csv("./data/movies.csv")

# crear ficheros csv a partir de una tabla
set.seed(2019)
datos <-  
  data.frame(A = sample(1:100, 10), B = sample(LETTERS, 10), C = rnorm(10))
head(datos, 2)
write_csv(datos, file = "./data/sesion02-guardarCsv.csv")

# Ficheros Excel
library(readxl)
accidentes <- read_excel("./data/train_acc_2010.xls")

library(writexl)

getwd()
write_xlsx(iris, path = "./data/iris.xlsx")

file.remove("./data/iris.xlsx")


# Datos de enfermedades coronarias en Framingham
fhs = read_csv("./data/framingham.csv")

# variable discreta, tabla de frecuencia absoluta
table(mpg$cty)

mpg %>%
  count(cty)

# y tabla de frecuencia relativa
signif(prop.table(table(mpg$cty)), 2)

      mpg %>% 
        count(cty) %>%
          mutate(cty, relFreq = prop.table(n), n=NULL) 

# suma frecuencias relativas
sum(prop.table(table(mpg$cty)))

# Frecuencias acumuladas.
cumsum(table(mpg$cty))



# Error:  tabla de frecuencia de una v. continua 
table(fhs$totChol)

# Creando intervalos con cut
cholLevels = cut(fhs$totChol, breaks = 10)
head(cholLevels)



table(cholLevels)

# Histograma con ggplot2
cortes = seq(min(mpg$cty), max(mpg$cty), length.out = 11)
ggplot(data = mpg) + 
  geom_histogram(mapping = aes(cty), breaks = cortes, 
                 fill = "orange", color="black")

# Curvas de densidad con ggplot
ggplot(mpg) + 
  geom_density(mapping = aes(cty), color="red", fill="lightblue", size=1.5)

# Relación entre curvas de densidad e histogramas.
ggplot(mpg, aes(x = cty)) + 
  geom_histogram(aes(y=stat(density)), 
                 breaks = cortes, fill = "orange", color="black")  + 
  geom_density(color="red", size=1.5)

# Curvas de densidad de auto2, p. 4
require(gridExtra)
require(tidyverse)
require(haven)
auto2 = read_dta("./data/auto2.dta")
p1 = ggplot(auto2) + 
  geom_density(mapping = aes(price), color="red", fill="lightblue", size=1.5)

p2 = ggplot(auto2) + 
  geom_density(mapping = aes(trunk), color="red", fill="lightblue", size=1.5)

p3 = ggplot(auto2) + 
  geom_density(mapping = aes(weight), color="red", fill="lightblue", size=1.5)

p4 = ggplot(auto2) + 
  geom_density(mapping = aes(gear_ratio), color="red", fill="lightblue", size=1.5)

grid.arrange(p1, p2, p3, p4, nrow = 2)

# Distribución unimodal p. 5
curve(dchisq(x, df=6), from = 0, to = 16, ylab="density", col="red", lwd=4, main="")

# Distribución bimodal p. 5
set.seed(2014)
N = 10000
mu1 = 12
sigma1 = 0.7
mu2 = 14
sigma2 = 0.5
n1 = rnorm(N, mean=mu1, sd=sigma1)
n2 = rnorm(N, mean=mu2, sd=sigma2)
# mezcla de normales
mezcla = c (n1,  n2)
plot(density(mezcla), col="red", lwd=4, main="")

# Ilustrando el concepto de skewness p.5
par(mfrow = c(3, 1))
curve(dchisq(x, df=6), from = 0, to = 16, 
      ylab="density", col="blue", lwd=4, main="Asimétrica a derecha", cex.main=2)  
curve(dnorm, from = -3, to = 3, 
      ylab="density", col="black", lwd=4, main="Simétrica", cex.main=2)  
curve(dchisq(15-x, df=6), from = 0, to = 16, 
      ylab="density", col="red", lwd=4, main="Asimétrica a izquierda", cex.main=2)  
par(mfrow = c(1, 1))



# Media aritmética, p. 9
set.seed(2019) # no ejecutes esta línea si quieres ver otras muestras
(muestra = sample(0:100, size = 20, replace = TRUE))
(media = mean(muestra))

# Valores atípicos, p. 10
set.seed(2019)
muestra = sample(0:100, size = 99, replace = TRUE) 
(media = mean(muestra))

muestra2 = c(muestra, 1000)
(media2 = mean(muestra2))

# Mediana, p. 11
set.seed(2019)
(valores = sample(1:100, 17, replace = TRUE))

(ordenados = sort(valores))
(mediana = ordenados[9])

median(valores)

median(muestra)
median(muestra2)

# Valores de posición, p. 13
quantile(mpg$cty, probs = 0.43)

# Summary, p. 13
summary(mpg$cty)

# El gráfico para ilustrar la idea de dispersión
library(tidyverse)
set.seed(2019)
n = 400
library(MASS)
muestra1 = mvrnorm(n, mu = 0, Sigma = 1, empirical = TRUE)
min1 = min(muestra1)
max1 = max(muestra1)
muestra1 = c(muestra1[-(1:10)], runif(10, 2 * min1, 2 * max1))
muestra2 = runif(n, 2 * min1, 2 * max1)
muestras = 
  data.frame(x = c(muestra1, muestra2), tipo = gl(2, n))

ggplot(data = muestras) +
  geom_point(mapping = aes(tipo, y = x, color = tipo), 
             position =  position_jitter(w = 0.05, h = 0)) + 
  coord_flip()

# y la versión en curvas de densidad de la siguiente página
plot(density(muestra1, adjust = 2), col="red", 
     xlim = c(-12, 12), lwd = 3, main = "", xlab= "")
lines(density(muestra2, adjust = 2), col="blue", lwd = 3, sub="")

# Recorrido intercuartílico, p. 16
IQR(mpg$cty)

summary(mpg$cty)

unname(quantile(mpg$cty, probs = c(1/4, 3/4)) + c(-1, 1) * 1.5 * IQR(mpg$cty))



# Boxplot con R básico p. 19
bxp_cty = boxplot(mpg$cty, col="orange")

bxp_cty$out

# Boxplot y violinplot con R básico p. 20
ggplot(mpg) + 
  geom_boxplot(mapping = aes(y = cty), fill="orange")

ggplot(mpg) + 
  geom_violin(mapping = aes(x=0, y = cty)) +
  scale_x_discrete(breaks = c()) +
  geom_boxplot(mapping = aes(y = cty), fill="green") +
  geom_jitter(aes(x=0, y = cty), 
              position = position_jitter(w=0.05, h= 0), col="blue")

# Desviación absoluta mediana p. 21
library(readxl)
accidentes = read_excel("./data/train_acc_2010.xls")
mad(accidentes$Speed, constant = 1)

# Varianza y desviación típica p. 24
var(mpg$displ)

n = length(mpg$displ)
media = mean(mpg$displ)
sum((mpg$displ - media)^2) / (n - 1 )

sd(mpg$displ)

sqrt(var(mpg$displ))

# Factores p. 26
table(accidentes$TrkType)
prop.table(table(accidentes$TrkType))

# Función factor
(ardeida = factor(c("martinete", "garzaReal", "avetorillo", "garzaReal",
                    "cangrejera", "martinete", "martinete"), ))

(ardeida = factor(c("martinete", "garzaReal", "avetorillo", "garzaReal",
                  "cangrejera", "martinete", "martinete"), 
levels = c("garzaReal", "martinete",  "cangrejera", "avetorillo")))  

## Más funciones que generan factores
options(width = 70)
gl(n = 3, k = 4, labels = c("piedra", "papel", "tijera"))

gl(n = 3, k=1, length = 30, labels = c("piedra", "papel", "tijera"))

# Gráficos para factores
library(viridisLite)
ggplot(accidentes) + 
  geom_bar(mapping = aes(x = TrkType), fill= viridis(5))

# Factores dicotómicos
fhs = read_csv("./data/framingham.csv")
mean(fhs$male)
