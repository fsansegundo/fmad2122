## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 5. Introducción a la Inferencia Estadística."

## El error es aleatorio porque la muestra es aleatoria.
  
par(mar = c(5.1,4.1,1,2.1)) 
set.seed(2019)
library(tidyverse)
# generamos los datos, 20 muestras (se distinguen por su "tipo")
datos = data.frame(x = rnorm(600), tipo = rep(1:20, each = 30))
# calculamos las medias por muestra con dplyr
medias = datos %>%
  group_by(tipo) %>%
  summarise(medias = mean(x)) %>%
  .$medias 
# Usamos stripchart para dibujar las medias por tipo  
stripchart(x ~ tipo, data = datos, pch = 16, col="blue",
           xlab="Valores de la variable X", ylab = "Número de muestra")
# Añadimos líneas horizontales para ayudar a visualizar
segments(x0 = min(datos$x), y0 = 1:20, x1 = max(datos$x), y1 = 1:20,  col="blue")
# La línea vertical central marca la media
abline(v = 0, lty=2, col= "red", lwd=5)
# Y los puntos naranjas son las medias muestrales
points(x = medias, y = 1:20, col="orange", pch=18, cex=3)

# Código para generar la figura de los intervalos de confianza
par(mar = c(5.1,4.1,1,2.1)) 
set.seed(2018)
library(tidyverse)
# Esta función calcula un intervalo de confianza para cada muestra
# Pronto veremos como funciona
getCI = function(x){
  CI = t.test(x, alternative = "two.sided", mu = 0)$conf.int
  return(CI)
}
# Generamos las muestras
datos = matrix(rt(3000, df = 29), nrow = 100)
# y los correspondientes intervalos
intervalos = t(apply(datos, MARGIN = 1, FUN = getCI))
# los colores dependen de que el intervalo capture la media poblacional
# que en nuestro caso es 0
colores = ifelse(intervalos[,1] * intervalos[,2] < 0, "blue", "red")
# Ahora pintamos los extremos de los intervalos
plot(c(intervalos), rep(1:100,times = 2), col=rep(colores, 2), 
     xlab = "La línea de puntos indica la media poblacional real", ylab="")
# Los segmentos que los conectan
segments(x0 = intervalos[,1], 
         y0 = 1:100, 
         x1 = intervalos[ ,2], 
         y1 = 1:100,
         col=colores)
# Y una línea vertical en la media poblacional
abline(v = 0, lty=2, col= "black", lwd=5)

## Valores críticos con R.
nc = 0.95
alfa = 1 - nc
(zc = qnorm(alfa / 2, lower.tail = FALSE)) # Atención, cola derecha

## Intervalos de confianza con R.
n = 100
barX = 7.34
s = 0.31
(intervalo = barX + c(-1, 1) * zc * s / sqrt(n))

# La distribución t vs Z

k = 1000
xvals = seq(-5, 5, length = k) 
d = data.frame(y = c(dnorm(xvals), dt(xvals, df = 2)), 
                x = xvals,
                dist = factor(rep(c("Normal", "T"), c(k,k)))) 
g = ggplot(d, aes(x = x, y = y))
g = g + geom_line(size = 2, aes(color = dist))
g 

## La distribución $t$ en R.

## Función pt

1 - pt(2.5, df = 17)

# Valores críticos con qt
n = 20
nc = 0.95
alfa = 1 - nc
df = n - 1
(tc = qt(alfa / 2, df, lower.tail = FALSE)) # Atención, cola derecha

# Valores aleatorios con rt
rt(8, df = 19)

# Intervalo de confianza con t
datos = c(0.04, 0.05, 0.03, 0.06, 0.04, 0.06, 0.07, 0.03, 0.06, 0.02)
n = length(datos)
barX = mean(datos)
s = sd(datos)
nc = 0.95
alfa = 1 - nc
tc = qt(1 - alfa/2, df = n - 1)
(intervalo = barX + c(-1, 1) * tc * s / sqrt(n))

## Intervalos de confianza por bootstrap.
# set.seed(2017)
# skewdata = rchisq(100, df = 3) + 5
# write.table(skewdata,file = "./data/skewdata.csv", row.names = FALSE)
url = "https://raw.githubusercontent.com/mbdfmad/fmad2122/main/data/skewdata.csv"
x = pull(read_csv(file = url), 1) 
x = tibble(x)
ggplot(x) + 
  geom_histogram(aes(x = x, y=stat(density)),
                 bins = 12, fill = "orange", color="black")  + 
  geom_density(aes(x = x), color="red", size=1.5) 

url = 
  "https://raw.githubusercontent.com/mbdfmad/fmad2122/main/data/skewdata.csv"
x = pull(read_csv(file = url), 1) 

# Creamos la "caja" del gráfico.
plot(c(0, 40), c(5,10.5), type="n", xlab="Tamaño muestral", ylab="") 

for (k in seq(5, 40, 1)){ # Este bucle recorre los tamaños muestrales
  a =  numeric(10000) # el vector a almacenará las medias muestrales
  for (i in 1:10000){ # este es el bucle de remuestreo (bootstrap)
  # generamos un remuestreo con reemp. y calculamos su media
    a[i] = mean(sample(x, k, replace=T)) 
    }
  # dibujo del intervalo bootstrap de este tamaño muestral  
  points(c(k,k), quantile(a, c(.025,.975)), type="o", 
         col = "orange", lwd= 3) 
}

# el siguiente bloque de código genera una banda con 
# los intervalos clásicos correspondientes a esas muestras.
xv = seq(5, 40, 0.1) 
yv = mean(x) - qt(0.975, xv) * sqrt(var(x) / xv)
lines(xv, yv, lty = 2, col = "black", lwd = 4)
yv = mean(x) + qt(.975, xv) * sqrt(var(x) / xv)
lines(xv, yv, lty = 2, col = "black", lwd = 4)

# añadimos una línea horizontal en la media
abline(h = mean(x), col="blue", lwd=2) 

## Chi cuadrado: intervalos de confianza para la varianza.

# Estos son los valores muestrales y el nc deseado
varianza = 62 # cuidado si el dato muestral es s y no s^2
n = 7
nc = 0.95
(alfa = 1 - nc)

# Calculamos dos valores críticos de chi cuadrado.
(chi1 = qchisq(alfa / 2, df = n - 1, lower.tail = FALSE)) # cola derecha
(chi2 = qchisq(alfa/2, df = n - 1)) # cola izquierda

# Construimos el intervalo
(intervalo = (n - 1) * varianza / c(chi1, chi2))

# Analizando gráficamente la normalidad de una población a partir de muestras

library(gridExtra)

# Primero con datos normales

set.seed(2017)
tamMuestra = 500
normales = scale(rnorm(tamMuestra))
p1 = ggplot(tibble(x = normales)) + 
geom_histogram(aes(x = x, y=stat(density)),
               bins = 12, fill = "orange", color="black")  + 
geom_density(aes(x = x), color="red", size=1.5) + 
  ggtitle("Datos normales") +
  xlab("") + 
  ylab("Densidad")

# Y ahora con datos no normales

set.seed(2017)
tamMuestra = 500
noNormales = scale(rchisq(tamMuestra, df = 4))
p2 = ggplot(tibble(x = noNormales)) + 
geom_histogram(aes(x = x, y=stat(density)),
               bins = 12, fill = "orange", color="black")  + 
geom_density(aes(x = x), color="red", size=1.5)  + 
ggtitle("Datos no normales") +
xlab("") + 
ylab("Densidad")

grid.arrange(p1, p2, nrow = 1)

# Boxplots para analizar la simetría.
muestras = tibble(x = c(scale(normales), scale(noNormales)),
                  tipo = gl(n = 2, k = 500, labels = c("normal", "nonnormal")) )
ggplot(muestras) + 
  geom_boxplot(aes(x = tipo, y = x, col = tipo)) + 
  geom_jitter(aes(x = tipo, y = x, color = tipo), width = 0.05,  alpha = 0.2) + 
  theme(legend.position = "none")

# Violinplot

  ggplot(muestras) + 
  geom_violin(aes(x = tipo, y = x, col = tipo), width = 0.5) + 
  geom_boxplot(aes(x = tipo, y = x, col = tipo), width=0.15) + 
  geom_jitter(aes(x = tipo, y = x, color = tipo), width = 0.05,  alpha = 0.2) + 
  theme(legend.position = "none") + 
  coord_flip()

## QQplots.

p1 = ggplot(tibble(x = normales), aes(sample = x)) + 
  geom_qq(alpha = 0.2, color = "red") + 
  geom_qq_line() + 
  ggtitle("Datos normales") + 
  xlab("") +  ylab("") + 
  theme(plot.title = 
          element_text(color="red", size=14, face="bold.italic"))

p2 = ggplot(tibble(x = noNormales), aes(sample = x)) + 
  geom_qq(alpha = 0.2, color = "blue") + 
  geom_qq_line() + 
  ggtitle("Datos no normales") +
  xlab("") +  ylab("") + 
  theme(plot.title = 
          element_text(color="blue", size=14, face="bold.italic"))

grid.arrange(p1, p2, nrow = 1)

# Precaución al analizar la normalidad con muestras pequeñas

# Densidades...

numMuestras = 12
sizeMuestras = 15
muestras = tibble(x = rnorm(numMuestras * sizeMuestras),
                 tipo = gl(numMuestras, k = sizeMuestras))
ggplot(muestras, aes(x = x)) +
  geom_density(color="red", size=1.5) + 
  facet_wrap(. ~ tipo, nrow = 3)

# ... y boxplots

numMuestras = 12
sizeMuestras = 15
muestras = tibble(x = rnorm(numMuestras * sizeMuestras),
                 tipo = gl(numMuestras, k = sizeMuestras))
ggplot(muestras, aes(y = x)) +
  geom_boxplot(color="red") + 
  facet_wrap(. ~ tipo, nrow = 3)

