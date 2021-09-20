## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 6. Contraste de hipótesis.

########################################################################
########################################################################
########################################################################
########################################################################

#####################################################################
# Contraste de Hipótesis
#####################################################################

# p-valor usando el TCL

digits = 30

# Simulamos una muestra de una población normal como la del ejemplo.
set.seed(2017)
library(MASS)
muestra = mvrnorm(n = 100, mu = 2.65, Sigma = 0.5^2, empirical = TRUE)

mu0 = 2.5
(n = length(muestra))
(xBar = mean(muestra))
(s = sd(muestra))
# El valor del estadístico es: 
(z = (xBar - mu0) / (s / sqrt(n)))
# Y el p-valor es:
(pValor = 1 - pnorm(z))

###    
# p-valor por remuestreo

# En el ejemplo de las baterías de vehículos, supongamos 
# que la Hipótesis nula tiene razón. 
# Concretamente,  suponemos que mu <= 2.5. Entonces podemos simular 
# la toma de muestras de la población de baterías del nuevo método. 
# Vamos a usar replicate para hacer esto y ver qué fracción de 
# esas muestras están de acuerdo con H0.
# Estimamos p(H0 | datos muestrales)

numMuestras = 100000
mediasRemuestras = replicate(n = numMuestras, {
  remuestra = sample(muestra, 100, replace = TRUE)
  mediaRemuestra = mean(remuestra)
})
# Hemos obtenido muchas medias muestrales. Las primeras son:
head(mediasRemuestras)
# ¿Qué proporción de estas medias muestrales está de acuerdo con H0?
# Es fácil de obtener:
sum(mediasRemuestras <= mu0) / numMuestras
# Esta es otra manera de medir el p-valor y como ves, nos 
# da una respuesta muy parecida al TCL.
# hist(mediasRemuestras)

###    
# t de Student: contrastes sobre la media 
# con muestras pequeñas en variables normales.

n = 21
barX = 3.6
s = 0.6
mu0 = 4
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = pt(estadistico, df = n - 1)

# La función t.test

library(tidyverse)
(testCty = t.test(mpg$cty, mu = 16, 
                  alternative = "two.sided", conf.level = 0.95))

testCty$p.value

testCty$conf.int

testDispl = t.test(mpg$displ,  mu = 3.4, 
                    alternative = "greater", conf.level = 0.95)
testDispl$conf.int

## Contraste sobre la desviación típica.

n = 15
sigma0 = 0.5
s = 0.7
estadistico = (n - 1) * s^2 / sigma0^2
pValor = pchisq(estadistico, df = n - 1, lower.tail = FALSE)

# Usando TeachingDemos
require(TeachingDemos)
(varTestCty = sigma.test(mpg$cty, sigmasq = 16, 
           alternative = "greater", conf.level = 0.95))

# Gráfica de las curvas de potencia

alfa = 0.01
s = 0.5
deltas = seq(0, s, length.out=1000)
n0 = 100
powers = power.t.test(sd = s, n=n0, sig.level =alfa, delta = deltas, 
                      type = "one.sample", alternative = "two.sided", strict=FALSE)$power
plot(deltas, powers,pch=20,col="blue",lwd=0.6,ylab="Potencia",xlab=expression(delta==mu-mu[0]),font.lab=2,cex.axis=1.5,cex.lab=1.3)

## Tamaño muestral y potencia del contraste.

power.t.test(delta = 0.1, sd = 0.5, sig.level = 0.05,
             power = 0.80, type="one.sample", alternative="one.sided")

## Significación estadística vs relevancia científica. 

n = 50
barX = 13.05
s = 0.6
mu0 = 13
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = 2 * pt(abs(estadistico), df = n - 1, lower.tail = FALSE)

# ahora con una muestra mucho más grande:

n = 5000
barX = 13.05
s = 0.6
mu0 = 13
estadistico = (barX - mu0) / (s/sqrt(n))
pValor = 2 * pt(abs(estadistico), df = n - 1, lower.tail = FALSE)

## Relevancia y la d de Cohen.

dCohen = (barX - mu0) / s

## Simulando contrastes múltiples con R.

set.seed(2019)
nTests = 20 # Haremos 20 contrastes
# y este vector los 20 p-valores
pValores = numeric(nTests)
# Ahora hacemos los contrastes y guardamos los p-valores
for(i in 1:nTests){
  muestra = c(rnorm(15))
  pValores[i] = t.test(muestra, alternative = "two.sided", mu = 0)$p.value
}
# ¿Cuál es el p-valor más pequeño?
min(pValores)
