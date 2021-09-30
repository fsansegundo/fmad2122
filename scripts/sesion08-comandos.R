## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Sesión 8. Modelos lineales generalizados (glm). Regresión Logística. .

########################################################################
########################################################################
########################################################################
########################################################################

# Modelos lineales generalizados (glm). Regresión Logística. 

# Descripción del problema.
library(tidyverse)
CHDdata <-  read_delim("./data/CHDAGE.txt", delim = "\t")
CHDdata %>% slice_head(n = 6)

summary(CHDdata)

# Diagrama de dispersión para regresión logística
ggplot(CHDdata) + 
  geom_point(aes(x = AGE, y = CHD, size=2), 
             show.legend=FALSE, position = position_jitter(w = 0, h = 0.02)) +
  geom_hline(yintercept = 0:1, linetype = "dashed", color = "blue", size = 2)

# Construcción del modelo.. Franjas de edad
AGEbreaks = c(20, seq(from = 30, to = 60, by = 5), 70)
CHDdata <- CHDdata %>% 
  mutate(AgeGroup = cut(AGE, breaks = AGEbreaks, right = FALSE))

# tabla de contingencia de CHD frente al grupo de edad
options(width = 80)
(tabla1 = as.matrix(table(CHDdata$CHD, CHDdata$AgeGroup)))

(sumaColumnas = colSums(tabla1))

(probs = signif(tabla1[2, ] / sumaColumnas, 2))

# ...para pensar en términos de probabilidades. 
(probs = signif(tabla1[2, ] / sumaColumnas, 2))

# Representación gráfica de las probabilidades por franja de edad

midpoints = AGEbreaks[-length(AGEbreaks)] + c(5, rep(2.5, 6), 5)
probsdf = data.frame(midpoints, probs)
ggplot(CHDdata) + 
  geom_point(aes(x = AGE, y = CHD, size=4), 
             show.legend=FALSE, 
             position = position_jitter(w = 0, h = 0.02)) +
  geom_hline(yintercept = 0:1, 
             linetype = "dashed", 
             color="blue", 
             size=2) + 
  geom_point(data = probsdf, 
             mapping = aes(x = midpoints, 
                           y = probs, 
                           size=4, col="red"),
             show.legend=FALSE)

# El modelo es una curva con forma de s.

glmCHD = glm(CHD ~ AGE, family = binomial(link = "logit"), CHDdata)
summGlmCHD = summary(glmCHD)
curvaX = data.frame(AGE = seq(20, 70, length.out = 101))
curvaY = predict(glmCHD, newdata = curvaX, type = "response")
curvaDf = data.frame(AGE = curvaX$AGE, CHD = curvaY)
ggplot(CHDdata) + 
geom_point(aes(x = AGE, y = CHD, size=4), 
           show.legend=FALSE, position = position_jitter(w = 0, h = 0.02)) +
  geom_hline(yintercept = 0:1, linetype = "dashed", color="blue", size=2) + 
  geom_line(data = curvaDf, aes(x = AGE, y = CHD, col= "red", size=3), 
            show.legend=FALSE)

# Ajustando un modelo logístico con R. Función glm.
(glmCHD = glm(CHD ~ AGE, family = binomial, data = CHDdata))

coefficients(glmCHD)

# Usando el modelo logístico para predecir.
edadPredecir = data.frame(AGE = 32)
(probCHD = predict(glmCHD, newdata = edadPredecir, type = 'response'))

# Predicción en escala de log-odds
predict(glmCHD, newdata = edadPredecir, type = 'link')
coefficients(glmCHD)[1] + coefficients(glmCHD)[2] * 32

# Interpretación de coeficientes en regresión logística. 

edades = data.frame(AGE = 50:55)
probabilidades = predict(glmCHD, newdata = edades, type = 'response')
diff(probabilidades)
logOdds = predict(glmCHD, newdata = edades, type = 'link')
diff(logOdds)
coefficients(glmCHD)[2]

########################################################################
########################################################################
########################################################################
########################################################################
