## title: 'Master en Big Data. Fundamentos  matemáticos  del  análisis  de  datos.'
## author: "Fernando San Segundo"
## subtitle: "Práctica 1"


library(tidyverse)

fhs = read_csv("data/framingham.csv")

#############################################
### Verbos de dplyr
#############################################


# dplyr: select.

library(gapminder)
names(gapminder)

gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  head(3)

gapminder %>% 
  select(continent:pop, -year) %>% 
  names()

gapminder %>%  
  select(starts_with("c")) %>% 
    names()

# dplyr: filter

gapminder %>%
  filter(country == 'Spain') %>%
  head(4)

# dplyr: filter

gapminder %>% 
filter(year == "1997") %>% 
top_n(3, gdpPercap)

# dplyr: mutate
gapminder %>% 
  mutate(gdp = pop * gdpPercap / 10^6) %>% 
  filter(year == 1982) %>% 
  sample_n(4)

gapminder %>% 
  mutate(gdp = pop * gdpPercap / 10^6) %>% 
  mutate_at("gdp", log10) %>% 
  head(4)

# dplyr: summarise 
    iris %>% 
      summarise(mediana = median(Petal.Length), desvMediana = mad(Petal.Length))


# dplyr: summarise con group_by
    
    iris %>% 
      group_by(Species) %>% 
      summarise(mediana = median(Petal.Length), desvMediana = mad(Petal.Length))

# group_by con más de un factor

    mpg %>% 
      group_by(manufacturer, cyl) %>% 
      summarise(urbano = mean(cty), n = n()) %>% 
  head(6)

# group_by con más de un factor

    mpg %>% 
      group_by(manufacturer) %>% 
      count(cyl) %>% 
      head(8)

(planeta = list(nombre = "Marte", exterior = TRUE, 
                 radio = 3389.5, satelites = list("Fobos", "Deimos")))

# accediendo a los elementos de una lista
planeta[[1]]
planeta$exterior
planeta$satelites[[1]]

planeta[1]
planeta["exterior"]

# Funciones list, append y c
(l1 = list("A", "B"))
(l2 = list(c("A", "B")))

## (l3 = list(l2, "C"))

l4 = append(l2, "D")
(l4 = c(l2, "D"))

# Otras propiedades y operaciones con listas
l4[3] = NULL
l4

unlist(l1)

lista = list(letters[1:3], matrix(1:12, nrow = 3), TRUE)
unlist(lista)

# Estructuras de control. If/else 
ifelse(((1:5) < 3), yes = "A",  no = "B")

# Ejemplo de bucle for con next y break
valores = numeric(10) # Creamos un vector del tamaño previsto
for (k in 1:10){
  sorteo = sample(1:20, 1)
  print(paste0("k = ", k, ", sorteo = ", sorteo))
  if (k %in% 5:6){
    next # saltamos dos valores
  } else if (sorteo  == 1){
    print("Resultado del sorteo es 1, fin del bucle")
    break # paramos si un valor aleatorio es 1
  }
  valores[k] = sorteo # se ejecuta cuando no se cumplan las condiciones
} 
valores

# Ejemplo de bucle while
k = 0
while (k < 4){
  k = k + 1
  print(k)
  if(sample(1:6, 1) == 6){
    print("Final prematuro")
    break()
  } 
}

# Ejemplo de bucle repeat similar al bucle while previo
k = 1
repeat {
  k = k + 1
  print(k)
  if(sample(1:6, 1) == 6){
    print("Final prematuro")
    break()
  } 
}

# Funciones de R
genPasswd = function(size, upp = TRUE, low = TRUE, nmb = TRUE){

  # El vector pool guarda el juego de caracteres del password
  pool = character() 
  
  # Generamos pool según las opciones
  if(upp) pool = c(pool, LETTERS) 
  if(low) pool = c(pool, letters)
  if(nmb) pool = c(pool, 0:9)
  
  # Sorteamos los símbolos que aparecen en el password
  passwd = sample(pool, size, replace = TRUE) 
  # Y lo reducimos a un string con paste
  paste(passwd, sep = "", collapse = "") 
}

genPasswd(size = 15)

# Acceso a las componentes de una función
formals(genPasswd)

body(genPasswd)

body(genPasswd) = "No me apetece trabajar...invéntate tú el password"
genPasswd(12)

# Manejo de datos ausentes. Función is.na
x = c(2, 3, -5, NA, 4, 6, NA)
is.na(x)

any(is.na(fhs$glucose))

# complete.cases y na.rm
head(complete.cases(fhs), 17)

mean(fhs$glucose)

mean(fhs$glucose, na.rm = TRUE)
