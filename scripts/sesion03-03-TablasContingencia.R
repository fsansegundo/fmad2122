########################################################
# Fundamentos Matemáticos de la Estadística. Curso 2021-2022.
# sesion03-03-TablasContingencia
# Ejemplos clase. 
########################################################

rm(list=ls())

(tablaContingencia = matrix(c(192, 4, 158, 9646), nrow=2))

colnames(tablaContingencia) = c("ENF","SANO" )
rownames(tablaContingencia) = c("Prueba+", "Prueba-" )
tablaContingencia


sum(tablaContingencia)

# Añadimos márgenes
(tablaContingenciaAmpliada = addmargins(tablaContingencia))

# Intersecciones
(tablaContingenciaRel = tablaContingenciaAmpliada / sum(tablaContingencia))

# Dividimos cada fila por su suma (usando prop.table):
(tablaMarginalFilas = prop.table(tablaContingencia, margin = 1))
addmargins(tablaMarginalFilas) # Condicionadas por prueba+ / prueba-


# Dividimos cada columna por su suma:
(tablaMarginalColumnas = prop.table(tablaContingencia, margin = 2))
addmargins(tablaMarginalColumnas)  # Condicionadas por sano / enfermo



