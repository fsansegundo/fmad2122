#########################################
#########################################
# COMANDOS R DE USO FRECUENTE 
#########################################
#########################################


#########################################
# LIMPIEZA Y REINICIO DE SESIÓN 
#########################################

rm(list=ls()) # Borra todos los objetos del entorno
rstudioapi::restartSession() # Reinicia la sesión de R
cat("\014") # Es Ctrl + L para limpiar la Consola

#########################################
# UTILIDADES PARA Rmarkdown
#########################################

# El siguiente comando hace que el directorio de trabajo
# sea el que contiene el fichero desde el que se ejecuta.

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################################
# DESCARGA DE FICHEROS
#########################################

# Comprobamos el directorio de trabajo
getwd()
# Datos del fichero. La url, incluyendo el nombre del fichero.
file_url <-   "http://users.stat.ufl.edu/~winner/data/train_acc_2010.xls"
# Cambia la siguiente fila si quieres usar una subcarpeta del wd.
local_folder <-  "./"
# No cambies nada a partir de aquí
file_name <- tail(unlist(strsplit(file_url, split = "/")), 1)
localFile <-  paste0(local_folder, file_name)
if(!file.exists(localFile)){
  download.file(url = file_url, destfile = localFile)
} else {
  warning(paste0("Cuidado: el fichero de datos", localFile, " ya existe."))
}
