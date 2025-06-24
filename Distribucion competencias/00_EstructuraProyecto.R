#### BIBLIOTECAS NECESARIAS ####

instaladas <- row.names(installed.packages())
necesarias <-  c("TAF", "openxlsx")

install.packages(setdiff(necesarias, instaladas))

#### ESTRUCTURA DE CARPETAS ####

library(TAF) # Para usar mkdir

# Creo la estructura del proyecto

mkdir("datos")
mkdir("salida")

#### DATOS ####

url <- "https://github.com/Narciandi/ejemplos/raw/refs/heads/main/Matriz%20de%20competencias/datos/Descriptores%20Eso.xlsx" # Matriz con datos de ESO
destino <- "./datos/Descriptores ESO.xlsx"
download.file(url = url,
              destfile = destino,
              mode = "wb")

url <- "https://github.com/Narciandi/ejemplos/raw/refs/heads/main/Matriz%20de%20competencias/datos/Descriptores%20Bachillerato.xlsx" # Matriz con datos de Bachillerato
destino <- "./datos/Descriptores Bachillerato.xlsx"
download.file(url = url,
              destfile = destino,
              mode = "wb")


