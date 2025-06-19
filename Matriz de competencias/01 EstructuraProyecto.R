
#### BIBLIOTECAS NECESARIAS ####

instaladas <- row.names(installed.packages())
necesarias <-  c("TAF", "pdftools", "stringr", "openxlsx")

install.packages(setdiff(necesarias, instaladas))

#### ESTRUCTURA DE CARPETAS ####

library(TAF) # Para usar mkdir

# Creo la estructura del proyecto

mkdir("recursos")
mkdir("datos")
mkdir("salida")

#### NORMATIVA ####

# Descargo los ficheros con la normativa

url <- "https://sede.asturias.es/bopa/2022/09/01/2022-06713.pdf" # Decreto ESO
destino <- "./recursos/Decreto ESO.pdf"
download.file(url = url,
              destfile = destino,
              mode = "wb")


url <- "https://sede.asturias.es/bopa/2022/09/01/2022-06714.pdf" # Decreto Bachillerato
destino <- "./recursos/Decreto Bachillerato.pdf"
download.file(url = url,
              destfile = destino,
              mode = "wb")
  