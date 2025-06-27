
#### BIBLIOTECAS NECESARIAS ####

instaladas <- row.names(installed.packages())
necesarias <-  c("TAF", "openxlsx","dplyr","ggplot2","tidyr","stringr","scales","pals","treemapify")

install.packages(setdiff(necesarias, instaladas))

#### ESTRUCTURA DE CARPETAS ####

library(TAF) # Para usar mkdir

# Creo la estructura del proyecto

mkdir("datos")
mkdir("salida")
mkdir("recursos")

#### DOCUMENTOS ####

url <- "https://www.educastur.es/documents/34868/907664/2022-08-LOMLOE-curriculo-ESO-asturias-Anexo4.pdf/141a62d6-938b-7ec3-6bb4-06f74a8a68e9?t=1661860842496" # Horario de la ESO
destino <- "./recursos/Horario ESO.pdf"
download.file(url = url,
              destfile = destino,
              mode = "wb")
# Este documento lo convierto en Excel a mano.

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


