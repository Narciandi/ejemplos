
# Vacío la memoria de variables y paquetes - https://stackoverflow.com/a/41828719
rm(list = ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 

library(stringr)
library(openxlsx)

GenerarMatrizBachillerato <- function(datos){
  
  datos$CE <- str_replace_all(string = datos$CE,
                              pattern = "Competencia específica ",
                              replacement = "CE")
  datos$CompEsp <- datos$CE
  
  datos$CCL <- datos$CCL1 + datos$CCL2 + datos$CCL3 + datos$CCL4 + datos$CCL5
  datos$CP <- datos$CP1 + datos$CP2 + datos$CP3
  datos$STEM <- datos$STEM1 + datos$STEM2 + datos$STEM3 + datos$STEM4 + datos$STEM5
  datos$CD <- datos$CD1 + datos$CD2 + datos$CD3 + datos$CD4 + datos$CD5
  datos$CPSAA <- datos$CPSAA1.1 + datos$CPSAA1.2 + datos$CPSAA2 + datos$CPSAA3.1 + datos$CPSAA3.2 + datos$CPSAA4 + datos$CPSAA5
  datos$CC <- datos$CC1 + datos$CC2 + datos$CC3 + datos$CC4
  datos$CE <- datos$CE1 + datos$CE2 + datos$CE3
  datos$CCEC <- datos$CCEC1 + datos$CCEC2 + datos$CCEC3.1 + datos$CCEC3.2 + datos$CCEC4.1 + datos$CCEC4.2
  
  columnas <- c("Materia", "CompEsp", "CCL", "CP", "STEM", "CD", "CPSAA", "CC", "CE", "CCEC")
  
  salida <- datos[,columnas]  
  
  return(salida)
}

GenerarMatrizEso <- function(datos){
  
  datos$CE <- str_replace_all(string = datos$CE,
                              pattern = "Competencia específica ",
                              replacement = "CE")
  datos$CompEsp <- datos$CE
  
  datos$CCL <- datos$CCL1 + datos$CCL2 + datos$CCL3 + datos$CCL4 + datos$CCL5
  datos$CP <- datos$CP1 + datos$CP2 + datos$CP3
  datos$STEM <- datos$STEM1 + datos$STEM2 + datos$STEM3 + datos$STEM4 + datos$STEM5
  datos$CD <- datos$CD1 + datos$CD2 + datos$CD3 + datos$CD4 + datos$CD5
  datos$CPSAA <- datos$CPSAA1 + datos$CPSAA2 + datos$CPSAA3 + datos$CPSAA4 + datos$CPSAA5
  datos$CC <- datos$CC1 + datos$CC2 + datos$CC3 + datos$CC4
  datos$CE <- datos$CE1 + datos$CE2 + datos$CE3
  datos$CCEC <- datos$CCEC1 + datos$CCEC2 + datos$CCEC3 + datos$CCEC4
  
  columnas <- c("Materia", "CompEsp", "CCL", "CP", "STEM", "CD", "CPSAA", "CC", "CE", "CCEC")
  
  salida <- datos[,columnas]  
  
  return(salida)
}

#### ESO ####
rutaEntrada <- "./datos/Descriptores Eso.rds"
rutaSalida <- "./datos/Descriptores Eso.xlsx"
datos <- readRDS(file = rutaEntrada)
salida <- GenerarMatrizEso(datos)

# Añado las competencias de las materias de diversificación - Creado a mano
rutaDiversificacion <- "./recursos/Matriz Ambitos Diversificacion.xlsx"
salida <- rbind(salida, 
                read.xlsx(xlsxFile = rutaDiversificacion))
# Añado las competencias de las materias bilingües - Creado a mano
rutaBilingue <- "./recursos/Matriz Materias Bilingüe.xlsx"
salida <- rbind(salida, 
                read.xlsx(xlsxFile = rutaBilingue))
# Añado las competencias de las materias propias del centro - Creado a mano
rutaLlanes <- "./recursos/Matriz Materias Llanes.xlsx"
salida <- rbind(salida, 
                read.xlsx(xlsxFile = rutaLlanes))
write.xlsx(x = salida,
           file = rutaSalida)


#### Bachillerato ####

rutaEntrada <- "./datos/Descriptores Bachillerato.rds"
rutaSalida <- "./datos/Descriptores Bachillerato.xlsx"
datos <- readRDS(file = rutaEntrada)
salida <- GenerarMatrizBachillerato(datos)

# Añado las competencias de las materias propias del centro - Creado a mano
rutaLlanes <- "./recursos/Matriz Materias Llanes.xlsx"
salida <- rbind(salida, 
                read.xlsx(xlsxFile = rutaLlanes))

write.xlsx(x = salida,
           file = rutaSalida)
