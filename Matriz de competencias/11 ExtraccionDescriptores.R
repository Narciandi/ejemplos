
# Vacío la memoria de variables y paquetes - https://stackoverflow.com/a/41828719
rm(list = ls())
pacman::p_unload(pacman::p_loaded(), character.only = TRUE) 

library(stringr)

#### Funciones ####

MateriasBachillerato <- function(){
  materias <- c("Análisis Musical","Artes Escénicas","Biología","Biología, Geología y Ciencias Ambientales","Ciencias Generales","Coro y Técnica Vocal","Cultura Audiovisual","Dibujo Artístico","Dibujo Técnico","Dibujo Técnico Aplicado a las Artes Plásticas y al Diseño","Diseño","Economía","Economía Emprendimiento y Actividad Empresarial","Educación Física","Empresa y Diseño de Modelos de Negocio","Filosofía","Física","Física y Química","Fundamentos Artísticos","Geología y Ciencias Ambientales","Geografía","Griego","Historia del Arte","Historia de España","Historia de la Filosofía","Historia del Mundo Contemporáneo","Historia de la Música y de la Danza","Latín","Lengua Castellana y Literatura","Lengua Extranjera","Lenguaje y Práctica Musical","Literatura Dramática","Literatura Universal","Matemáticas","Matemáticas Aplicadas a las Ciencias Sociales","Matemáticas Generales","Movimientos Culturales y Artísticos","Proyectos Artísticos","Química","Técnicas de Expresión Gráfico-plástica","Tecnología e Ingeniería","Volumen","Anatomía Aplicada","El Legado Clásico","Lengua Asturiana y Literatura","Psicología y Sociedad","Segunda Lengua Extranjera","Tecnologías Digitales Aplicadas","Gestión de Fuentes Documentales y Comunicación","Proyecto de Investigación Integrado","Recursos Energéticos y Sostenibilidad","Religión")  

  return(materias)
}

MateriasEso <- function(){
  materias <- c("Biología y Geología","Digitalización","Economía y Emprendimiento",
                "Educación Física","Educación Plástica, Visual y Audiovisual",
                "Educación en Valores Cívicos y Éticos","Expresión Artística",
                "Física y Química","Formación y Orientación Personal y Profesional",
                "Geografía e Historia","Latín","Lengua Castellana y Literatura",
                "Lengua Extranjera","Matemáticas","Música","Segunda Lengua Extranjera",
                "Tecnología","Tecnología y Digitalización","Cultura Clásica",
                "Digitalización Aplicada","Filosofía","Lengua Asturiana y Literatura",
                "Proyecto de Emprendimiento Social o Empresarial",
                "Taller de Economía Aplicada","Religión")  
  return(materias)
}

CorreccionErratas <- function(lineas, textoErrado, textoCorrecto){
  filaErrata <- grep(pattern = textoErrado,
                     x = lineas,
                     fixed = TRUE)
  lineas[filaErrata] <- str_replace(string = lineas[filaErrata],
                                    pattern = textoErrado,
                                    replacement = textoCorrecto)  
  return(lineas)
}

FilasTitulos <- function(lineas, materias){
  filasTitulos <- c()

for (materia in materias){
  filaMateria <- which(tolower(lineas) == tolower(materia))
  filasTitulos <- c(filasTitulos,filaMateria)
}

filasTitulos <- sort(filasTitulos)
  return(filasTitulos)
}

FilasCompetenciasEspecificas <- function(lineas){
  filasCompetenciasEspecificas <- grep("Competencia específica [0-9]", x = lineas)
  return(filasCompetenciasEspecificas)
}

FilasDescriptores <- function(lineas){
  # Para identificar la fila con los descriptores hay más de una posible frase
  frase <- "Esta competencia específica se conecta con los siguientes descriptores"
  filasDescriptores <- grep(pattern = frase, 
                            x = lineas)
  frase <- "Esta competencia se conecta con los siguientes descriptores"
  filasDescriptores <- c(filasDescriptores,
                         grep(pattern = frase, 
                              x = lineas))
  frase <- "Esta competencia conecta con los descriptores"
  filasDescriptores <- c(filasDescriptores,
                         grep(pattern = frase, 
                              x = lineas))
  frase <- "Esta competencia especifica se conecta con los siguientes descriptores"
  filasDescriptores <- c(filasDescriptores,
                         grep(pattern = frase, 
                              x = lineas))
  frase <- "Esta competencia específica se relaciona con los descriptores"
  filasDescriptores <- c(filasDescriptores,
                         grep(pattern = frase, 
                              x = lineas))


  filasDescriptores <- sort(filasDescriptores)
  return(filasDescriptores)
}

ExtraerDescriptores <- function(lineas, materias){
  
  filasTitulos <- FilasTitulos(lineas, materias)
  filasCompetenciasEspecificas <- FilasCompetenciasEspecificas(lineas)
  filasDescriptores <- FilasDescriptores(lineas)
  
  nLineas <- length(lineas)
  salida <- data.frame(Materia = character(),
                       CE = character(),
                       Descriptores = character())
  
  for (i in 1:nLineas){
    
    if(i %in% filasTitulos){
      titulo <- lineas[i]
    }
    if(i %in% filasCompetenciasEspecificas){
      competenciaEspecifica <- lineas[i]
    }
    if(i %in% filasDescriptores){
      descriptor <- paste(lineas[i],lineas[i+1])
      salida <- rbind(salida,
                      c(Materia = titulo, CE = competenciaEspecifica, Descriptor = descriptor),
                      deparse.level = 1)
    }
  }
  names(salida) <- c("Materia","CE","Descriptores")
  
  return(salida)
}

DepurarCompetenciasEspecificas <- function(datos){
  datos$CE <- substr(x = datos$CE, start = 1, stop = 25)
  datos$CE <- gsub(pattern = ".", replacement = "", x = datos$CE, fixed = TRUE)
  datos$CE <- gsub(pattern = ":", replacement = "", x = datos$CE, fixed = TRUE)
  
  return(datos)
}

DescriptoresBachillerato <- function(){
  descriptores <- c("CCL1","CCL2","CCL3","CCL4","CCL5",
                    "CP1","CP2","CP3",
                    "STEM1","STEM2","STEM3","STEM4","STEM5",
                    "CD1","CD2","CD3","CD4","CD5",
                    "CPSAA1.1","CPSAA1.2","CPSAA2","CPSAA3.1","CPSAA3.2","CPSAA4","CPSAA5",
                    "CC1","CC2","CC3","CC4",
                    "CE1","CE2","CE3",
                    "CCEC1","CCEC2","CCEC3.1","CCEC3.2","CCEC4.1","CCEC4.2")
  
  return(descriptores)
}

DescriptoresEso <- function(){
  descriptores <- c("CCL1","CCL2","CCL3","CCL4","CCL5",
                    "CP1","CP2","CP3",
                    "STEM1","STEM2","STEM3","STEM4","STEM5",
                    "CD1","CD2","CD3","CD4","CD5",
                    "CPSAA1","CPSAA2","CPSAA3","CPSAA4","CPSAA5",
                    "CC1","CC2","CC3","CC4",
                    "CE1","CE2","CE3",
                    "CCEC1","CCEC2","CCEC3","CCEC4")
  
  return(descriptores)
}

DepurarDescriptores <- function(datos, descriptores){
  for (descriptor in descriptores){
    datos[,descriptor] <- 0
    filas <- grep(pattern = descriptor,
                  x = datos$Descriptores,
                  fixed = TRUE)
    datos[filas,descriptor] <- 1 
  }
  
  datos <- datos[,-3]
  return(datos)
}

BuscarTexto <- function(lineas, texto){
  for (i in 1:length(lineas)){
    if(trimws(lineas[i]) == texto){
      return(i)
    }
  }
}


#### Bachillerato ####
rutaNormativa <- "./datos/Decreto Bachillerato.txt"
rutaSalida <- "./datos/Descriptores Bachillerato.rds"
materias <- MateriasBachillerato()
descriptores <- DescriptoresBachillerato()

# Cargo el fichero de normativa
conexion <- file(rutaNormativa, "r") 
lineas <- readLines(con = conexion, encoding = "ANSI")
close(conexion)
lineas <- trimws(lineas) # Elimino los espacios sobrantes antes y después de cada línea

inicioAnexo <- BuscarTexto(lineas, "ANEXO II. a)") 
finAnexo <- BuscarTexto(lineas, "ANEXO III") - 1
lineas <- lineas[inicioAnexo:finAnexo]

# Erratas #
textoErrado <- "Competencias específica 1."
textoCorrecto <- "Competencia específica 1."
lineas <- CorreccionErratas(lineas, textoErrado, textoCorrecto)

# Extracción de la información #
datosDescriptores <- ExtraerDescriptores(lineas, materias)

# Depuración #
datosDescriptores <- DepurarCompetenciasEspecificas(datosDescriptores)
salida <- DepurarDescriptores(datosDescriptores, descriptores)

# Salida #
saveRDS(object = salida,
        file = rutaSalida)

#### ESO ####
rutaNormativa <- "./datos/Decreto ESO.txt"
rutaSalida <- "./datos/Descriptores ESO.rds"
materias <- MateriasEso()
descriptores <- DescriptoresEso()

# Cargo el fichero de normativa
conexion <- file(rutaNormativa, "r") 
lineas <- readLines(con = conexion, encoding = "ANSI")
close(conexion)
lineas <- trimws(lineas) # Elimino los espacios sobrantes antes y después de cada línea

inicioAnexo <- BuscarTexto(lineas, "ANEXO II") 
finAnexo <- BuscarTexto(lineas, "ANEXO III") - 1
lineas <- lineas[inicioAnexo:finAnexo]

# Erratas #
textoErrado <- "Competencias específica "
textoCorrecto <- "Competencia específica "
lineas <- CorreccionErratas(lineas, textoErrado, textoCorrecto)

# Extracción de la información #
datosDescriptores <- ExtraerDescriptores(lineas, materias)

# Depuración #
datosDescriptores <- DepurarCompetenciasEspecificas(datosDescriptores)
salida <- DepurarDescriptores(datosDescriptores, descriptores)

# Salida #
saveRDS(object = salida,
        file = rutaSalida)


