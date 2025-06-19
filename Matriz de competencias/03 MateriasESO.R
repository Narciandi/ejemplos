
# rm(list = ls())

library(pdftools)
library(stringr)


rutaPdf <- "./recursos/Decreto ESO.pdf"

entrada <- pdf_text(pdf = rutaPdf)

rutaTxt <- "./datos/Decreto ESO.txt"

fileConn <- file(rutaTxt)
writeLines(entrada, fileConn)
close(fileConn)

fileConn <- file(rutaTxt)
lineas <- readLines(con = fileConn)
close(fileConn)

BuscarTexto <- function(lineas, texto){
  for (i in 1:length(lineas)){
    if(trimws(lineas[i]) == texto){
      return(i)
    }
  }
}

inicioAnexo <- BuscarTexto(lineas, "ANEXO II") 

finAnexo <- BuscarTexto(lineas, "ANEXO III") - 1

LineasCentradas <- function(lineas, nEspaciosEnBlanco = 25){
  
  nLineas <- length(lineas)
  salida <- rep(x = FALSE, nLineas)
  
  for (i in 1:nLineas){
    linea <- lineas[i]
    if (linea != ""){
      inicio <- substr(x = linea,
                       start = 1,
                       stop = nEspaciosEnBlanco)
      if (trimws(inicio) == ""){
        salida[i] <- TRUE
      }  
    }

  }
  
  return(salida)
}

lineasAnexo <- lineas[inicioAnexo:finAnexo]


lineasCentradas <- LineasCentradas(lineasAnexo)

materias <- lineasAnexo[lineasCentradas]

materias <- trimws(materias)

indices <- str_detect(string = materias,
                      pattern = "[:digit:]+")
materias <- materias[!indices]

noMaterias <- c("ANEXO II",
                "Materias de la Educación Secundaria Obligatoria",
                "disfrutando del entorno de manera sostenible, minimizando el impacto ambiental que",
                "estas puedan producir y siendo conscientes de su huella ecológica.",
                "de seguridad individuales y colectivas.",
                "estas puedan producir, siendo conscientes de su huella ecológica y desarrollando",
                "actuaciones intencionadas dirigidas a la conservación y mejora de las condiciones de los",
                "espacios en los que se desarrollen.",
                "asumiendo responsabilidades y aplicando normas de seguridad individuales y colectivas.",
                "reflexionando de manera progresivamente autónoma sobre su funcionamiento.",
                "conocimientos necesarios.",
                "herramientas y estrategias matemáticas, estableciendo conexiones entre el mundo real",
                "y las matemáticas y usando los procesos inherentes a la investigación: inferir, medir,",
                "comunicar, clasificar y predecir.",
                "reflexionando de manera progresivamente autónoma sobre aspectos básicos de su",
                "funcionamiento.",
                "reflexionando de manera progresivamente autónoma sobre aspectos significativos de su",
                "Materias optativas de la Educación Secundaria Obligatoria",
                "ANEXO III",
                "https://sede.asturias.es/bopa")

materias <- setdiff(materias, noMaterias)

# cat(materias, sep = '","')

