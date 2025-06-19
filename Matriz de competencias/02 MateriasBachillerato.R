
# rm(list = ls())

library(pdftools)
library(stringr)

rutaPdf <- "./recursos/Decreto Bachillerato.pdf"

entrada <- pdf_text(pdf = rutaPdf)

rutaTxt <- "./datos/Decreto Bachillerato.txt"

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

inicioAnexo <- BuscarTexto(lineas, "ANEXO II. a)") 

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

noMaterias <- c("ANEXO II. a)",
                "Materias del Bachillerato",
                "https://sede.asturias.es/bopa",
                "fundamentales para el estudio y comprensión de la civilización helena y cuyo aprendizaje",
                "diferentes formatos.",
                "fundamentales para el estudio y comprensión de la civilización latina y cuyo aprendizaje",
                "combina conocimientos léxicos y culturales, tales como imperium, natura, civis o paterfamilias,",
                "en textos de diferentes formatos.",
                "producción y la coproducción de textos orales, escritos y multimodales.",
                "Anexo II.b",
                "Materias optativas del Bachillerato")

materias <- setdiff(materias, noMaterias)

# Elimino una fila con caracteres griegos
indices <- grepl(pattern = "combina conocimientos léxicos y culturales, tales como",
                 x = materias)
materias <- materias[!indices]

