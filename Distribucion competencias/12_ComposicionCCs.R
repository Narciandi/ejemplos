
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales) # Para usar label_wrap

#### CARGA DE DATOS ####

rutaDatos <- "./datos/Descriptores ESO.xlsx"
datos <- read.xlsx(xlsxFile = rutaDatos)

### CALCULO EL PORCENTAJE DE DOs POR MATERIA ####

datos %>% 
  group_by(Materia) %>% 
  summarise(nDOs = sum(CCL) + sum(CP) + sum(STEM) + sum(CD) + 
              sum(CPSAA) + sum(CC) + sum(CE) + sum(CCEC)) %>%
  arrange(desc(nDOs))

datosGrafico <- datos %>% 
  group_by(Materia) %>% 
  summarise(nDOs = sum(CCL) + sum(CP) + sum(STEM) + sum(CD) + sum(CPSAA) + sum(CC) + sum(CE) + sum(CCEC),
            nCCL = sum(CCL),
            nCP = sum(CP),
            nSTEM = sum(STEM),
            nCD = sum(CD),
            nCPSAA = sum(CPSAA),
            nCC = sum(CC),
            nCE = sum(CE),
            nCCEC = sum(CCEC)) %>%
  arrange(desc(nDOs))

# Mantengo el orden de materias de otros gráficos
datosGrafico$Materia <- factor(x = datosGrafico$Materia,
                               levels = datosGrafico$Materia)

# Calculo el porcentaje que representa cada CC en cada materia
columnas <- c("nCCL", "nCP", "nSTEM", "nCD", "nCPSAA", "nCC", "nCE", "nCCEC")
datosGrafico[,columnas] <- datosGrafico[,columnas] / datosGrafico$nDOs


datosGrafico <- pivot_longer(data = datosGrafico,
                             cols = c("nDOs", "nCCL", "nCP", "nSTEM", 
                                      "nCD", "nCPSAA", "nCC", "nCE", "nCCEC"))

datosGrafico <- pivot_wider(data = datosGrafico,
                            names_from = "Materia",
                            values_from = "value")

filas <- datosGrafico$name != "nDOs"
datosGrafico <- datosGrafico[filas,]

materias <- setdiff(names(datosGrafico),"name")

datosGrafico$PesoCC <- rowSums(datosGrafico[,materias]) 

datosGrafico[,c("name","PesoCC")]

datosGrafico[,materias] <- datosGrafico[,materias] / datosGrafico$PesoCC # Normalizo los pesos para calcular la contribución de cada materia

names(datosGrafico)[1] <- "CC"

datosGrafico <- pivot_longer(data = datosGrafico,
                             cols = materias)

# Sacada de un comentario en https://stackoverflow.com/a/9568659 
paleta <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", 
            "black", "gold1", "skyblue2", "palegreen2", "#FDBF6F", "gray70", 
            "maroon", "orchid1", "darkturquoise", "darkorange4", "brown") 

paleta <- pals::cols25(n = 24)

grafico <- ggplot(data = datosGrafico) +
  geom_col(mapping = aes(x = CC, 
                         y = value, 
                         fill= name),
           position = "stack") +
  scale_fill_manual(values = paleta) +
  coord_flip() +
  theme(legend.position = "bottom")

grafico

