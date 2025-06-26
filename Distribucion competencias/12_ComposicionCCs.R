

library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(pals)
library(treemapify)

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

datosGrafico$CC <- str_replace(string = datosGrafico$CC,
                               pattern = "n",
                               replacement = "")


### GRÁFICO DE BARRAS ####

# Sacada de un comentario en https://stackoverflow.com/a/9568659 
paleta <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", 
            "black", "gold1", "skyblue2", "palegreen2", "#FDBF6F", "gray70", 
            "maroon", "orchid1", "darkturquoise", "darkorange4", "brown") 
paleta <- cols25(n = 24)

grafico <- ggplot(data = datosGrafico) +
  geom_col(mapping = aes(x = CC, 
                         y = value, 
                         fill= name),
           position = "stack") +
  scale_fill_manual(values = paleta) +
  coord_flip() +
  theme(legend.position = "bottom")

grafico


### TREEMAP ####

# paleta <- brewer.blues(n = 24)
# paleta <- linearl(n = 24)
# paleta <- linearlhot(n = 24)
# paleta <- kovesi.rainbow(n = 24)
# paleta <- cols25(n = 24)

paleta <- c("#00876c", "#3d9a70", "#64ad73", "#89bf77", "#afd17c",
            "#d6e184", "#fff18f", "#fdd576", "#fbb862", "#f59b56",
            "#ee7d4f", "#e35e4e", "#de425b", "#e9597c", "#f2719b",
            "#f789b9", "#faa0d3", "#fcb8eb", "#ffceff", "#ecb2f4",
            "#d696eb", "#bd7ce3", "#a263db", "#814bd5", "#5736cf")


competencias <- c("CCL", "CP", "STEM", "CD", "CPSAA", "CC", "CE", "CCEC")

nombresCompetencias <- c("Competencia en Comunicación Lingüística (CCL)", 
                         "Competencia Plurilingue (CP)", 
                         "Competencia en Ciencia, Tecnología, Ingeniería y Matemáticas (STEM)", 
                         "Competencia Digital (CD)", 
                         "Competencia Persona, Social y de Aprender a Aprender (CPSAA)", 
                         "Competencia Ciudadana (CC)", 
                         "Competencia Emprendedora (CE)", 
                         "Competencia en Conciencia y Expresiones Culturales (CCEC)")


for (competencia in competencias){
  
  rutaSalida <- paste0("./salida/PesoMaterias_",competencia,".png")
  nombreCompetencia <- nombresCompetencias[competencia == competencias]
  
  grafico <- ggplot(data = datosGrafico %>% filter(CC == competencia)) +
    geom_treemap(mapping = aes(area = value, 
                               fill = name),
                 layout = "srow",
                 start = "topleft",
                 color = "black") +
    geom_treemap_text(mapping = aes(label = paste0(name,":\n",round(100*value),"%"),
                                    area = value),
                      place = "centre",
                      reflow = TRUE,
                      grow = FALSE,
                      layout = "srow",
                      start = "topleft") +
    scale_fill_manual(values = paleta) +
    theme(legend.position = "none",
          plot.title = element_text(size = 25),
          plot.subtitle = element_text(size = 20)) +
    ggtitle(label = nombreCompetencia,
            subtitle = "Peso de cada materia")

  ggsave(filename = rutaSalida,
         plot = grafico,
         units = "mm",
         width = 200,
         height = 200)
}


