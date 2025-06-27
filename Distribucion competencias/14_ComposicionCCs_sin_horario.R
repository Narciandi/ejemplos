
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

rutaHorario <- "./recursos/Horario ESO.xlsx" # Fichero generado a mano 
horario <- read.xlsx(xlsxFile = rutaHorario)
names(horario) <- c("Materia", "Tipo", "Horas1", "Horas2", "Horas3" , "Horas4")

curso <- 4

#### PREPARO LOS DATOS ####

# Compruebo que las listas sin idénticas
materiasDatos <- unique(datos$Materia)
materiasHorario <- unique(horario$Materia)
setdiff(materiasDatos, materiasHorario)
setdiff(materiasHorario, materiasDatos)


datosGrafico <- datos %>% 
  group_by(Materia) %>% 
  summarise(DOs = sum(CCL) + sum(CP) + sum(STEM) + sum(CD) + sum(CPSAA) + sum(CC) + sum(CE) + sum(CCEC),
            CCL = sum(CCL),
            CP = sum(CP),
            STEM = sum(STEM),
            CD = sum(CD),
            CPSAA = sum(CPSAA),
            CC = sum(CC),
            CE = sum(CE),
            CCEC = sum(CCEC)) %>%
  arrange(desc(DOs))


# Mantengo el orden de materias de otros gráficos
datosGrafico$Materia <- factor(x = datosGrafico$Materia,
                               levels = datosGrafico$Materia)

# Calculo el porcentaje que representa cada CC en cada materia
columnas <- c("CCL", "CP", "STEM", "CD", "CPSAA", "CC", "CE", "CCEC")
datosGrafico[,columnas] <- datosGrafico[,columnas] / datosGrafico$DOs



# Añado las horas de cada materia
datosGrafico <- left_join(x = datosGrafico,
                          y = horario,
                          by = "Materia")

# Escojo la columna de horas del curso
colHoras <- paste0("Horas",curso)

# Ñapa rápida para eliminar materias sin carga horaria
for (columna in columnas) {
  datosGrafico[,columna]  <- datosGrafico[,columna] * datosGrafico[,colHoras] / datosGrafico[,colHoras]
}


# Elimino columnas sobrantes
datosGrafico <- datosGrafico[,c("Materia", "CCL", "CP", "STEM", "CD", "CPSAA", "CC", "CE", "CCEC" )]

# Continúo preparando los datos para los gráficos
datosGrafico <- pivot_longer(data = datosGrafico,
                             cols = c("CCL", "CP", "STEM", 
                                      "CD", "CPSAA", "CC", "CE", "CCEC"))

datosGrafico <- pivot_wider(data = datosGrafico,
                            names_from = "Materia",
                            values_from = "value")



materias <- setdiff(names(datosGrafico),c("name"))

datosGrafico$PesoCC <- rowSums(datosGrafico[,materias], na.rm = TRUE) 

datosGrafico[,materias] <- datosGrafico[,materias] / datosGrafico$PesoCC # Normalizo los pesos para calcular la contribución de cada materia

names(datosGrafico)[1] <- "CC"

datosGrafico <- pivot_longer(data = datosGrafico,
                             cols = materias)

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
  
  rutaSalida <- paste0("./salida/PesoMaterias_",competencia,"_SinHoras_",curso,"ESO.png")
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
          plot.title = element_text(size = 15),
          plot.subtitle = element_text(size = 12)) +
    ggtitle(label = paste0(nombreCompetencia," ",curso,"ºESO"),
            subtitle = "Peso de cada materia sin ajuste por horas")
  
  ggsave(filename = rutaSalida,
         plot = grafico,
         units = "mm",
         width = 200,
         height = 200)
}