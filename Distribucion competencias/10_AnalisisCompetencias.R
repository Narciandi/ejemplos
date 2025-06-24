
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales) # Para usar label_wrap

rutaDatos <- "./datos/Descriptores ESO.xlsx"
datos <- read.xlsx(xlsxFile = rutaDatos)


# Análisis que puedo hacer:
# Número total de descriptores por materia

datos %>% 
  group_by(Materia) %>% 
  summarise(nDOs = sum(CCL) + sum(CP) + sum(STEM) + sum(CD) + 
              sum(CPSAA) + sum(CC) + sum(CE) + sum(CCEC)) %>%
  arrange(desc(nDOs))

# Número total de descriptores por CE
# Análisis de descriptores por competencia clave
# Peso de materia en cada CC

#### DESCRIPTORES POR MATERIA ####

datosGrafico <- datos %>% 
  group_by(Materia) %>% 
  summarise(nDOs = sum(CCL) + sum(CP) + sum(STEM) + sum(CD) + sum(CPSAA) + sum(CC) + sum(CE) + sum(CCEC)) %>%
  arrange(desc(nDOs))

datosGrafico$Materia <- factor(x = datosGrafico$Materia,
                               levels = datosGrafico$Materia)

grafico <- ggplot(datosGrafico) + 
  geom_col(mapping = aes(x = Materia, y = nDOs)) + 
  theme(axis.text.x = element_text(angle = 90),
      panel.grid = element_line(colour = "black"),
      panel.background = element_rect(fill = "white"),
      legend.title = element_blank()) +
  scale_x_discrete(labels = label_wrap(28)) +
  ggtitle(label = "Número de descriptores operativos por materia",
          subtitle = "ESO") +
  xlab(label = "") +
  ylab(label = "")

grafico

#### DESCRIPTORES POR MATERIA Y CC ####

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

datosGrafico <- pivot_longer(data = datosGrafico,
                             cols = c("nDOs", "nCCL", "nCP", "nSTEM", "nCD", "nCPSAA", "nCC", "nCE", "nCCEC"))

filas <- datosGrafico$name != "nDOs"
datosGrafico <- datosGrafico[filas,]
datosGrafico$name <- str_replace(string = datosGrafico$name,
            pattern = "n",
            replacement = "") 


grafico <- ggplot(data = datosGrafico) +
  geom_col(mapping = aes(x = Materia, fill = name, y = value),
           position = position_stack(),
           width = 0.75) +
  geom_label(mapping = aes(x = Materia, 
                           group = name, 
                           y = value, 
                           label = ifelse(value !=0, value, NA)),
             position = position_stack(vjust = 0.5),
             size = 2.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = label_wrap(28)) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank()) +
  ggtitle(label = "Número de descriptores operativos por materia y competencia clave",
          subtitle = "ESO") +
  xlab(label = "") +
  ylab(label = "")

grafico

ggsave(filename = "./salida/Número DOs por materia y CC.png",
       plot = grafico,
       units = "mm",
       height = 210,
       width = 280)

#### PORCENTAJE DE DESCRIPTORES POR MATERIA Y CC ####

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

datosGrafico[,columnas] <- datosGrafico[,columnas]*100

datosGrafico <- pivot_longer(data = datosGrafico,
                             cols = c("nDOs", "nCCL", "nCP", "nSTEM", "nCD", "nCPSAA", "nCC", "nCE", "nCCEC"))

filas <- datosGrafico$name != "nDOs"
datosGrafico <- datosGrafico[filas,]
datosGrafico$name <- str_replace(string = datosGrafico$name,
                                 pattern = "n",
                                 replacement = "") 


grafico <- ggplot(data = datosGrafico) +
  geom_col(mapping = aes(x = Materia, fill = name, y = value),
           position = position_stack(),
           width = 0.75) +
  geom_label(mapping = aes(x = Materia, 
                           group = name, 
                           y = value, 
                           label = ifelse(value !=0, paste0(round(value),"%"), NA)),
             position = position_stack(vjust = 0.5),
             size = 2.5) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = label_wrap(28)) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid = element_line(colour = "black"),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank()) +
  ggtitle(label = "Porcentaje de los DOs de cada materia para cada CC",
          subtitle = "ESO") +
  xlab(label = "") +
  ylab(label = "")

grafico

ggsave(filename = "./salida/Porcentaje CC por materia.png",
       plot = grafico,
       units = "mm",
       height = 210,
       width = 280)

# Peso de cada CC en cada materia
# Análisis de componentes principales
