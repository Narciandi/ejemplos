
library(openxlsx)

rutaDatos <- "./datos/Descriptores ESO.xlsx"
datos <- read.xlsx(xlsxFile = rutaDatos)


# Análisis que puedo hacer:
# Número total de descriptores por materia

library(dplyr)

datos %>% 
  group_by(Materia) %>% 
  summarise(nDOs = sum(CCL) + sum(CP) + sum(STEM) + sum(CD) + 
              sum(CPSAA) + sum(CC) + sum(CE) + sum(CCEC)) %>%
  arrange(desc(nDOs))

# Número total de descriptores por CE
# Análisis de descriptores por competencia clave
# Peso de materia en cada CC

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

datosGrafico$Materia <- factor(x = datosGrafico$Materia,
                               levels = datosGrafico$Materia)

library(ggplot2)

grafico <- ggplot(datosGrafico) + 
  geom_col(mapping = aes(x = Materia, y = nDOs)) + 
  theme(axis.text.x = element_text(angle = 90))




# Peso de cada CC en cada materia
# Análisis de componentes principales
