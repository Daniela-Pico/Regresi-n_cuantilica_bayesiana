# ========================================
# 1. Cargar librerías
# ========================================
library(ggplot2)
library(ggthemes)
library(sf)
library(dplyr)
library(RColorBrewer)

# ========================================
# 2. Cargar y filtrar los datos
# ========================================
datos <- read.csv("completos_datos_clusterizados_final.csv")

# Filtrar solo municipios de Antioquia
datos <- subset(datos, COLE_DEPTO_UBICACION == "ANTIOQUIA")

# ========================================
# 3. Calcular promedio del puntaje por municipio y clúster
# ========================================
df_promedios <- datos %>%
  group_by(COLE_MCPIO_UBICACION, ClusterMuni) %>%
  summarise(mean_puntaje = mean(PUNT_GLOBAL, na.rm = TRUE), .groups = "drop")

# ========================================
# 4. Leer shapefile de municipios de Antioquia
# ========================================
mapa_antioquia <- st_read("Municipios Antioquia")

# ========================================
# 5. Emparejar municipios entre datos y shapefile
# ========================================
municipios_datos <- unique(datos$COLE_MCPIO_UBICACION)
municipios_mapa  <- mapa_antioquia$MPIO_NOMBR

# Verificar cuáles municipios no coinciden
diferencia <- setdiff(municipios_mapa, municipios_datos)

# Filtrar shapefile solo a municipios presentes en los datos
mapa_antioquia <- mapa_antioquia %>%
  filter(MPIO_NOMBR %in% municipios_datos)

# ========================================
# 6. Unir los datos de puntaje al mapa
# ========================================
mapa_antioquia <- mapa_antioquia %>%
  left_join(df_promedios, by = c("MPIO_NOMBR" = "COLE_MCPIO_UBICACION"))

# ========================================
# 7. Mapa de clústeres por municipio
# ========================================
p <- ggplot(data = mapa_antioquia) +
  geom_sf(aes(fill = as.factor(ClusterMuni))) + 
  scale_fill_brewer(palette = "Blues", name = "Clúster Municipio") +
  theme_minimal() + 
  labs(title = "Distribución de clústeres por municipio en Antioquia")

print(p)

# ========================================
# 8. Boxplot del puntaje global por clúster
# ========================================
ggplot(datos, aes(x = factor(ClusterMuni), y = PUNT_GLOBAL, fill = factor(ClusterMuni))) +
  geom_boxplot(alpha = 0.6, outlier.size = 1.5, outlier.shape = 16) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Clúster", y = "Puntaje Global") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

