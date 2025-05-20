# ==============================================
# 1. LIBRERÍAS
# ==============================================
library(tidyverse)
library(lubridate)
library(viridis)
library(Hmisc)
library(FactoMineR)
library(dbscan)
library(factoextra)
library(cluster)
library(MixSim)
library(readr)

# ==============================================
# 2. CARGA Y PREPARACIÓN DE DATOS
# ==============================================
datos <- read_delim("SB11_20232.TXT",
                    delim = "¬", escape_double = FALSE, trim_ws = TRUE)

datos <- subset(datos, ESTU_DEPTO_PRESENTACION == "ANTIOQUIA")
datos <- datos %>%
  mutate(ESTU_FECHANACIMIENTO = dmy(ESTU_FECHANACIMIENTO),
         EDAD_2023 = as.period(interval(ESTU_FECHANACIMIENTO, ymd("2023-08-01")), unit = "year")$year) %>%
  filter(EDAD_2023 >= 15 & EDAD_2023 <= 40) %>%
  group_by(COLE_NOMBRE_ESTABLECIMIENTO) %>%
  filter(n() > 49) %>%
  ungroup() %>%
  filter(FAMI_ESTRATOVIVIENDA != "Sin Estrato",
         !is.na(COLE_COD_DANE_ESTABLECIMIENTO),
         !is.na(ESTU_MCPIO_RESIDE),
         !is.na(PUNT_GLOBAL)) %>%
  mutate(across(where(is.character), as.factor))

# ==============================================
# 3. CLUSTERING DE MUNICIPIOS CON K-MEDOIDS
# ==============================================
datos_agrupados <- datos %>%
  group_by(ESTU_COD_MCPIO_PRESENTACION) %>%
  summarise(Puntaje_Medio = median(PUNT_GLOBAL, na.rm = TRUE))

# Método del codo
wcss <- sapply(1:15, function(k) {
  set.seed(999)
  sum(kmeans(datos_agrupados$Puntaje_Medio, centers = k)$withinss)
})
ggplot(data.frame(k = 1:15, WCSS = wcss), aes(x = k, y = WCSS)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(x = "Número de clusters (k)", y = "WCSS")

# Método de la silueta
silhouette_scores <- sapply(2:15, function(k) {
  set.seed(999)
  km <- kmeans(datos_agrupados$Puntaje_Medio, centers = k)
  mean(silhouette(km$cluster, dist(datos_agrupados$Puntaje_Medio))[, 3])
})
ggplot(data.frame(k = 2:15, Silhouette = silhouette_scores), aes(x = k, y = Silhouette)) +
  geom_line() + geom_point() +
  labs(x = "Número de clusters (k)", y = "Ancho promedio de silueta") +
  theme_minimal()

# K-Medoids con PAM
k <- 4
matriz_distancias <- as.matrix(dist(as.matrix(datos_agrupados$Puntaje_Medio)))
n <- nrow(matriz_distancias)
v <- sapply(1:n, function(j) sum(matriz_distancias[, j]) / sum(rowSums(matriz_distancias)))
medoids_iniciales <- order(v)[1:k]
modelo_kmedoids <- pam(matriz_distancias, k = k, medoids = medoids_iniciales)
datos_agrupados$ClusterMuni <- as.factor(modelo_kmedoids$clustering)

datos <- left_join(datos, datos_agrupados, by = "ESTU_COD_MCPIO_PRESENTACION")

# ==============================================
# 4. VISUALIZACIÓN DEL CLUSTERING
# ==============================================
ggplot(datos_agrupados, aes(x = factor(ESTU_COD_MCPIO_PRESENTACION), y = Puntaje_Medio, color = ClusterMuni)) +
  geom_point(size = 4, alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Cluster de Municipios", x = "Municipio", y = "Puntaje Medio")

fviz_cluster(modelo_kmedoids, data = as.matrix(datos_agrupados$Puntaje_Medio),
             geom = "point", ellipse.type = "norm", main = "")

fviz_silhouette(silhouette(modelo_kmedoids))

fviz_silhouette(silhouette(modelo_kmedoids),
                palette = c("#FF6A6A80", "#A2CD5A80", "#54FF9F80", "#00BFFF80", "#FF3E9680"),
                label = FALSE) +
  theme_minimal() +
  labs(title = "Clusters Silhouette Plot") +
  geom_hline(yintercept = 0.59, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# ==============================================
# 5. SUBCLUSTERING POR COLEGIO DENTRO DE CADA MUNICIPIO
# ==============================================
datos_agrupados_colegios <- datos %>%
  group_by(ClusterMuni, COLE_NOMBRE_ESTABLECIMIENTO) %>%
  summarise(Puntaje_Medio_Colegio = median(PUNT_GLOBAL, na.rm = TRUE), .groups = "drop")

# Funciones para WCSS y silueta
calcular_wcss <- function(data, max_clusters) {
  sapply(1:max_clusters, function(k) {
    set.seed(999)
    sum(kmeans(data, centers = k)$withinss)
  })
}
calcular_silueta <- function(data, max_clusters) {
  sapply(2:max_clusters, function(k) {
    set.seed(999)
    mean(silhouette(kmeans(data, centers = k)$cluster, dist(data))[, 3])
  })
}


# Evaluación por cluster municipal
for (muni in unique(datos_agrupados_colegios$ClusterMuni)) {
  datos_muni <- datos_agrupados_colegios %>%
    filter(ClusterMuni == muni) %>%
    pull(Puntaje_Medio_Colegio)
  
  if (length(datos_muni) > 1) {
    wcss <- calcular_wcss(datos_muni, 15)
    silhouette_scores <- calcular_silueta(datos_muni, 15)
    
    # Graficar método del codo
    elbow_plot <- ggplot(data.frame(k = 1:15, WCSS = wcss), aes(k, WCSS)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Elbow Municipio", muni))
    
    ggsave(paste0("elbow_plot_muni_", muni, ".pdf"), plot = elbow_plot, width = 8, height = 6)
    
    # Graficar método de la silueta
    silhouette_plot <- ggplot(data.frame(k = 2:15, Silhouette = silhouette_scores), aes(k, Silhouette)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Silhouette Municipio", muni))
    
    ggsave(paste0("silhouette_plot_muni_", muni, ".pdf"), plot = silhouette_plot, width = 8, height = 6)
  } else {
    message(paste("Municipio", muni, "tiene un solo colegio o menos, no se puede clusterizar."))
  }
}

# Subclustering manual por municipio (puedes parametrizar esto si quieres)
assignar_cluster_colegios <- function(muni_data, k, offset) {
  resultado <- pam(muni_data$Puntaje_Medio_Colegio, k = k)
  muni_data$colegio <- resultado$clustering + offset
  return(muni_data)
}

datos_muni1 <- datos_agrupados_colegios %>%
  filter(ClusterMuni == 1) %>%
  assignar_cluster_colegios(., k = 4, offset = 0)

datos_muni2 <- datos_agrupados_colegios %>%
  filter(ClusterMuni == 2) %>%
  assignar_cluster_colegios(., k = 3, offset = 4)

datos_muni3 <- datos_agrupados_colegios %>%
  filter(ClusterMuni == 3) %>%
  assignar_cluster_colegios(., k = 3, offset = 7)

datos_muni4 <- datos_agrupados_colegios %>%
  filter(ClusterMuni == 4) %>%
  assignar_cluster_colegios(., k = 4, offset = 10)

########################## Graficos ########################################

p <- ggplot(datos_muni4, aes(x = as.factor(COLE_NOMBRE_ESTABLECIMIENTO), 
                             y = Puntaje_Medio_Colegio, 
                             color = as.factor(colegio))) +
  geom_point(alpha = 0.8, size = 4) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Mediana puntaje global", color = "Cluster Colegio") + # Cambia el nombre del eje Y y leyenda
  theme(
    panel.background = element_rect(fill = "white", color = NA), # Fondo del panel blanco
    plot.background = element_rect(fill = "white", color = NA),  # Fondo general blanco
    axis.text.x = element_blank(),      # Quitar las etiquetas del eje x
    axis.ticks.x = element_blank(),     # Quitar las marcas (ticks) del eje x
    axis.title.x = element_blank(),     # Quitar el título del eje x
    panel.grid.major = element_blank(), # Quitar cuadrícula mayor
    panel.grid.minor = element_blank()  # Quitar cuadrícula menor
  )

p

# Combinar todos
df_combined <- bind_rows(datos_muni1, datos_muni2, datos_muni3, datos_muni4)

datos <- datos %>%
  left_join(df_combined, by = c("COLE_NOMBRE_ESTABLECIMIENTO", "ClusterMuni")) %>%
  filter(!is.na(FAMI_TIENEINTERNET), !is.na(FAMI_ESTRATOVIVIENDA), !is.na(ESTU_HORASSEMANATRABAJA))

# ==============================================
# 6. GUARDADO FINAL
# ==============================================
write.csv(datos, "completos_datos_clusterizados.csv", row.names = FALSE)
