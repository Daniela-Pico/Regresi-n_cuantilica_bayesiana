# ---------------------------
# LIBRERÍAS
# ---------------------------
library(dplyr)
library(extraDistr)
library(ggplot2)
library(rstan)

# ---------------------------
# 1. SIMULACIÓN JERÁRQUICA
# ---------------------------

# 1.1 Municipios (5)
set.seed(20)
mu <- 400
sigma_m <- 30
Mu_m <- rnorm(5, mu, sigma_m)

# 1.2 Colegios por municipio (20)
set.seed(10)
sigma_c <- 25
Mu_c <- sapply(Mu_m, function(i) rnorm(20, i, sigma_c))  # 20x5 matriz

# 1.3 Puntajes individuales (100 por colegio)
set.seed(50)
sigma <- 30
Y_icm_l <- apply(Mu_c, 2, function(mu_vec) {
  sapply(mu_vec, function(i) rlaplace(100, i, sigma))
})
Y_L <- as.vector(Y_icm_l)

# ---------------------------
# 2. VARIABLES Y DATA FRAME
# ---------------------------

# Identificadores jerárquicos
Municipio <- rep(1:5, each = 20 * 100)
Colegio   <- rep(1:20, each = 100, times = 5)

# Covariable continua: Edad
set.seed(30)
Edad <- ceiling(rnorm(10000, mean = 17, sd = 3))

# Construcción del dataset completo
data <- data.frame(Municipio, Colegio, Edad, Y = Y_L)

# ---------------------------
# 3. PARTICIONES ESTRATIFICADAS
# ---------------------------

# Barajar y crear 5 particiones
set.seed(456)
data <- data[sample(nrow(data)), ]

sub_bases <- data %>%
  group_by(Municipio, Colegio) %>%
  mutate(Partition = sample(rep(1:5, length.out = n()), replace = FALSE)) %>%
  ungroup() %>%
  split(.$Partition)

# Extraer sub-bases
sub_base1 <- as.data.frame(sub_bases[[1]])
sub_base2 <- as.data.frame(sub_bases[[2]])
sub_base3 <- as.data.frame(sub_bases[[3]])
sub_base4 <- as.data.frame(sub_bases[[4]])
sub_base5 <- as.data.frame(sub_bases[[5]])

# ---------------------------
# 4. PREPARACIÓN PARA STAN
# ---------------------------

datos_stan <- list(
  N = nrow(data),
  J = 20,
  K = 5,
  N2 = 20,
  cole = data$Colegio,
  col2 = 1:20,
  muni = data$Municipio,
  muni2 = rep(1:5, each = 4),  # ⚠️ Ajustar si necesario
  y = data$Y,
  x = data$Edad,
  tau = 0.5
)

# ---------------------------
# 5. AJUSTE DEL MODELO STAN
# ---------------------------

fit <- stan(
  data = datos_stan,
  file = "cuantilica_jerarquica",
  chains = 5,
  iter = 80000,
  cores = 5
)

print(fit)

# Tiempo de cómputo
sum(get_elapsed_time(fit))             # Total CPU time
max(rowSums(get_elapsed_time(fit)))    # Tiempo real con paralelización

# Guardar resultado
save(fit, file = "sub_2.RData")

# ---------------------------
# 6. GRÁFICO DE TIEMPOS DE MODELO POR PARTICIÓN
# ---------------------------

# 6.1 Gráfico de barras agrupado
df <- data.frame(
  Particiones = c("1a", "2a", "3a", "4a", "5a", "1b", "2b", "Datos completos"),
  tiempo = c(30, 11, 9, 8, 9, 40, 41, 89),
  grupo = c(rep("con_n_part", 5), rep("subconjuntos", 2), "Datos completos")
)

ggplot(df, aes(x = Particiones, y = tiempo, fill = grupo)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c("con_n_part" = "skyblue", "subconjuntos" = "orange", "Datos completos" = "tomato")) +
  labs(
    title = "Comparación de tiempos de ajuste por partición",
    x = "Modelo (número de particiones)",
    y = "Tiempo (minutos)",
    fill = "Grupo"
  ) +
  theme_minimal(base_size = 14)

# 6.2 Gráfico de línea comparativa
resumen <- data.frame(
  Modelo = factor(c("Datos completos", "2 Particiones", "5 Particiones"),
                  levels = c("Datos completos", "2 Particiones", "5 Particiones")),
  Tiempo_min = c(89, 81, 67)
)

tiempo_particiones <- ggplot(resumen, aes(x = Modelo, y = Tiempo_min, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 4) +
  geom_text(aes(label = paste0(Tiempo_min, " min")), hjust = -0.2, vjust = -0.5, size = 5) +
  labs(
    title = "",
    x = "",
    y = "Tiempo total (minutos)"
  ) +
  coord_cartesian(ylim = c(50, 100)) +
  theme_minimal(base_size = 14)

ggsave("tiempo_particiones.pdf", plot = tiempo_particiones, width = 8, height = 6, units = "in", dpi = 300)
