# Librerías
library(jmuOutlier)
library(rstan)
library(ggplot2)
library(ggridges)

# Parámetros generales
N <- 1000              # Número de observaciones
set.seed(50)
x <- runif(5, 0, 4)    # Variable predictora
tau <- 0.8             # Cuantil deseado

##########################
# 1. SIMULACIÓN LAPLACE
##########################
beta <- 2
beta0 <- 1

y.simu_laplace <- sapply(x, function(xi) {
  set.seed(50)
  errors <- rlaplace(N, 0, 1)
  y <- beta0 + beta * xi + errors
  return(y)
})

# Preparación de datos para Stan
y_laplace <- as.vector(y.simu_laplace)
x1_laplace <- rep(x, each = N)
stan_data_laplace <- list(N = length(y_laplace), x = x1_laplace, y = y_laplace, tau = tau)

# Ajuste modelo Stan
fit_laplace <- stan(
  data = stan_data_laplace,
  file = "regresion_cuantilica_simple.stan",
  chains = 2,
  iter = 8000,
  core = 2
)

fit_laplace

save(fit_laplace, file = "laplace08_simu1.RData")

# Valores reales para tau = 0.8
real_laplace <- apply(y.simu_laplace, 2, quantile, 0.8)

##########################
# 2. SIMULACIÓN NORMAL
##########################
y.simu_normal <- sapply(x, function(xi) {
  set.seed(50)
  errors <- rnorm(N, 0, 1)
  y <- beta0 + beta * xi + errors
  return(y)
})

##########################
# 3. SIMULACIÓN GAMMA
##########################
beta <- 0.5
beta0 <- 0.4

y.simu_gamma <- sapply(x, function(xi) {
  set.seed(50)
  mean_y <- exp(beta0 + beta * xi)
  shape <- (mean_y^2) / 0.3
  scale <- 0.3 / mean_y
  ys <- rgamma(N, shape = shape, scale = scale)
  return(ys)
})

# Preparación de datos para Stan
y_gamma <- as.vector(y.simu_gamma)
x1_gamma <- rep(x, each = N)
stan_data_gamma <- list(N = length(y_gamma), x = x1_gamma, y = y_gamma, tau = tau)

# Ajuste modelo Stan
fit_gamma <- stan(
  data = stan_data_gamma,
  file = "regresion_cuantilica_simple.stan",
  chains = 2,
  iter = 8000,
  core = 2
)

save(fit_gamma, file = "gamma08_simu1.RData")

##########################
# 4. VISUALIZACIÓN GAMMA
##########################
datos_gamma <- data.frame(x = round(x1_gamma, 2), y = y_gamma)

ggplot(datos_gamma, aes(x = y, y = x, group = x)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(height = 0),
    point_size = 1.5,
    point_shape = 1,
    alpha = 0.3,
    scale = 0.9,
    fill = "#ADD8E6"
  ) +
  stat_function(fun = function(x) (-1.02 + x)/2, aes(color = "0.5")) +
  stat_function(fun = function(x) (-0.18 + x)/2, aes(color = "0.2")) +
  stat_function(fun = function(x) (-1.83 + x)/2, aes(color = "0.8")) +
  scale_color_manual(values = c("0.5" = "red", "0.2" = "blue", "0.8" = "purple")) +
  scale_fill_viridis_d() +
  coord_flip() +
  scale_y_continuous(
    limits = c(0.5, 3.8),
    breaks = sort(x),
    labels = c(expression(x[1]), expression(x[2]), expression(x[3]), expression(x[4]), expression(x[5]))
  ) +
  scale_x_continuous(limits = c(-1, 11)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "",
    x = "Respuesta Gamma (Y)",
    y = "",
    color = expression(tau)
  ) +
  theme(plot.title = element_text(hjust = 0.5))

