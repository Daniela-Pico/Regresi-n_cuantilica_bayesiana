
# --------------------------- LIBRERÍAS ---------------------------
library(ggplot2)
library(jmuOutlier)
library(rstan)
library(magrittr)
library(tidyverse)
library(dplyr)
library(tidyr)

# --------------------------- PARÁMETROS GLOBALES ---------------------------
mu <- 400
sigma_m <- 30
sigma_c <- 25
sigma <- 30

# --------------------------- SIMULACIÓN DE DATOS ---------------------------
set.seed(20)
Mu_m <- rnorm(3, mu, sigma_m)  # Medias de municipios

set.seed(10)
Mu_c <- sapply(Mu_m, function(i) rnorm(8, i, sigma_c))  # Medias de colegios por municipio

Municipio <- rep(1:3, each = 168)
Colegio <- rep(1:24, each = 21)

set.seed(30)
Edad <- ceiling(rnorm(504, mean = 17, sd = 3))  # Edad redondeada

# Simulación de respuestas Y ~ Laplace
set.seed(50)
Y_icm_l <- apply(Mu_c, 2, function(mu) sapply(mu, function(i) rlaplace(21, i, sigma)))
Y_L <- as.vector(Y_icm_l)

data <- data.frame(
  Municipio = Municipio,
  Colegio = Colegio,
  Y_L = Y_L,
  edad = Edad
)

col2 <- 1:24
muni2 <- rep(1:3, each = 8)

# --------------------------- DATOS PARA STAN ---------------------------
datos_stan <- list(
  N = 72,
  J = 24,
  K = 3,
  N2 = length(col2),
  cole = Colegio,
  col2 = col2,
  muni2 = muni2,
  muni = Municipio,
  y = data$Y_L,
  tau = 0.95,
  x = data$edad
)

# --------------------------- AJUSTE STAN ---------------------------
fit <- stan(
  data = datos_stan, 
  file = "cuanitlica_jerarquica",
  chains = 5,
  iter = 20000,
  cores = 5
)
save(fit, file = "escenario1_095_noinformativo_.RData")


# Simulación de respuestas Y ~ Gamma
set.seed(50)
Y_icm_g <- apply(Mu_c, 2, function(mu) sapply(mu, function(i) {
  shape <- i^2 / sigma^2
  scale <- sigma^2 / i
  rgamma(21, shape = shape, scale = scale)
}))
Y_G <- as.vector(Y_icm_g)

data <- data.frame(
  Municipio = Municipio,
  Colegio = Colegio,
  Y_G = Y_G,
  edad = Edad
)

col2 <- 1:24
muni2 <- rep(1:3, each = 8)

# --------------------------- DATOS PARA STAN ---------------------------
datos_stan <- list(
  N = 72,
  J = 24,
  K = 3,
  N2 = length(col2),
  cole = Colegio,
  col2 = col2,
  muni2 = muni2,
  muni = Municipio,
  y = data$Y_G,
  tau = 0.95,
  x = data$edad
)

# --------------------------- AJUSTE STAN ---------------------------
fit <- stan(
  data = datos_stan, 
  file = "cuanitlica_jerarquica",
  chains = 5,
  iter = 20000,
  cores = 5
)
save(fit, file = "escenario1_095_gamma.RData")

# Simulación de respuestas Y ~ Normal
set.seed(50)
Y_icm_n <- apply(Mu_c, 2, function(mu) sapply(mu, function(i) rnorm(21, mean = i, sd = sigma)))
Y_N <- as.vector(Y_icm_n)

data <- data.frame(
  Municipio = Municipio,
  Colegio = Colegio,
  Y_N = Y_N,
  edad = Edad
)

col2 <- 1:24
muni2 <- rep(1:3, each = 8)

# --------------------------- DATOS PARA STAN ---------------------------
datos_stan <- list(
  N = 72,
  J = 24,
  K = 3,
  N2 = length(col2),
  cole = Colegio,
  col2 = col2,
  muni2 = muni2,
  muni = Municipio,
  y = data$Y_N,
  tau = 0.95,
  x = data$edad
)

# --------------------------- AJUSTE STAN ---------------------------
fit <- stan(
  data = datos_stan, 
  file = "cuanitlica_jerarquica",
  chains = 5,
  iter = 20000,
  cores = 5
)
save(fit, file = "escenario1_095_normal.RData")