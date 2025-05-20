# --------------------------- LIBRERÍAS ---------------------------
library(ggplot2)
library(jmuOutlier)
library(rstan)
library(magrittr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstan)

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

################################GRAFICA PARA MUNICIPIOS ################################################################
cuantiles <- c(0.5, 0.25, 0.95)
labels <- c("05", "025", "095")
# Vector con los cuantiles y sus etiquetas
for(j in 1:length(cuantiles)){
  
  q <- cuantiles[j]
  etiqueta <- labels[j]
  
  # Resumen del cuantil por municipio
  cuantil_municipio <- data %>%
    group_by(Municipio) %>%
    summarise(cuantil_real = quantile(Y_L, q), .groups = 'drop')
  
  # Cargar los datos del ajuste
  load(paste0("escenario 1\\escenario1_", etiqueta, ".RData"))
  posterior_samples <- rstan::extract(fit, permuted = TRUE)
  alpha_values <- posterior_samples$alpha
  mu_values    <- posterior_samples$mu
  
  # Media e intervalos
  mu_mean  <- apply(mu_values, 2, mean)
  mu_lower <- apply(mu_values, 2, function(x) quantile(x,  0.025))  # límite inferior
  mu_upper <- apply(mu_values, 2, function(x) quantile(x, 0.975))  # límite superior
  
  # Añadir al data frame
  cuantil_municipio$mu_mean  <- mu_mean
  cuantil_municipio$mu_lower <- mu_lower
  cuantil_municipio$mu_upper <- mu_upper
  
  # Gráfico
  muni_plot <- ggplot(cuantil_municipio, aes(x = factor(Municipio))) +
    geom_ribbon(aes(ymin = mu_lower, ymax = mu_upper, group = 1), 
                fill = "#1E90FF", alpha = 0.2, linetype = "dotted", color = NA) +
    geom_line(aes(y = cuantil_real, color = "Real"), group = 1, size = 3) +
    geom_point(aes(y = cuantil_real, color = "Real"), size = 6) +
    geom_line(aes(y = mu_mean, color = "Estimado"), group = 1, linetype = "dashed", size = 3) +
    geom_point(aes(y = mu_mean, color = "Estimado"), size = 6) +
    labs(x = "",
         y = paste0("Cuantil ", q),
         title = "",
         color = "") +
    scale_color_manual(values = c("Real" = "#104E8B", "Estimado" = "#1E90FF")) +
    scale_x_discrete(labels = parse(text = paste0("mu[", 1:length(unique(cuantil_municipio$Municipio)), "]"))) +
    theme_minimal(base_size = 18) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 30),
      legend.title = element_text(size = 30),
      legend.key.size = unit(1.5, "lines"),
      axis.text.x = element_text(size = 30),
      axis.text.y = element_text(size = 25),
      axis.title.x = element_text(size = 30, face = "plain"),
      axis.title.y = element_text(size = 30, face = "plain"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  
  # Guardar gráfico
  ggsave(paste0("escenario5_muni_", etiqueta, ".pdf"),
         plot = muni_plot,
         width = 12, height = 6, units = "in", dpi = 300)
}


########################### GRAFICA PARA COLEGIOS ############################


library(dplyr)
library(ggplot2)
library(rstan)

cuantiles <- c(0.5, 0.25, 0.95)
labels <- c("05", "025", "095")

for(j in 1:length(cuantiles)){
  
  q <- cuantiles[j]
  etiqueta <- labels[j]
  
  cuantiles_por_colegio <- data %>%
    group_by(Municipio, Colegio) %>%
    summarise(cuantil_real = quantile(Y_L, q), .groups = 'drop')
  
  # Cargar el archivo con valores ajustados
  load(paste0("escenario 5/escenario5_",etiqueta, ".RData"))
  posterior_samples <- rstan::extract(fit, permuted = TRUE)
  alpha_values <- posterior_samples$alpha
  alpha_mean <- apply(alpha_values, 2, mean)
  
  cuantiles_por_colegio$alpha_mean <- alpha_mean
  
  cuantiles_por_colegio$alpha_lower <- apply(alpha_values, 2, function(x) quantile(x, 0.025))
  cuantiles_por_colegio$alpha_upper <- apply(alpha_values, 2, function(x) quantile(x, 0.975))
  cuantiles_por_colegio$alpha_mean  <- apply(alpha_values, 2, mean)
  
  
  # Gráfico
  colegio_plot <- ggplot(cuantiles_por_colegio, aes(x = 1:nrow(cuantiles_por_colegio))) +
    geom_ribbon(aes(ymin = alpha_lower, ymax = alpha_upper, group = 1),
                fill = "#1E90FF", alpha = 0.2, linetype = "dotted", color = NA) +
    geom_line(aes(y = cuantil_real, color = "Real"), group = 1, size = 3) +
    geom_point(aes(y = cuantil_real, color = "Real"), size = 6) +
    geom_line(aes(y = alpha_mean, color = "Estimado"), group = 1, linetype = "dashed", size = 3) +
    geom_point(aes(y = alpha_mean, color = "Estimado"), size = 6) +
    labs(x = "",
         y = paste0("Cuantil ", q),
         title = "",
         color = "") +
    scale_color_manual(values = c("Real" = "#104E8B", "Estimado" = "#1E90FF")) +
    scale_x_continuous(breaks = 1:nrow(cuantiles_por_colegio),
                       labels = parse(text = paste0("alpha[", 1:nrow(cuantiles_por_colegio), "]"))) +
    theme_minimal(base_size = 18) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 30),
      legend.title = element_text(size = 30),
      legend.key.size = unit(1.5, "lines"),
      axis.text.x = element_text(size = 30, angle = 90, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 25),
      axis.title.x = element_text(size = 30, face = "plain"),
      axis.title.y = element_text(size = 30, face = "plain"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  
  # Guardar el gráfico
  ggsave(paste0("escenario5_cole_", etiqueta, ".pdf"),
         plot = colegio_plot,
         width = 12, height = 6, units = "in", dpi = 300)
}