# =========================================
# 1. CARGA DE MODELOS AJUSTADOS
# =========================================

load("sub_1.RData"); fit1 <- fit
load("sub_2.RData"); fit2 <- fit
load("sub_base3.RData"); fit3 <- fit
load("sub_base4.RData"); fit4 <- fit
load("sub_base5.RData"); fit5 <- fit
load("completo.RData"); completo <- fit

fits <- list(fit1, fit2)

# =========================================
# 2. COMPARACIÓN DE PARÁMETROS ESTIMADOS
# =========================================

parametros_adicionales <- c("sigma_cole", "mu_global", "sigma_global", "sigma", "beta")
num_alphas <- 20
num_mus <- 5
num_parametros_adicionales <- length(parametros_adicionales)

resultados <- matrix(NA, nrow = num_alphas + num_mus + num_parametros_adicionales, ncol = 2)

for (i in 1:(num_alphas + num_mus)) {
  parametro <- if (i <= num_alphas) paste0("alpha[", i, "]") else paste0("mu[", i - num_alphas, "]")
  for (j in 1:2) {
    resumen <- rstan::summary(fits[[j]])$summary
    if (parametro %in% rownames(resumen)) resultados[i, j] <- resumen[parametro, "mean"]
  }
}

for (k in 1:num_parametros_adicionales) {
  parametro <- parametros_adicionales[k]
  index <- num_alphas + num_mus + k
  for (j in 1:2) {
    resumen <- rstan::summary(fits[[j]])$summary
    if (parametro %in% rownames(resumen)) resultados[index, j] <- resumen[parametro, "mean"]
  }
}

nombres_parametros <- c(paste0("alpha[", 1:num_alphas, "]"), paste0("mu[", 1:num_mus, "]"), parametros_adicionales)
rownames(resultados) <- nombres_parametros
promedios_filas <- rowMeans(resultados, na.rm = TRUE)
tabla_resultados <- data.frame(mean = promedios_filas)
print(tabla_resultados)

# =========================================
# 3. SIMULACIÓN DE PREDICCIONES (Y_HAT)
# =========================================
library(extraDistr)

simular_yhat_laplace <- function(fit) {
  mu <- rstan::extract(fit, pars = "mu")$mu
  sigma <- rstan::extract(fit, pars = "sigma")$sigma
  matrix(sapply(1:nrow(mu), function(i) {
    rlaplace(ncol(mu), mu[i, ], sigma[i] / sqrt(2))
  }), nrow = nrow(mu), byrow = TRUE)
}

samples_completo_yhat <- simular_yhat_laplace(completo)
samples_particiones_yhat <- lapply(fits, simular_yhat_laplace)
samples_mezcla_yhat <- Reduce("+", samples_particiones_yhat) / length(samples_particiones_yhat)

n_muestras <- min(nrow(samples_completo_yhat), nrow(samples_mezcla_yhat))
samples_completo_yhat <- samples_completo_yhat[1:n_muestras, ]
samples_mezcla_yhat <- samples_mezcla_yhat[1:n_muestras, ]

# =========================================
# 4. VISUALIZACIÓN COMPARACIÓN DE Y_HAT
# =========================================
max_obs <- min(50, ncol(samples_completo_yhat))
observaciones <- 1:max_obs

library(tidyr)
library(ggplot2)

df_multi <- lapply(observaciones, function(i) {
  completo_vec <- samples_completo_yhat[, i]
  mezcla_vec <- samples_mezcla_yhat[, i]
  ic_completo <- quantile(completo_vec, c(0.025, 0.975))
  ic_mezcla <- quantile(mezcla_vec, c(0.025, 0.975))
  data.frame(
    muestra = 1:n_muestras,
    completo = completo_vec,
    mezcla = mezcla_vec,
    observacion = paste("Obs", i)
  ) %>%
    pivot_longer(cols = c("completo", "mezcla"), names_to = "modelo", values_to = "valor") %>%
    mutate(
      li_95 = ifelse(modelo == "completo", ic_completo[1], ic_mezcla[1]),
      ls_95 = ifelse(modelo == "completo", ic_completo[2], ic_mezcla[2])
    )
}) %>% bind_rows()

ggplot(df_multi, aes(x = valor, fill = modelo)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = li_95, color = modelo), linetype = "dashed") +
  geom_vline(aes(xintercept = ls_95, color = modelo), linetype = "dashed") +
  facet_wrap(~observacion, scales = "free") +
  labs(
    title = "Distribuciones predictivas (modelo completo vs mezcla)",
    x = "Predicción simulada",
    y = "Densidad",
    fill = "Modelo"
  ) + theme_minimal(base_size = 14)
# =========================================
# 5. COMPARACIÓN GRAFICA DE PARAMETROS
# =========================================
modelo_completo_alpha <- c(407.02, 386.70, 395.81, 400.64, 404.40, 407.27, 401.68, 414.73, 386.83, 394.86, 
                           390.55, 398.08, 409.17, 420.76, 401.31, 396.83, 391.65, 403.68, 398.71, 421.42)
modelo_completo_mu <- c(399.79, 403.61, 397.76, 403.61, 402.34)
aproximacion_alpha <- c(401.48, 386.20, 392.73, 396.92, 400.56, 402.89, 397.87, 406.43, 384.41, 391.58, 
                        389.12, 393.73, 403.96, 412.03, 398.73, 394.92, 390.00, 399.29, 396.16, 413.65)
aproximacion_mu <- c(395.92, 399.45, 393.57, 399.68, 398.53)

# Gráfico para alpha
library(ggplot2)
datos_alpha <- data.frame(
  Componente = factor(paste0("alpha[", 1:20, "]"), levels = paste0("alpha[", 1:20, "]")),
  Modelo_Completo = modelo_completo_alpha,
  Aproximacion = aproximacion_alpha
)

ggplot(datos_alpha, aes(x = Componente)) +
  geom_point(aes(y = Modelo_Completo, color = "Modelo Completo"), size = 3) +
  geom_point(aes(y = Aproximacion, color = "Aproximación"), size = 3) +
  scale_color_manual(values = c("Modelo Completo" = "#104E8B", "Aproximación" = "#00BFFF")) +
  theme(axis.text.x = element_text(angle = 90), legend.title = element_blank())

# Gráfico para mu
library(ggplot2)
datos_mu <- data.frame(
  Componente = factor(paste0("mu[", 1:5, "]"), levels = paste0("mu[", 1:5, "]")),
  Modelo_Completo = modelo_completo_mu,
  Aproximacion = aproximacion_mu
)

grafico_mu <- ggplot(datos_mu, aes(x = Componente)) +
  geom_point(aes(y = Modelo_Completo, color = "Modelo Completo"), size = 5) +
  geom_point(aes(y = Aproximacion, color = "Aproximación"), size = 5) +
  scale_color_manual(values = c("Modelo Completo" = "#104E8B", "Aproximación" = "#00BFFF")) +
  theme(axis.text.x = element_text(angle = 90), legend.title = element_blank()) +
  labs(x = "", y = "")

print(grafico_mu)

# =========================================
# 6. DENSIDADES POSTERIORES
# =========================================
extraer_muestras <- function(fit, parametro) {
  posterior <- as.matrix(fit, pars = parametro)
  return(posterior)
}

parametro <- "mu[4]"
muestras_completo <- extraer_muestras(completo, parametro)
muestras_particiones <- lapply(fits, function(fit) extraer_muestras(fit, parametro))
muestras_promedio <- Reduce("+", muestras_particiones) / length(fits)

data_completo <- data.frame(muestra = muestras_completo[, 1], modelo = "Completo")
data_promedio <- data.frame(muestra = muestras_promedio[, 1], modelo = "Promedio Mixtura")
data_final <- rbind(data_completo, data_promedio)

ggplot(data_final, aes(x = muestra, fill = modelo, color = modelo)) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(title = parametro, x = "", y = "Densidad")

