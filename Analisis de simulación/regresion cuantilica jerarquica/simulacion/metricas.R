# ================================================
# 1. LIBRERÍAS
# ================================================
library(dplyr)
library(ggplot2)
library(rstan)
library(tidyr)

# ================================================
# 2. GRÁFICOS DE AJUSTE PARA UN ESCENARIO (escenario 5)
# ================================================
cuantiles <- c(0.5, 0.25, 0.95)
labels <- c("05", "025", "095")

for (j in seq_along(cuantiles)) {
  q <- cuantiles[j]
  etiqueta <- labels[j]
  
  # Cuantil real por municipio
  cuantil_municipio <- data %>%
    group_by(Municipio) %>%
    summarise(cuantil_real = quantile(Y_L, q), .groups = "drop")
  
  # Cargar modelo ajustado
  load(paste0("escenario 5/escenario5_", etiqueta, ".RData"))
  posterior_samples <- rstan::extract(fit)
  mu_values <- posterior_samples$mu
  
  # Estadísticas de mu
  cuantil_municipio <- cuantil_municipio %>%
    mutate(
      mu_mean  = apply(mu_values, 2, mean),
      mu_lower = apply(mu_values, 2, function(x) quantile(x, 0.025)),
      mu_upper = apply(mu_values, 2, function(x) quantile(x, 0.975))
    )
  
  # Gráfico
  muni_plot <- ggplot(cuantil_municipio, aes(x = factor(Municipio))) +
    geom_ribbon(aes(ymin = mu_lower, ymax = mu_upper), fill = "#1E90FF", alpha = 0.2) +
    geom_line(aes(y = cuantil_real, color = "Real"), size = 3) +
    geom_point(aes(y = cuantil_real, color = "Real"), size = 6) +
    geom_line(aes(y = mu_mean, color = "Estimado"), linetype = "dashed", size = 3) +
    geom_point(aes(y = mu_mean, color = "Estimado"), size = 6) +
    scale_color_manual(values = c("Real" = "#104E8B", "Estimado" = "#1E90FF")) +
    scale_x_discrete(labels = parse(text = paste0("mu[", 1:length(unique(cuantil_municipio$Municipio)), "]"))) +
    labs(x = "", y = paste0("Cuantil ", q), color = "") +
    theme_minimal(base_size = 18) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 30),
      legend.title = element_text(size = 30),
      axis.text.x = element_text(size = 30),
      axis.text.y = element_text(size = 25),
      axis.title = element_text(size = 30),
      panel.grid.minor = element_blank()
    )
  
  ggsave(paste0("escenario5_muni_", etiqueta, ".pdf"),
         plot = muni_plot, width = 12, height = 6, dpi = 300)
}

# ================================================
# 3. MÉTRICAS GLOBALES POR ESCENARIO Y MUNICIPIO
# ================================================
escenarios <- 1:9
cuantiles  <- c(0.25, 0.5, 0.95)
labels     <- c("025", "05", "095")

resumen_metricas_global <- data.frame()

for (e in escenarios) {
  for (j in seq_along(cuantiles)) {
    q <- cuantiles[j]
    etiqueta <- labels[j]
    
    cuantil_municipio <- data %>%
      group_by(Municipio) %>%
      summarise(cuantil_real = quantile(Y_L, q), .groups = "drop")
    
    load(paste0("/escenario ", e, "/escenario", e, "_", etiqueta, ".RData"))
    mu_values <- rstan::extract(fit)$mu
    
    mu_mean  <- apply(mu_values, 2, mean)
    mu_lower <- apply(mu_values, 2, function(x) quantile(x, 0.025))
    mu_upper <- apply(mu_values, 2, function(x) quantile(x, 0.975))
    
    cuantil_municipio <- cuantil_municipio %>%
      mutate(
        mu_mean    = mu_mean,
        mu_lower   = mu_lower,
        mu_upper   = mu_upper,
        error      = mu_mean - cuantil_real,
        error_abs  = abs(error),
        error_sq   = error^2,
        error_pct  = abs(error / cuantil_real),
        dentro_int = cuantil_real >= mu_lower & cuantil_real <= mu_upper
      )
    
    resumen_metricas_global <- bind_rows(resumen_metricas_global, tibble(
      Escenario = e,
      Cuantil = q,
      MAE = mean(cuantil_municipio$error_abs),
      RMSE = sqrt(mean(cuantil_municipio$error_sq)),
      MAPE = mean(cuantil_municipio$error_pct) * 100,
      Sesgo = mean(cuantil_municipio$error),
      SD_Error = sd(cuantil_municipio$error),
      Cobertura_95 = mean(cuantil_municipio$dentro_int) * 100
    ))
  }
}

write.csv(resumen_metricas_global, "resumen_metricas_global.csv", row.names = FALSE)

# ================================================
# 4. MÉTRICAS GLOBALES POR ESCENARIO Y COLEGIO
# ================================================
resumen_metricas_alpha <- data.frame()

for (e in escenarios) {
  for (j in seq_along(cuantiles)) {
    q <- cuantiles[j]
    etiqueta <- labels[j]
    
    cuantil_colegio <- data %>%
      group_by(Colegio) %>%
      summarise(cuantil_real = quantile(Y_L, q), .groups = "drop")
    
    load(paste0("/escenario ", e, "/escenario", e, "_", etiqueta, ".RData"))
    alpha_values <- rstan::extract(fit)$alpha
    
    alpha_mean  <- apply(alpha_values, 2, mean)
    alpha_lower <- apply(alpha_values, 2, function(x) quantile(x, 0.025))
    alpha_upper <- apply(alpha_values, 2, function(x) quantile(x, 0.975))
    
    cuantil_colegio <- cuantil_colegio %>%
      mutate(
        alpha_mean  = alpha_mean,
        alpha_lower = alpha_lower,
        alpha_upper = alpha_upper,
        error       = alpha_mean - cuantil_real,
        error_abs   = abs(error),
        error_sq    = error^2,
        error_pct   = abs(error / cuantil_real),
        dentro_int  = ifelse(cuantil_real >= alpha_lower & cuantil_real <= alpha_upper, 1, 0)
      )
    
    resumen_metricas_alpha <- bind_rows(resumen_metricas_alpha, tibble(
      Escenario = e,
      Cuantil = q,
      MAE = mean(cuantil_colegio$error_abs),
      RMSE = sqrt(mean(cuantil_colegio$error_sq)),
      MAPE = mean(cuantil_colegio$error_pct) * 100,
      Sesgo = mean(cuantil_colegio$error),
      SD_Error = sd(cuantil_colegio$error),
      Cobertura_95 = mean(cuantil_colegio$dentro_int) * 100
    ))
  }
}

write.csv(resumen_metricas_alpha, "resumen_metricas_alpha_colegios.csv", row.names = FALSE)

# ================================================
# 5. VISUALIZACIÓN DE MÉTRICAS POR COLEGIO
# ================================================
resumen_long_alpha <- resumen_metricas_alpha %>%
  pivot_longer(cols = c(MAE, RMSE, MAPE, Sesgo, SD_Error, Cobertura_95),
               names_to = "Metrica", values_to = "Valor")

metrica_labels <- c(
  MAE = "Error Absoluto Medio (MAE)",
  RMSE = "Raíz del Error Cuadrático Medio (RMSE)",
  MAPE = "Error Porcentual Absoluto Medio (MAPE%)",
  Sesgo = "Sesgo Medio",
  SD_Error = "Desviación Estándar del Error",
  Cobertura_95 = "Cobertura del 95%"
)

for (met in unique(resumen_long_alpha$Metrica)) {
  plot_data <- resumen_long_alpha %>% filter(Metrica == met)
  
  p <- ggplot(plot_data, aes(x = factor(Cuantil), y = Valor, fill = factor(Escenario))) +
    geom_col(position = position_dodge(0.7), width = 0.6) +
    labs(title = metrica_labels[met], x = "Cuantil", y = "Valor", fill = "Escenario") +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 18),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  ggsave(paste0("grafico_alpha_", met, ".png"), plot = p, width = 8, height = 5, dpi = 300)
  print(p)
}
