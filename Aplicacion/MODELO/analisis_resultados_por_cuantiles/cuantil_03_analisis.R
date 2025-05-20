### =========================================
### MODELO BAYESIANO - PARTICIÓN 03
### =========================================

# --- Librerías necesarias
library(rstan)
library(posterior)
library(dplyr)

# --- Paso 1: Cargar los modelos de la partición 03
load("particion1_modelo_completo_sub203.RData"); fit2 <- fit4
load("particion1_modelo_completo_sub303.RData"); fit3 <- fit6
load("particion1_modelo_completo_sub403.RData"); fit4 <- fit8
load("particion1_modelo_completo03.RData");     fit1 <- fit11

fits <- list(fit1, fit2, fit3, fit4)

# --- Paso 2: Aplicar burn-in y thinning
thin_factor <- 7
burnin_extra <- 10000

fits_thinned <- lapply(fits, function(fit) {
  posterior_array <- as.array(fit)
  iter_total <- dim(posterior_array)[1]
  if (burnin_extra >= iter_total) stop("El burn-in extra es mayor o igual al número total de iteraciones.")
  posterior_array_burned <- posterior_array[(burnin_extra + 1):iter_total, , ]
  posterior_thinned <- posterior_array_burned[seq(1, dim(posterior_array_burned)[1], by = thin_factor), , ]
  as_draws_df(as_draws_array(posterior_thinned))
})

# --- Paso 3: Definición de parámetros de interés
parametros <- c(
  paste0("alpha[", 1:14, "]"), "sigma_cole", paste0("mu[", 1:4, "]"),
  "mu_global", "sigma", "sigma_global", "beta_age",
  paste0("horas_effect[", 1:5, "]"), paste0("estrato_effect[", 1:6, "]"),
  paste0("internet_effect[", 1:2, "]"), paste0("educ_madre_effect[", 1:10, "]"),
  paste0("educ_padre_effect[", 1:10, "]"), "beta_naturaleza",
  paste0("horas_cen[", 1:5, "]"), paste0("estrato_cen[", 1:6, "]"),
  paste0("internet_cen[", 1:2, "]"), paste0("educ_madre_cen[", 1:10, "]"),
  paste0("educ_padre_cen[", 1:10, "]"), "lp__"
)

# --- Paso 4: Resumen estadístico de cada modelo
resumenes <- lapply(fits_thinned, function(fit) {
  summarise_draws(
    fit, mean, sd,
    ~quantile2(.x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)),
    ess_bulk, rhat
  )
})

# --- Paso 5: Consolidar resúmenes etiquetados por modelo
resumen_total <- bind_rows(
  resumenes[[1]] %>% mutate(fit = "fit1"),
  resumenes[[2]] %>% mutate(fit = "fit2"),
  resumenes[[3]] %>% mutate(fit = "fit3"),
  resumenes[[4]] %>% mutate(fit = "fit4")
)

# --- Paso 6: Calcular promedios y errores estándar
tabla_resultados <- resumen_total %>%
  group_by(variable) %>%
  summarise(
    mean = mean(mean, na.rm = TRUE),
    sd = mean(sd, na.rm = TRUE),
    `2.5%` = mean(`q2.5`, na.rm = TRUE),
    `25%` = mean(`q25`, na.rm = TRUE),
    `50%` = mean(`q50`, na.rm = TRUE),
    `75%` = mean(`q75`, na.rm = TRUE),
    `97.5%` = mean(`q97.5`, na.rm = TRUE),
    n_eff = mean(ess_bulk, na.rm = TRUE),
    Rhat = mean(rhat, na.rm = TRUE)
  ) %>%
  mutate(se_mean = sd / sqrt(n_eff)) %>%
  relocate(se_mean, .after = mean) %>%
  rename(parametro = variable) %>%
  mutate(across(where(is.numeric), round, 2))

# --- Exportar tabla
write.csv(tabla_resultados, "tabla_resultados_alpha_03.csv", row.names = FALSE)


################ graficas ############################

############# DISTRIBUCION POSTERIOR ################################
###################### SIGMA ######################################
####################################################################
library(ggplot2)
library(bayesplot)
library(dplyr)


df_sigma <- bind_rows(
  lapply(fits_thinned, function(fit) {
    data.frame(sigma = as.numeric(fit[["sigma"]]))
  })
)
# Gráfico de densidad posterior para sigma
# Gráfico de densidad posterior para sigma
plot_sigma <- ggplot(df_sigma, aes(x = sigma)) +
  geom_density(fill = NA, color = "black", size = 1.2) +
  stat_density(
    geom = "area", 
    fill = "#87CEFA", 
    alpha = 0.3,
    position = "identity",
    aes(y = after_stat(density))
  ) +
  labs(
    x = expression(sigma),
    y = ""
  ) +
  theme_minimal(base_size = 16)

ggsave("sigma_03.pdf", plot = plot_sigma, width = 8, height = 6, units = "in", dpi = 300)



###################### SIGMA_COLE ######################################
####################################################################

# --- Paso 1: Extraer las muestras de sigma_cole de todas las particiones
df_sigma_cole <- bind_rows(
  lapply(fits_thinned, function(fit) {
    data.frame(sigma_cole = as.numeric(fit[["sigma_cole"]]))
  })
)

# --- Paso 2: Crear el gráfico
plot_sigma_cole <- ggplot(df_sigma_cole, aes(x = sigma_cole)) +
  geom_density(fill = NA, color = "black", size = 1.2) +
  stat_density(
    geom = "area", 
    fill = "#87CEFA", 
    alpha = 0.3,
    position = "identity",
    aes(y = after_stat(density))
  ) +
  labs(
    x = expression(sigma[cole]),
    y = ""
  ) +
  theme_minimal(base_size = 16)

# --- Paso 3: Guardar el gráfico
ggsave("sigma_cole_03.pdf", plot = plot_sigma_cole, width = 8, height = 6, units = "in", dpi = 300)

#######################################################################
######## mu_global ##################################################


# --- Paso 1: Extraer las muestras de mu_global de todas las particiones
df_mu_global <- bind_rows(
  lapply(fits_thinned, function(fit) {
    data.frame(mu_global = as.numeric(fit[["mu_global"]]))
  })
)

# --- Paso 2: Calcular percentiles 2.5% y 97.5% (intervalo de credibilidad 95%)
quantiles_mu_global <- quantile(df_mu_global$mu_global, probs = c(0.025, 0.975))

# --- Paso 3: Crear el gráfico
plot_mu_global <- ggplot(df_mu_global, aes(x = mu_global)) +
  geom_density(fill = NA, color = "black", size = 1.2) +
  stat_density(
    geom = "area", 
    fill = "#87CEFA", 
    alpha = 0.3,
    position = "identity",
    aes(y = after_stat(density))
  ) +
  labs(
    x = expression(mu[global]),
    y = ""
  ) +
  theme_minimal(base_size = 16)

# --- Paso 4: Guardar el gráfico
ggsave("mu_global_03.pdf", plot = plot_mu_global, width = 8, height = 6, units = "in", dpi = 300)

################################## BETA AGE #######################################
###################################################################################

# --- Paso 1: Extraer las muestras de beta_age de todas las particiones
df_beta_age <- bind_rows(
  lapply(fits_thinned, function(fit) {
    data.frame(beta_age = as.numeric(fit[["beta_age"]]))
  })
)

# --- Paso 2: Calcular percentiles 2.5% y 97.5% (intervalo de credibilidad 95%)
quantiles_beta_age <- quantile(df_beta_age$beta_age, probs = c(0.025, 0.975))

# --- Paso 3: Crear el gráfico
plot_beta_age <- ggplot(df_beta_age, aes(x = beta_age)) +
  geom_density(fill = NA, color = "black", size = 1.2) +
  stat_density(
    geom = "area", 
    fill = "#87CEFA", 
    alpha = 0.3,
    position = "identity",
    aes(y = after_stat(density))
  )  + 
  labs(
    x = expression(beta[age]),
    y = ""
  ) +
  theme_minimal(base_size = 16)

# --- Paso 4: Guardar el gráfico
ggsave("edad_posterior_03.pdf", plot = plot_beta_age, width = 8, height = 6, units = "in", dpi = 300)



######################## MUNICIPIOS ####################################
############################################################


# --- Paso 1: Extraer muestras de mu[1] a mu[4] de todas las particiones
df_mu <- bind_rows(
  lapply(fits_thinned, function(fit) {
    data.frame(
      mu1 = as.numeric(fit[["mu[1]"]]),
      mu2 = as.numeric(fit[["mu[2]"]]),
      mu3 = as.numeric(fit[["mu[3]"]]),
      mu4 = as.numeric(fit[["mu[4]"]])
    )
  })
)

# --- Paso 2: Formato largo
df_mu_long <- df_mu %>%
  pivot_longer(cols = everything(), names_to = "municipio", values_to = "valor")

# --- Paso 3: Crear el gráfico
municipios <- ggplot(df_mu_long, aes(x = valor, fill = municipio)) +
  geom_density(alpha = 0.9, color = "black", size = 1.0) +
  labs(
    x = expression(mu[i]),
    y = "",
    fill = ""
  ) +
  scale_fill_brewer(palette = "Pastel1",
                    labels = c(expression(mu[1]), expression(mu[2]),
                               expression(mu[3]), expression(mu[4]))) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold")
  )

# --- Paso 4: Guardar el gráfico
ggsave("municipios_03.pdf", plot = municipios, width = 8, height = 6, units = "in", dpi = 300)


############################ COLEGIOS ########################################
#######################################################################

# --- Paso 1: Extraer muestras de alpha[1] a alpha[14] de todas las particiones
df_alpha <- bind_rows(
  lapply(fits_thinned, function(fit) {
    as.data.frame(
      sapply(1:14, function(i) {
        as.numeric(fit[[paste0("alpha[", i, "]")]])
      })
    ) %>%
      setNames(paste0("alpha", 1:14))
  })
)

# --- Paso 2: Pasar a formato largo
df_alpha_long <- df_alpha %>%
  pivot_longer(cols = everything(), names_to = "par", values_to = "valor")

# --- Paso 3: Configurar colores
colores_14 <- c(
  "#68228B", "palegreen", "darkslategray4", "deeppink4", "#FFFF00",
  "#104E8B", "#b3de69", "firebrick4", "gold4", "#008B00",
  "lightpink", "lightskyblue", "#FF00FF", "#9370DB"
)

# Asegurar orden correcto
df_alpha_long$par <- factor(df_alpha_long$par, levels = paste0("alpha", 1:14))

# Crear etiquetas matemáticas para la leyenda
label_alpha <- paste0("alpha[", 1:14, "]")

# --- Paso 4: Crear el gráfico
colegios <- ggplot(df_alpha_long, aes(x = valor, fill = par)) +
  geom_density(alpha = 0.6, color = "black", size = 0.8) +
  labs(
    x = expression(alpha[i]),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(
    values = colores_14,
    labels = parse(text = label_alpha),
    breaks = paste0("alpha", 1:14)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# --- Paso 5: Mostrar el gráfico
print(colegios)

# --- Paso 6: Guardarlo
ggsave("colegios_03.pdf", plot = colegios, width = 10, height = 7, units = "in", dpi = 300)


####################### ESTRATO #####################################
###################################################################


# --- Paso 1: Extraer muestras de estrato_cen[1] a estrato_cen[6] de todas las particiones
df_estrato <- bind_rows(
  lapply(fits_thinned, function(fit) {
    as.data.frame(
      sapply(1:6, function(i) {
        as.numeric(fit[[paste0("estrato_cen[", i, "]")]])
      })
    ) %>%
      setNames(paste0("estrato", 1:6))
  })
)

# --- Paso 2: Convertir a formato largo
df_estrato_long <- df_estrato %>%
  pivot_longer(cols = everything(), names_to = "estrato", values_to = "valor")

# --- Paso 3: Crear etiquetas matemáticas para leyenda
labels_estrato <- paste0("estrato[cen[", 1:6, "]]")

# --- Paso 4: Crear el gráfico
estrato <- ggplot(df_estrato_long, aes(x = valor, fill = estrato)) +
  geom_density(alpha = 0.7, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
  labs(
    x = expression(estrato[cen[i]]),
    y = "",
    fill = ""
  ) +
  scale_fill_brewer(
    palette = "Set2",
    labels = parse(text = labels_estrato)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold")
  )

# --- Paso 5: Mostrar
print(estrato)

# --- Paso 6: Guardarlo
ggsave("estrato_posterior_03.pdf", plot = estrato, width = 8, height = 6, units = "in", dpi = 300)

##################################### Internet ############################
#########################################################################


# --- Paso 1: Extraer muestras de internet_cen[1] e internet_cen[2] de todas las particiones
df_internet <- bind_rows(
  lapply(fits_thinned, function(fit) {
    data.frame(
      internet1 = as.numeric(fit[["internet_cen[1]"]]),
      internet2 = as.numeric(fit[["internet_cen[2]"]])
    )
  })
)

# --- Paso 2: Convertir a formato largo
df_internet_long <- df_internet %>%
  pivot_longer(cols = everything(), names_to = "nivel", values_to = "valor")

# --- Paso 3: Crear etiquetas matemáticas para la leyenda
labels_internet <- c("internet[cen[1]]", "internet[cen[2]]")

# --- Paso 4: Crear el gráfico
internet <- ggplot(df_internet_long, aes(x = valor, fill = nivel)) +
  geom_density(alpha = 0.7, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
  labs(
    x = expression(internet[cen[i]]),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(
    values = c("internet1" = "#00688B", "internet2" = "#009ACD"),
    labels = parse(text = labels_internet)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold")
  )

# --- Paso 5: Mostrar el gráfico
print(internet)

# --- Paso 6: Guardar el gráfico
ggsave("internet_03.pdf", plot = internet, width = 8, height = 6, units = "in", dpi = 300)



#############################BETA NATURALEZA ####################
#########################################################################



# --- Paso 1: Extraer muestras de beta_naturaleza de todas las particiones
df_naturaleza <- bind_rows(
  lapply(fits_thinned, function(fit) {
    data.frame(beta = as.numeric(fit[["beta_naturaleza"]]))
  })
)

# --- Paso 2: Crear el gráfico
plot_naturaleza <- ggplot(df_naturaleza, aes(x = beta)) +
  geom_density(fill = "#87CEFA", color = "black", alpha = 0.7, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
  labs(
    x = expression(beta[naturaleza]),
    y = "",
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16)
  )

# --- Paso 3: Mostrar el gráfico
print(plot_naturaleza)

# --- Paso 4: Guardarlo
ggsave("naturaleza_posterior_03.pdf", plot = plot_naturaleza, width = 8, height = 6, units = "in", dpi = 300)

############################## horas a la semana en que trabaja ########
#########################################################################

# --- Paso 1: Extraer muestras de horas_cen[1] a horas_cen[5] de todas las particiones
df_horas <- bind_rows(
  lapply(fits_thinned, function(fit) {
    as.data.frame(
      sapply(1:5, function(i) {
        as.numeric(fit[[paste0("horas_cen[", i, "]")]])
      })
    ) %>%
      setNames(paste0("horas", 1:5))
  })
)

# --- Paso 2: Convertir a formato largo
df_horas_long <- df_horas %>%
  pivot_longer(cols = everything(), names_to = "grupo", values_to = "valor")

# --- Paso 3: Crear etiquetas matemáticas para la leyenda
labels_horas <- paste0("horas[cen[", 1:5, "]]")

# --- Paso 4: Crear el gráfico
horas <- ggplot(df_horas_long, aes(x = valor, fill = grupo)) +
  geom_density(alpha = 0.7, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
  labs(
    x = expression(horas[cen[i]]),
    y = "",
    fill = ""
  ) +
  scale_fill_brewer(
    palette = "Set2",
    labels = parse(text = labels_horas)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold")
  )

# --- Paso 5: Mostrar el gráfico
print(horas)

# --- Paso 6: Guardarlo
ggsave("horas_03.pdf", plot = horas, width = 8, height = 6, units = "in", dpi = 300)


######################### EDU MADRE ##############################


# --- Paso 1: Extraer muestras de educ_madre_cen[1] a educ_madre_cen[10] de todas las particiones
df_madre <- bind_rows(
  lapply(fits_thinned, function(fit) {
    as.data.frame(
      sapply(1:10, function(i) {
        as.numeric(fit[[paste0("educ_madre_cen[", i, "]")]])
      })
    ) %>%
      setNames(paste0("madre", 1:10))
  })
)

# --- Paso 2: Formato largo y orden correcto
df_madre_long <- df_madre %>%
  pivot_longer(cols = everything(), names_to = "nivel", values_to = "valor") %>%
  mutate(nivel = factor(nivel, levels = paste0("madre", 1:10)))

# --- Paso 3: Crear etiquetas matemáticas
labels_madre <- paste0("educ_madre[cen[", 1:10, "]]")

# --- Paso 4: Crear el gráfico
madre <- ggplot(df_madre_long, aes(x = valor, fill = nivel)) +
  geom_density(alpha = 0.7, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
  labs(
    x = expression(educ_madre[cen[i]]),
    y = "",
  ) +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(10, "Set3"),
    labels = parse(text = labels_madre)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold")
  )

# --- Paso 5: Mostrar el gráfico
print(madre)

# --- Paso 6: Guardarlo
ggsave("madre_03.pdf", plot = madre, width = 8, height = 6, units = "in", dpi = 300)


############# EDU PADRE #######################
# --- Paso 1: Extraer muestras de educ_padre_cen[1] a educ_padre_cen[10] de todas las particiones
df_padre <- bind_rows(
  lapply(fits_thinned, function(fit) {
    as.data.frame(
      sapply(1:10, function(i) {
        as.numeric(fit[[paste0("educ_padre_cen[", i, "]")]])
      })
    ) %>%
      setNames(paste0("padre", 1:10))
  })
)

# --- Paso 2: Formato largo y orden correcto
df_padre_long <- df_padre %>%
  pivot_longer(cols = everything(), names_to = "nivel", values_to = "valor") %>%
  mutate(nivel = factor(nivel, levels = paste0("padre", 1:10)))

# --- Paso 3: Crear etiquetas matemáticas
labels_padre <- paste0("educ_padre[cen[", 1:10, "]]")

# --- Paso 4: Crear el gráfico
padre <- ggplot(df_padre_long, aes(x = valor, fill = nivel)) +
  geom_density(alpha = 0.7, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 1) +
  labs(
    x = expression(educ_padre[cen[i]]),
    y = "",
    fill = ""
  ) +
  scale_fill_manual(
    values = RColorBrewer::brewer.pal(10, "Set3"),
    labels = parse(text = labels_padre)
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.title = element_text(size = 20, face = "bold")
  )

# --- Paso 5: Mostrar el gráfico
print(padre)

# --- Paso 6: Guardarlo
ggsave("padre_03.pdf", plot = padre, width = 8, height = 6, units = "in", dpi = 300)



######################### ANOVA ############################333
###############################################################


library(dplyr)
library(tidyr)

library(posterior)

# --- Combinar las 4 particiones thinned en un solo objeto
posterior_mezcla <- as_draws_array(
  do.call(rbind, lapply(fits_thinned, as_draws_matrix))
)


# --- Paso 2: Identificar índices
param_idx <- function(prefix) {
  grep(paste0("^", prefix, "\\["), dimnames(posterior_mezcla)[[3]])
}

# --- Paso 3: Crear función para extraer matrices de muestras
extract_param_matrix <- function(indices) {
  do.call(cbind, lapply(indices, function(i) as.vector(posterior_mezcla[, , i])))
}

# --- Paso 4: Extraer muestras
alpha_samples          <- extract_param_matrix(param_idx("alpha"))
mu_samples             <- extract_param_matrix(param_idx("mu"))
horas_cen_samples      <- extract_param_matrix(param_idx("horas_cen"))
estrato_cen_samples    <- extract_param_matrix(param_idx("estrato_cen"))
internet_cen_samples   <- extract_param_matrix(param_idx("internet_cen"))
educ_madre_cen_samples <- extract_param_matrix(param_idx("educ_madre_cen"))
educ_padre_cen_samples <- extract_param_matrix(param_idx("educ_padre_cen"))

# --- Paso 5: Función para calcular S
calc_S_posterior <- function(samples) {
  apply(samples, 1, function(x) {
    n <- length(x)
    I_n <- diag(n)
    ones_n <- matrix(1, n, n)
    cov_matrix <- I_n - (1/n) * ones_n
    sqrt((1 / (n - 1)) * t(x) %*% cov_matrix %*% x)
  })
}

# --- Paso 6: Calcular los S para cada fuente de variabilidad
S_list <- list(
  colegio      = calc_S_posterior(alpha_samples),
  municipio    = calc_S_posterior(mu_samples),
  horas        = calc_S_posterior(horas_cen_samples),
  estrato      = calc_S_posterior(estrato_cen_samples),
  internet     = calc_S_posterior(internet_cen_samples),
  educ_madre   = calc_S_posterior(educ_madre_cen_samples),
  educ_padre   = calc_S_posterior(educ_padre_cen_samples)
)

# --- Paso 7: Imprimir resultados
cat("Resultados de S (mediana e intervalo creíble 95%):\n\n")
for (name in names(S_list)) {
  s_samples <- S_list[[name]]
  s_median <- median(s_samples)
  s_ci <- quantile(s_samples, probs = c(0.025, 0.975))  # 95% intervalo creíble
  cat(paste0("S_", name, ": ", round(s_median, 2), " (", round(s_ci[1], 2), ", ", round(s_ci[2], 2), ")\n"))
}


#
library(ggplot2)

# --- Paso 1: Lista de parámetros
parametros <- c(
  paste0("alpha[", 1:14, "]"),
  paste0("mu[", 1:4, "]"),
  "mu_global", "sigma", "sigma_cole", "sigma_global", "beta_age", "beta_naturaleza",
  paste0("educ_madre_cen[", 1:10, "]"),
  paste0("educ_padre_cen[", 1:10, "]"),
  paste0("estrato_cen[", 1:6, "]"),
  paste0("horas_cen[", 1:5, "]"),
  paste0("internet_cen[", 1:2, "]")
)

# --- Paso 2: Carpeta donde guardar los plots
dir.create("ACF_plots", showWarnings = FALSE)

# --- Paso 3: Bucle para graficar y guardar ACF de cada parámetro
for (param in parametros) {
  
  # Extraer las muestras de la primera partición (o podrías combinar si quieres)
  muestras <- as.numeric(fits_thinned[[1]][[param]])
  
  # Calcular ACF
  acf_result <- acf(muestras, plot = FALSE)
  
  # Preparar los datos
  acf_data <- data.frame(
    Lag = acf_result$lag,
    ACF = acf_result$acf
  )
  
  # Crear gráfico
  plot_acf <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
    geom_bar(stat = "identity", width = 0.1, fill = "steelblue") +
    geom_hline(yintercept = 0, color = "red") +
    labs(
      title = paste("ACF de", param),
      x = "Lag",
      y = "Autocorrelación"
    ) +
    theme_minimal(base_size = 14)
  
  # Guardar gráfico
  nombre_archivo <- paste0("ACF_plots/ACF_", gsub("\\[|\\]", "_05", param), ".pdf")
  ggsave(nombre_archivo, plot = plot_acf, width = 8, height = 6, units = "in", dpi = 300)
  
  cat("Guardado:", nombre_archivo, "\n")
}


### ACF
library(ggplot2)

# --- Paso 1: Lista de parámetros
parametros <- c(
  paste0("alpha[", 1:14, "]"),
  paste0("mu[", 1:4, "]"),
  "mu_global", "sigma", "sigma_cole", "sigma_global", "beta_age", "beta_naturaleza",
  paste0("educ_madre_cen[", 1:10, "]"),
  paste0("educ_padre_cen[", 1:10, "]"),
  paste0("estrato_cen[", 1:6, "]"),
  paste0("horas_cen[", 1:5, "]"),
  paste0("internet_cen[", 1:2, "]")
)

# --- Paso 2: Carpeta donde guardar los plots
dir.create("ACF_plots", showWarnings = FALSE)

# --- Paso 3: Bucle para graficar y guardar ACF de cada parámetro
for (param in parametros) {
  
  # Extraer las muestras de la primera partición (o podrías combinar si quieres)
  muestras <- as.numeric(fits_thinned[[1]][[param]])
  
  # Calcular ACF
  acf_result <- acf(muestras, plot = FALSE)
  
  # Preparar los datos
  acf_data <- data.frame(
    Lag = acf_result$lag,
    ACF = acf_result$acf
  )
  
  # Crear gráfico
  plot_acf <- ggplot(acf_data, aes(x = Lag, y = ACF)) +
    geom_bar(stat = "identity", width = 0.1, fill = "steelblue") +
    geom_hline(yintercept = 0, color = "red") +
    labs(
      title = paste("ACF de", param),
      x = "Lag",
      y = "Autocorrelación"
    ) +
    theme_minimal(base_size = 14)
  
  # Guardar gráfico
  nombre_archivo <- paste0("ACF_plots/ACF_", gsub("\\[|\\]", "_03", param), ".pdf")
  ggsave(nombre_archivo, plot = plot_acf, width = 8, height = 6, units = "in", dpi = 300)
  
  cat("Guardado:", nombre_archivo, "\n")
}

######### TRACEPLOT

library(ggplot2)
library(dplyr)

# --- Paso 1: Lista de parámetros
parametros <- c(
  paste0("alpha[", 1:14, "]"),
  paste0("mu[", 1:4, "]"),
  "mu_global", "sigma", "sigma_cole", "sigma_global", "beta_age", "beta_naturaleza",
  paste0("educ_madre_cen[", 1:10, "]"),
  paste0("educ_padre_cen[", 1:10, "]"),
  paste0("estrato_cen[", 1:6, "]"),
  paste0("horas_cen[", 1:5, "]"),
  paste0("internet_cen[", 1:2, "]")
)

# --- Paso 2: Crear carpeta para guardar los traceplots
dir.create("Traceplots", showWarnings = FALSE)

# --- Paso 3: Loop sobre todos los parámetros
for (param in parametros) {
  
  # Crear data frame para el traceplot
  df_trace <- data.frame(
    Iteracion = fits_thinned[[1]]$.iteration,
    Valor = as.numeric(fits_thinned[[1]][[param]]),
    Cadena = factor(fits_thinned[[1]]$.chain)
  )
  
  # Crear el gráfico
  p_trace <- ggplot(df_trace, aes(x = Iteracion, y = Valor, color = Cadena)) +
    geom_line() +
    labs(
      title = paste("Traceplot de", param),
      x = "Iteración",
      y = paste("Valor de", param)
    ) +
    theme_minimal(base_size = 14)
  
  # Guardarlo
  nombre_archivo <- paste0("Traceplots/Traceplot03_", gsub("\\[|\\]", "_", param), ".pdf")
  ggsave(nombre_archivo, plot = p_trace, width = 8, height = 6, units = "in", dpi = 300)
  
  cat("Guardado:", nombre_archivo, "\n")
}





