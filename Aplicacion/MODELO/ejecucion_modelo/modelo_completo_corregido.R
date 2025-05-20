library(rstan)


# Leer el archivo CSV
datos_sub <- read.csv("johnatan3/SUB_BASE1.csv", header = TRUE, sep = ",")

datos_sub <- datos_sub[!is.na(datos_sub$FAMI_TIENEINTERNET) & 
                         !is.na(datos_sub$FAMI_ESTRATOVIVIENDA) & 
                         !is.na(datos_sub$ESTU_HORASSEMANATRABAJA) &
                         !is.na(datos_sub$EDAD_2023) &
                         !is.na(datos_sub$FAMI_EDUCACIONMADRE) &
                         !is.na(datos_sub$FAMI_EDUCACIONPADRE), ]

# Eliminar filas donde FAMI_EDUCACIONMADRE o FAMI_EDUCACIONPADRE sean "No Sabe" o "No Aplica"
datos_sub <- datos_sub[!datos_sub$FAMI_EDUCACIONMADRE %in% c("No sabe", "No Aplica") & 
                         !datos_sub$FAMI_EDUCACIONPADRE %in% c("No sabe", "No Aplica"), ]


# Recodificación de variables existentes ----------------------------------
# FAMI_TIENEINTERNET: recodificar "Si" -> 2, "No" -> 1
datos_sub$FAMI_TIENEINTERNET <- ifelse(datos_sub$FAMI_TIENEINTERNET == "Si", 2,
                                       ifelse(datos_sub$FAMI_TIENEINTERNET == "No", 1, NA))

# FAMI_ESTRATOVIVIENDA: recodificar a números del 1 al 7
datos_sub$FAMI_ESTRATOVIVIENDA <- ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 1", 1,
                                         ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 2", 2,
                                                ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 3", 3,
                                                       ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 4", 4,
                                                              ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 5", 5,
                                                                     ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 6", 6,
                                                                            ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Sin Estrato", 7, NA)))))))

# ESTU_HORASSEMANATRABAJA: recodificar de acuerdo a los intervalos definidos (1 a 5)
datos_sub$ESTU_HORASSEMANATRABAJA <- ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "0", 1,
                                            ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "Menos de 10 horas", 2,
                                                   ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "Entre 11 y 20 horas", 3,
                                                          ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "Entre 21 y 30 horas", 4,
                                                                 ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "Más de 30 horas", 5, NA)))))

# Recodificación de las nuevas variables ------------------------------------
# Educación de la madre y del padre
datos_sub$FAMI_EDUCACIONMADRE <- as.integer(as.factor(datos_sub$FAMI_EDUCACIONMADRE))
datos_sub$FAMI_EDUCACIONPADRE <- as.integer(as.factor(datos_sub$FAMI_EDUCACIONPADRE))

colnames(datos_sub)[colnames(datos_sub) == "colegio"] <- "Colegio"

# Naturaleza del colegio: recodificar de "OFICIAL" -> 1 y "NO OFICIAL" -> 0  
datos_sub_distinct <- datos_sub[!duplicated(datos_sub[c("ClusterMuni", "Colegio")]), ]
datos_sub_distinct$COLE_NATURALEZA <- ifelse(datos_sub_distinct$COLE_NATURALEZA == "OFICIAL", 1,
                                             ifelse(datos_sub_distinct$COLE_NATURALEZA == "NO OFICIAL", 0, NA))

# Eliminar filas con NA en las variables clave (ajusta según convenga)
datos_sub <- datos_sub[!is.na(datos_sub$FAMI_TIENEINTERNET) & 
                         !is.na(datos_sub$FAMI_ESTRATOVIVIENDA) & 
                         !is.na(datos_sub$ESTU_HORASSEMANATRABAJA) &
                         !is.na(datos_sub$EDAD_2023) &
                         !is.na(datos_sub$FAMI_EDUCACIONMADRE) &
                         !is.na(datos_sub$FAMI_EDUCACIONPADRE), ]

# Convertir variables a enteros
datos_sub$FAMI_TIENEINTERNET <- as.integer(datos_sub$FAMI_TIENEINTERNET)
datos_sub$FAMI_ESTRATOVIVIENDA <- as.integer(datos_sub$FAMI_ESTRATOVIVIENDA)
datos_sub$ESTU_HORASSEMANATRABAJA <- as.integer(datos_sub$ESTU_HORASSEMANATRABAJA)
datos_sub$FAMI_EDUCACIONMADRE <- as.integer(datos_sub$FAMI_EDUCACIONMADRE)
datos_sub$FAMI_EDUCACIONPADRE <- as.integer(datos_sub$FAMI_EDUCACIONPADRE)

# Asegurarse de que la variable Colegio sea numérica
datos_sub_distinct$Colegio <- as.integer(datos_sub_distinct$Colegio)
datos_sub$Colegio <- as.integer(datos_sub$Colegio)

# Definir el número total de estudiantes, colegios y municipios ----------------
N <- nrow(datos_sub)
K <- length(unique(datos_sub$ClusterMuni))

# Para los colegios se usa el dataframe sin duplicados
datos_unicos <- datos_sub[!duplicated(datos_sub[c("ClusterMuni", "Colegio")]), ]
conteo_colegios_por_cluster <- aggregate(Colegio ~ ClusterMuni, data = datos_unicos, FUN = length)
total_colegios <- sum(conteo_colegios_por_cluster$Colegio)
J <- total_colegios
N2 <- total_colegios

# Definir número de niveles para las variables nuevas
M <- length(unique(datos_sub$FAMI_EDUCACIONMADRE))
P <- length(unique(datos_sub$FAMI_EDUCACIONPADRE))

# Preparar la lista de datos para Stan (en el orden que requiere el modelo)
stan_data <- list(
  N = N,
  J = J,
  K = K,
  H = 5,  # Niveles para ESTU_HORASSEMANATRABAJA (1 a 5)
  N2 = N2,
  E = 6,
  I = 2,  # Niveles para "tiene internet" (1 = No, 2 = Si)
  M = M,  # Niveles para FAMI_EDUCACIONMADRE
  P = P,  # Niveles para FAMI_EDUCACIONPADRE
  cole = datos_sub$Colegio,
  muni = datos_sub$ClusterMuni,  
  horas = datos_sub$ESTU_HORASSEMANATRABAJA,
  estrato = datos_sub$FAMI_ESTRATOVIVIENDA,
  internet = datos_sub$FAMI_TIENEINTERNET,  # Valores en {1,2}
  educ_madre = datos_sub$FAMI_EDUCACIONMADRE,
  educ_padre = datos_sub$FAMI_EDUCACIONPADRE,
  naturaleza = datos_sub_distinct$COLE_NATURALEZA,
  x = datos_sub$EDAD_2023,y = datos_sub$PUNT_GLOBAL,
  tau = 0.8,
  col2 = datos_sub_distinct$Colegio,
  muni2 = datos_sub_distinct$ClusterMuni
)


# Ajuste del modelo en Stan --------------------------------------------------
fit <- stan(data = stan_data, 
            file = "modelo_completo.stan",
            chains = 4,
            iter = 130000,
            cores = 4,
            refresh = 3000)
fit
options(max.print = 1050)
print(fit)
save(fit, file = "particion1_modelo_completo.RData")