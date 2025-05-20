era este
data {
 int<lower=1> N;             // Número total de estudiantes
 int<lower=1> J;             // Número de colegios
 int<lower=1> K;             // Número de municipios
 int<lower=1> H;             // Número de niveles para las horas de trabajo del estudiante
 int<lower=1> E;             // Número de niveles para estrato de vivienda
 int<lower=1> N2;            
 int<lower=1> I;             // Número de niveles para "tiene internet"
 // Nuevas covariables de cada estudiante
 int<lower=1> M;             // Número de niveles para educación de la madre
 int<lower=1> P;             // Número de niveles para educación del padre
 // Variable a nivel de colegio
 int<lower=0, upper=1> naturaleza[J]; // Naturaleza del colegio (0/1)
 // Datos de cada estudiante
 int<lower=1, upper=J> cole[N]; // Índice de colegio para cada estudiante
 int<lower=1, upper=K> muni[N]; // Índice de municipio para cada estudiante
 int<lower=1, upper=H> horas[N]; // Índice de horas de trabajo para cada estudiante
 int<lower=1, upper=E> estrato[N];// Índice de estrato de vivienda
 int<lower=1, upper=I> internet[N];// Índice para "tiene internet"
 // Nuevos índices para cada estudiante
 int<lower=1, upper=M> educ_madre[N];         // Índice de educación de la madre
 int<lower=1, upper=P> educ_padre[N];         // Índice de educación del padre
 vector[N] y;                // Variable respuesta para cada estudiante
 real<lower=0, upper=1> tau;   // Cuantil deseado
 // Datos auxiliares para la estructura jerárquica (colegios y municipios)
 int<lower=1, upper=J> col2[N2]; // Índice auxiliar de colegio
 int<lower=1, upper=K> muni2[N2];// Índice auxiliar de municipio
 vector[N] x;                // Edad
}
parameters {
 vector[J] alpha;            // Efectos aleatorios por colegio
 real<lower=0> sigma_cole;     // Desviación estándar para cada colegio
 vector[K] mu;               // Media por municipio
 real mu_global;             // Media global
 real<lower=0> sigma;        // Escala para la distribución de error
 real<lower=0> sigma_global; // Desviación estándar global para las medias de los municipios
 real beta_age;              // Coeficiente para la covariable de edad
 vector[H] horas_effect;     // Efectos para las horas de trabajo del estudiante
 vector[E] estrato_effect;   // Efectos para estrato de vivienda
 vector[I] internet_effect;  // Efectos para "tiene internet"
 // Nuevos parámetros para las covariables adicionales
 vector[M] educ_madre_effect;        // Efectos para educación de la madre
 vector[P] educ_padre_effect;        // Efectos para educación del padre
 real beta_naturaleza;               // Coeficiente para la naturaleza del colegio
}
transformed parameters {
 vector[H] horas_cen = horas_effect - mean(horas_effect);
 vector[E] estrato_cen = estrato_effect - mean(estrato_effect);
 vector[I] internet_cen = internet_effect - mean(internet_effect);
 // Centramos los efectos de las nuevas covariables
 vector[M] educ_madre_cen = educ_madre_effect - mean(educ_madre_effect);
 vector[P] educ_padre_cen = educ_padre_effect - mean(educ_padre_effect);
}
model {
 // Priors para parámetros generales
 mu_global ~ normal(0, 1000);
 sigma_global ~ inv_gamma(0.001, 0.001);
 mu ~ normal(mu_global, sigma_global);
 sigma ~ inv_gamma(0.001, 0.001);
 sigma_cole ~ cauchy(0, 10);
 // Priors para las covariables originales
 beta_age ~ normal(0, 1000);
 horas_effect ~ normal(0, 1000);
 estrato_effect ~ normal(0, 1000);
 internet_effect ~ normal(0, 1000);
 // Priors para las nuevas covariables
 educ_madre_effect ~ normal(0, 1000);
 educ_padre_effect ~ normal(0, 1000);
 beta_naturaleza ~ normal(0, 1000);
 // Estructura jerárquica: efectos de colegios
 for (j in 1:N2) {
   alpha[col2[j]] ~ normal(mu[muni2[j]], sigma_cole);
 }
 // Función de verosimilitud
 for (i in 1:N) {
   y[i] ~ skew_double_exponential(
     alpha[cole[i]] +
     beta_age * x[i] +
     estrato_cen[estrato[i]] +
     internet_cen[internet[i]] +
     horas_cen[horas[i]] +
     educ_madre_cen[educ_madre[i]] +
     educ_padre_cen[educ_padre[i]] +
     beta_naturaleza * naturaleza[cole[i]],
     2 * sigma, tau);
 }
}
