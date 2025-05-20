data {
  int<lower=1> N;             // Número total de estudiantes
  int<lower=1> J;             // Número de colegios
  int<lower=1> K;             // Número de municipios
  int<lower=1> N2;            // Número de niveles para estrato
  int<lower=1, upper=J> cole[N]; // Índice de colegio para cada estudiante
  int<lower=1, upper=K> muni[N]; // Índice de municipio para cada estudiante
  vector[N] y;                // Variable respuesta para cada estudiante
  real<lower=0, upper=1> tau; // Cuantil deseado
  int<lower=1, upper=J> col2[N2]; // Índice auxiliar de colegio
  int<lower=1, upper=K> muni2[N2];// Índice auxiliar de municipio
  vector[N] x;              // covariable
}

parameters {
  vector[J] alpha;            // Efectos aleatorios por colegio
  real<lower=0> sigma_cole;  // Desviación estándar para cada colegio
  vector[K] mu;               // Media por municipio
  real mu_global;             // Media global
  real<lower=0> sigma_global; // Desviación estándar global para las medias de los municipios
  real<lower=0> sigma;
  real beta;              // Coeficiente para la covariable de edad
}



model {
  // Priors
  mu_global ~ normal(0, 1000);
  sigma_global ~ inv_gamma(0.001,0.001);
  mu ~ normal(mu_global, sigma_global);
  sigma ~ inv_gamma(0.001,0.001);
  sigma_cole ~ inv_gamma(0.001,0.001);
  beta ~ normal(0, 1000); 

  for (j in 1:N2) {
    alpha[col2[j]] ~ normal(mu[muni2[j]], sigma_cole);
  }

  for (i in 1:N) {
    y[i] ~ skew_double_exponential(alpha[cole[i]] + beta * x[i], 2*sigma, tau);
  }
}

