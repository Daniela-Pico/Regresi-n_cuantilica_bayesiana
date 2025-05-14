//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;            // Número de observaciones
  vector[N] x;               // Variable predictora
  vector[N] y;               // Variable respuesta
  real<lower=0, upper=1> tau;// Cuantil deseado
}

parameters {
  real beta0;                // Intercepto
  real beta;                 // Pendiente
  real<lower=0> sigma;       // Desviación estándar de los errores
}

model {
  // Prioris para los parámetros
  beta0 ~ normal(0, 1000);
  beta ~ normal(0, 1000);
  sigma ~ inv_gamma(0.001, 0.001);
 
  // Likelihood utilizando la distribución de Laplace
  for (i in 1:N) {
    y[i] ~ skew_double_exponential(beta0 + beta * x[i], 2*sigma, tau);
  }
}


