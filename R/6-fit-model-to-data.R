# Prepare workspace -------------------------------------------------------

## Load libraries ----
library(ggplot2)
library(brms)
library(data.table)
library(ggpubr)

sample_data <- function(data, n, id_var, na.rm = FALSE) {
  na_fun <- function(x) return(x)
  if (isTRUE(na.rm)) na_fun <- na.omit
  na_fun(data)[, .SD[sort(sample.int(.N, n))], by = id_var]
}

## Load the data
data("rri_data")

## Standardize as with the simulated dataset
rri_data[, rri_mean := mean(rr_denoised, na.rm = TRUE), by = id]
rri_data[, rri_sd := sd(rr_denoised, na.rm = TRUE), by = id]
rri_data[, rri_std := (rr_denoised - rri_mean) / rri_sd, by = id]

m_data <- sample_data(rri_data, 200, "id", na.rm = TRUE)

# Build models ------------------------------------------------------------

## Model without Random intercepts ----

### Model formula
m1_formula <- bf(
  rri_std ~ alpha + 
    (beta / (1 + exp(lambda * (time - tau)))) + 
    ((-beta * c) / (1 + exp(phi * (time - tau - delta)))), 
  
  alpha + beta + c ~ 1, 
  lambda + phi ~ 1, 
  tau + delta ~ 1, 
  nl = TRUE
)

m1_prior <- c(
  ## Fix effects
  prior(normal(1, 0.5), nlpar = alpha), 
  prior(normal(-2.5, 0.5), nlpar = beta, ub = 0),
  prior(normal(0.8, 0.2), nlpar = c, lb = 0),
  prior(normal(-2, 0.2), nlpar = lambda, ub = 0),
  prior(normal(-2, 0.2), nlpar = phi, ub = 0),
  prior(normal(5, 0.5), nlpar = tau, lb = 0),
  prior(normal(5, 0.5), nlpar = delta, lb = 0),
  ## Sigma
  prior(student_t(3, 0, 1), class = sigma, lb = 0)
)

m1_model <- brm(
  formula = m1_formula,
  data = m_data,
  family = gaussian(),
  prior = m1_prior,
  seed = 1234,
  iter = 15000, warmup = 5000,
  chains = 5, cores = 5,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 50),
  file = "models/m1_model.RDS"
)

## Model with Random intercepts per ID per parameter ----

### Model formula
m2_formula <- bf(
  rri_std ~ alpha + 
    (beta / (1 + exp(lambda * (time - tau)))) + 
    ((-beta * c) / (1 + exp(phi * (time - tau - delta)))), 
  
  alpha + beta + c ~ 1 | id, 
  lambda + phi ~ 1 | id, 
  tau + delta ~ 1 | id, 
  nl = TRUE
)

m2_prior <- c(
  prior(normal(1, 0.5), nlpar = alpha), 
  prior(normal(-2.5, 0.5), nlpar = beta, ub = 0),
  prior(normal(0.8, 0.2), nlpar = c, lb = 0),
  prior(normal(-2, 0.2), nlpar = lambda, ub = 0),
  prior(normal(-2, 0.2), nlpar = phi, ub = 0),
  prior(normal(5, 0.5), nlpar = tau, lb = 0),
  prior(normal(5, 0.5), nlpar = delta, lb = 0),
  ## Random effects
  prior(normal(0, 0.2), nlpar = alpha, class = sd, lb = 0),  
  prior(normal(0, 0.2), nlpar = beta, class = sd, lb = 0),
  prior(normal(0, 0.2), nlpar = c, class = sd, lb = 0, ub = .5),      
  prior(normal(0, 0.2), nlpar = lambda, class = sd, lb = 0), 
  prior(normal(0, 0.2), nlpar = phi, class = sd, lb = 0),    
  prior(normal(0.5, 0.1), nlpar = tau, class = sd, lb = 0), 
  prior(normal(0.5, 0.1), nlpar = delta, class = sd, lb = 0),
  ## Sigma
  prior(student_t(3, 0, 3), class = sigma, lb = 0)
) 

m2_model <- brm(
  formula = m2_formula,
  data = m_data,
  family = gaussian(),
  prior = m2_prior,
  seed = 1234,
  iter = 15000, warmup = 5000,
  chains = 5, cores = 5,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 50),
  file = "models/m2_model.RDS"
)
