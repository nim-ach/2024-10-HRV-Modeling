
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(brms)
library(ggplot2)

## Load data
data("rri_data")
m_data <- rri_data[!is.na(rri_std)]

## Load adjusted model parameters
id_data <- fread(file = "models/id_level_posterior.csv")

## Custom functions
fit_model <- function(model, priors) {
  brms::brm(formula = model,
            data = id_data,
            prior = priors,
            family = gaussian(),
            chains = 5, cores = 5, seed = 1234,
            iter = 10000, warmup = 5000,
            control = list(adapt_delta = .99,
                           max_treedepth = 50),
            file = paste0("models/group_level/",
                          deparse(substitute(model)),
                          ".RDS")
  )
}

# Fit model ---------------------------------------------------------------

## Models
m_alpha_model <- bf(alphaMu | se(alphaSE, sigma = TRUE) ~ 1 + (1 | id))
m_beta_model <- bf(betaMu | se(betaSE, sigma = TRUE) ~ 1 + (1 | id))
m_c_model <- bf(cMu | se(cSE, sigma = TRUE) ~ 1 + (1 | id))
m_lambda_model <- bf(lambdaMu | se(lambdaSE, sigma = TRUE) ~ 1 + (1 | id))
m_phi_model <- bf(phiMu | se(phiSE, sigma = TRUE) ~ 1 + (1 | id))
m_tau_model <- bf(tauMu | se(tauSE, sigma = TRUE) ~ 1 + (1 | id))
m_delta_model <- bf(deltaMu | se(deltaSE, sigma = TRUE) ~ 1 + (1 | id))
m_sigma_model <- bf(sigmaMu | se(sigmaSE, sigma = TRUE) ~ 1 + (1 | id))

# Priors
m_alpha_prior <-
  prior(normal(850, 10), class = "Intercept", lb = 0) +
  prior(normal(115, 10), class = "sigma", lb = 0) +
  prior(normal(10, 10), class = "sd", lb = 0)
m_beta_prior <-
  prior(normal(-400, 50), class = "Intercept", ub = 0) +
  prior(normal(128, 10), class = "sigma", lb = 0) +
  prior(normal(50, 10), class = "sd", lb = 0)
m_c_prior <-
  prior(normal(0.8, 0.05), class = "Intercept", lb = 0) +
  prior(normal(0.1, 0.01), class = "sigma", lb = 0) +
  prior(normal(0.1, 0.05), class = "sd", lb = 0)
m_lambda_prior <-
  prior(normal(-3.0, 0.1), class = "Intercept", ub = 0) +
  prior(normal(1, 0.1), class = "sigma", lb = 0) +
  prior(normal(0.5, 0.5), class = "sd", lb = 0)
m_phi_prior <-
  prior(normal(-2.5, 0.1), class = "Intercept", ub = 0) +
  prior(normal(1, 0.1), class = "sigma", lb = 0) +
  prior(normal(0.5, 0.5), class = "sd", lb = 0)
m_tau_prior <-
  prior(normal(6.5, 0.1), class = "Intercept", lb = 0) +
  prior(normal(0.8, 0.1), class = "sigma", lb = 0) +
  prior(normal(0.5, 0.5), class = "sd", lb = 0)
m_delta_prior <-
  prior(normal(3.5, 0.5), class = "Intercept", lb = 0) +
  prior(normal(1.5, 0.1), class = "sigma", lb = 0) +
  prior(normal(0.5, 0.5), class = "sd", lb = 0)
m_sigma_prior <-
  prior(normal(30, 5), class = "Intercept", lb = 0) +
  prior(normal(10, 1), class = "sigma", lb = 0) +
  prior(normal(5, 1), class = "sd", lb = 0)

m_alpha_fit <- fit_model(m_alpha_model, m_alpha_prior)
m_beta_fit <- fit_model(m_beta_model, m_beta_prior)
m_c_fit <- fit_model(m_c_model, m_c_prior)
m_lambda_fit <- fit_model(m_lambda_model, m_lambda_prior)
m_phi_fit <- fit_model(m_phi_model, m_phi_prior)
m_tau_fit <- fit_model(m_tau_model, m_tau_prior)
m_delta_fit <- fit_model(m_delta_model, m_delta_prior)
m_sigma_fit <- fit_model(m_sigma_model, m_sigma_prior)

models <- list(
  alpha = m_alpha_fit, beta = m_beta_fit,
  c = m_c_fit, lambda = m_lambda_fit,
  phi = m_phi_fit, tau = m_tau_fit,
  delta = m_delta_fit, sigma = m_sigma_fit
)

m_fits_df <- lapply(models, as_draws_df) |> 
  rbindlist(idcol = "Parameter")

m_fits_df[, Parameter := factor(
  x = Parameter, 
  levels = c("alpha", "beta", "c", "lambda", "phi", "tau", "delta", "sigma")
)] 

fwrite(m_fits_df, file = "models/group_level_posterior.csv")
