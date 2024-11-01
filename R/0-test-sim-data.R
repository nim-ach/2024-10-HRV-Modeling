
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(ggplot2)
library(brms)

## Main equation
RRi_model <- function(t, alpha, beta, c, lambda, phi, tau, delta) {
  alpha +
    (beta / (1 + exp(lambda * (t - tau)))) +
    ((-beta * c) / (1 + exp(phi * (t - tau - delta))))
}

simulate_data <- function(n_id, n_time, noise = 10, seed = 1234) {
  set.seed(seed)
  within(
    data = data.table::data.table(), 
    expr = {
      alpha = rep(runif(n_id, 800, 1000), each = n_time)
      beta = rep(runif(n_id, -500, -300), each = n_time)
      c = rep(runif(n_id, .7, 1.1), each = n_time)
      lambda = rep(runif(n_id, log(1-.9), log(1-.7)), each = n_time)
      phi = rep(runif(n_id, log(1-.7), log(1-.5)), each = n_time)
      tau = rep(runif(n_id, 3, 7), each = n_time)
      delta = rep(runif(n_id, 1, 3), each = n_time)
      time = rep(x = seq(0, 20, length.out = n_time), times = n_id)
      rri_true = RRi_model(time, alpha, beta, c, lambda, phi, tau, delta)
      rri_hat = rri_true + brms::rstudent_t(n_id * n_time, df = 5, sigma = noise)
      id = rep(1:n_id, each = n_time)
    }
  )
}

## Load the data
data("rri_data")

## Standardize as with the simulated dataset
rri_data[, rri_mean := mean(rr_denoised, na.rm = TRUE), by = id]
rri_data[, rri_sd := sd(rr_denoised, na.rm = TRUE), by = id]
rri_data[, rri_std := (rr_denoised - rri_mean) / rri_sd, by = id]

## Check the 2d-density kernel of time-rri_std
ggplot(rri_data, aes(time, rri_std)) +
  stat_density_2d_filled() + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))
## Much more aligned across subjects. This will definitely improve model
## convergence


# Model with fixed effects only -------------------------------------------

m_simple_prior <- c(
  ## Fix effects
  prior(normal(1, 0.5), nlpar = alpha), 
  prior(normal(-2, 0.5), nlpar = beta, ub = 0),
  prior(normal(0.8, 0.2), nlpar = c, lb = 0),
  prior(normal(-2, 0.2), nlpar = lambda, ub = 0),
  prior(normal(-2, 0.2), nlpar = phi, ub = 0),
  prior(normal(5, 0.5), nlpar = tau, lb = 0),
  prior(normal(5, 0.5), nlpar = delta, lb = 0),
  ## Sigma
  prior(student_t(3, 0, 1), class = sigma, lb = 0)
)

## Model without random effects
m_simple_formula <- bf(
  formula = rri_std ~ alpha +
    (beta / (1 + exp(lambda * (time - tau)))) +
    ((-beta * c) / (1 + exp(phi * (time - tau - delta)))),
  alpha + beta + c + lambda + phi + tau + delta ~ 1,
  nl = TRUE
)

m_simple <- brm(
  formula = m_simple_formula,
  data = rri_data[id %in% 1:5 & !is.na(rri_std), .SD[sort(sample.int(.N, 100))], id],
  family = gaussian(),
  prior = m_simple_prior,
  cores = 4, chains = 4,
  seed = 1234,
  file = "R/m_test_simple.RDS"
)

## Elapsed Time (Windows): 701.218 seconds (Total)
## Elapsed Time (Windows): 665.612 seconds (Total) # small improvement with tight priors
## Elapsed Time (Windows): 6.716 seconds (Total) # big improvement with 100 points per id

conditional_effects(m_simple)
bayesplot::mcmc_pairs(m_simple, regex_pars = "^b_", off_diag_fun = "hex")

shinystan::launch_shinystan(m_simple)

# Model with random effects -----------------------------------------------


## Let´s use the same priors as the one used previously.
m_prior <- 
  c(
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

## Model with random effects for each parameter
m_formula <- bf( 
  formula = rri_std ~ alpha +
    (beta / (1 + exp(lambda * (time - tau)))) +
    ((-beta * c) / (1 + exp(phi * (time - tau - delta)))),
  alpha + beta + c + lambda + phi + tau + delta ~ 1 | id,
  nl = TRUE
) 

m_full <- brm(
  formula = m_formula,
  data = rri_data[id %in% 1:5 & !is.na(rri_std), .SD[sort(sample.int(.N, 100))], id],
  family = gaussian(),
  prior = m_prior,
  cores = 4, chains = 4,
  seed = 1234,
  file = "R/m_test_full.RDS"
)

## Elapsed Time (Windows): 243.31 seconds (Total)
## Elapsed Time (Windows): 354.618 seconds (Total) ## Not much improvement
## Elapsed Time (Windows): 364.949 seconds (Total) ## Not much improvement either

conditional_effects(m_full)
bayesplot::mcmc_pairs(m_full, regex_pars = "^b_", off_diag_fun = "hex")

shinystan::launch_shinystan(m_full)
