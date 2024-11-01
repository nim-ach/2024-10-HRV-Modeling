
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

## Simulating some data
n_id <- 10
n_time <- 100

set.seed(1234)
sim_data <- within(
  data = data.table(), 
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
    rri_hat = rri_true + brms::rstudent_t(n_id * n_time, df = 5, sigma = 20)
    id = rep(1:n_id, each = n_time)
  }
)

## Plot the simulated data
ggplot(sim_data, aes(time, rri_true, group = as.factor(id))) +
  facet_grid(~ id) +
  geom_line() +
  geom_line(aes(y = rri_hat)) +
  theme_linedraw()


# Simple model, no priors -------------------------------------------------

m_prior <- 
  c(
    ## Fix effects
    prior(normal(0, 1), nlpar = alpha), 
    prior(normal(-1, 1), nlpar = beta, ub = 0),
    prior(normal(1, 0.2), nlpar = c, lb = 0),
    prior(normal(-2, 0.5), nlpar = lambda, ub = 0),
    prior(normal(-2, 0.5), nlpar = phi, ub = 0),
    prior(normal(5, 0.5), nlpar = tau, lb = 0),
    prior(normal(5, 0.5), nlpar = delta, lb = 0),
    ## Random effects
    prior(normal(0, 0.5), nlpar = alpha, class = sd, lb = 0),  
    prior(normal(0, 0.5), nlpar = beta, class = sd, lb = 0),
    prior(normal(0, 0.2), nlpar = c, class = sd, lb = 0, ub = .5),      
    prior(normal(0, 0.2), nlpar = lambda, class = sd, lb = 0), 
    prior(normal(0, 0.2), nlpar = phi, class = sd, lb = 0),    
    prior(normal(0, 0.2), nlpar = tau, class = sd, lb = 0), 
    prior(normal(0, 0.2), nlpar = delta, class = sd, lb = 0),
    ## Sigma
    prior(student_t(3, 0, 3), class = sigma, lb = 0)
  )

m_formula <- bf(
  formula = rri_std ~ alpha +
    (beta / (1 + exp(lambda * (time - tau)))) +
    ((-beta * c) / (1 + exp(phi * (time - tau - delta)))),
  alpha + beta + c + lambda + phi + tau + delta ~ 1 | id,
  nl = TRUE
)

sim_data[, mean_rri := mean(rri_hat), id]
sim_data[, sd_rri := sd(rri_hat), id]
sim_data[, rri_std := (rri_hat - mean_rri)/sd_rri, id]

m_1 <- brm(
  formula = m_formula,
  data = sim_data,
  prior = m_prior,
  family = gaussian(),
  cores = 4, chains = 4,
  seed = 1234
)

## Time running: 129.634 seconds (Total)

