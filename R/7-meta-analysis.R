N <- 272
draws = 1

library(data.table)

posteriors <- data.table(
  id = rep(1:N, each = draws),
  alpha = rnorm(N * draws, 840, 50),
  alphaSigma = rnorm(N * draws, 50, 5) |> abs(),
  beta = rnorm(N * draws, -400, 50),
  betaSigma = rnorm(N * draws, 50, 5) |> abs(),
  c = rnorm(N * draws, 0.8, 0.05),
  cSigma = rnorm(N * draws, 0.05, 0.05) |> abs(),
  lambda = rnorm(N * draws, -2, 0.1),
  lambdaSigma = rnorm(N * draws, 0.1, 0.1) |> abs(),
  phi = rnorm(N * draws, -1.5, 0.1),
  phiSigma = rnorm(N * draws, 0.05, 0.1) |> abs(),
  tau = rnorm(N * draws, 5, 0.1),
  tauSigma = rnorm(N * draws, 0.1, 0.1) |> abs(),
  delta = rnorm(N * draws, 5, 0.1),
  deltaSigma = rnorm(N * draws, 0.1, 0.1) |> abs()
)

library(brms)

m_mod_alpha <- bf(alpha | se(alphaSigma, sigma = TRUE) ~ 1 + (1 | id))
m_mod_beta <- bf(beta | se(betaSigma, sigma = TRUE) ~ 1 + (1 | id))
m_mod_c <- bf(c | se(cSigma, sigma = TRUE) ~ 1 + (1 | id))
m_mod_lambda <- bf(lambda | se(lambdaSigma, sigma = TRUE) ~ 1 + (1 | id))
m_mod_phi <- bf(phi | se(phiSigma, sigma = TRUE) ~ 1 + (1 | id))
m_mod_tau <- bf(tau | se(tauSigma, sigma = TRUE) ~ 1 + (1 | id))
m_mod_delta <- bf(delta | se(deltaSigma, sigma = TRUE) ~ 1 + (1 | id))

priors <- c(
  prior(normal(-400, 100), class = "Intercept", resp = "beta", ub = 0),
  prior(normal(1, 0.5), class = "Intercept", resp = "c", lb = 0),
  prior(normal(-2.3, 0.5), class = "Intercept", resp = "lambda", ub = 0),
  prior(normal(-1.2, 0.5), class = "Intercept", resp = "phi", ub = 0),
  prior(normal(5, 1), class = "Intercept", resp = "tau", lb = 0),
  prior(normal(5, 1), class = "Intercept", resp = "delta", lb = 0)
)

fit_model <- function(formula, data, priors) {
  brm(formula = formula,
      data = data,
      prior = priors,
      family = gaussian(),
      chains = 4, cores = 4,
      control = list(adapt_delta = .99,
                     max_treedepth = 50))
}

m_fit_alpha <- fit_model(
  formula = m_mod_alpha, 
  data = posteriors, 
  priors = c(
    prior(normal(800, 100), class = "Intercept", lb = 0),
    prior(normal(0, 100), class = "sd", lb = 0),
    prior(normal(0, 100), class = "sigma", lb = 0)
  )
)

loo(m_fit_alpha)
performance::rmse(m_fit_alpha)
predict()

library(ggplot2)


as_draws_df(m_fit_alpha) |> 
  # tidybayes::spread_draws(
  #   r_id[subject,], Intercept
  # ) |> 
  # transform(id_intercept = Intercept + r_id) |> 
  ggplot(aes(x = b_Intercept)) +
  ggdist::stat_halfeye(
    fill = "lightblue",
    density = "unbounded", trim = FALSE) +
  scale_y_continuous(expand = c(0.02,0), breaks = NULL, name = NULL) +
  scale_x_continuous(expand = c(0.2,0), n.breaks = 6) +
  theme_classic() +
  labs(x = expression(symbol(E)~"["*alpha*"]"))
