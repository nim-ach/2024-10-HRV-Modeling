
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


# Fit model ---------------------------------------------------------------


m_model <- bf(alphaMu | se(alphaSE, sigma = TRUE) ~ 1 + (1 | id)) + 
  bf(betaMu | se(betaSE, sigma = TRUE) ~ 1 + (1 | id)) + 
  bf(cMu | se(cSE, sigma = TRUE) ~ 1 + (1 | id)) + 
  bf(lambdaMu | se(lambdaSE, sigma = TRUE) ~ 1 + (1 | id)) + 
  bf(phiMu | se(phiSE, sigma = TRUE) ~ 1 + (1 | id)) + 
  bf(tauMu | se(tauSE, sigma = TRUE) ~ 1 + (1 | id)) + 
  bf(deltaMu | se(deltaSE, sigma = TRUE) ~ 1 + (1 | id)) +
  bf(sigmaMu | se(sigmaSE, sigma = TRUE) ~ 1 + (1 | id)) +
  set_rescor(TRUE)

get_prior(m_model, id_data)

priors <- c(
  ## Main effects
  prior(normal(850, 10), class = "Intercept", resp = "alphaMu", lb = 0),
  prior(normal(-400, 50), class = "Intercept", resp = "betaMu", ub = 0),
  prior(normal(0.8, 0.1), class = "Intercept", resp = "cMu", lb = 0),
  prior(normal(-3.0, 0.5), class = "Intercept", resp = "lambdaMu", ub = 0),
  prior(normal(-2.5, 0.5), class = "Intercept", resp = "phiMu", ub = 0),
  prior(normal(6.5, 0.5), class = "Intercept", resp = "tauMu", lb = 0),
  prior(normal(3.5, 0.5), class = "Intercept", resp = "deltaMu", lb = 0),
  prior(normal(0.3, 0.5), class = "Intercept", resp = "sigmaMu", lb = 0),
  ## Random effects
  prior(normal(10, 10), class = "sd", resp = "alphaMu", lb = 0),
  prior(normal(50, 50), class = "sd", resp = "betaMu", lb = 0),
  prior(normal(0.1, 0.1), class = "sd", resp = "cMu", lb = 0),
  prior(normal(0.5, 0.5), class = "sd", resp = "lambdaMu", lb = 0),
  prior(normal(0.5, 0.5), class = "sd", resp = "phiMu", lb = 0),
  prior(normal(0.5, 0.5), class = "sd", resp = "tauMu", lb = 0),
  prior(normal(0.5, 0.5), class = "sd", resp = "deltaMu", lb = 0),
  prior(normal(0.5, 0.5), class = "sd", resp = "sigmaMu", lb = 0)
)

m_fit <- brm(formula = m_model,
    data = id_data,
    prior = priors,
    family = gaussian(),
    chains = 5, cores = 5,
    iter = 10000, warmup = 5000,
    control = list(adapt_delta = .99,
                   max_treedepth = 50)
    )

saveRDS(m_fit, file = "stage_2_model.RDS")

# 
# as_draws_df(m_fit_alpha) |> 
#   # tidybayes::spread_draws(
#   #   r_id[subject,], Intercept
#   # ) |> 
#   # transform(id_intercept = Intercept + r_id) |> 
#   ggplot(aes(x = b_Intercept)) +
#   ggdist::stat_halfeye(
#     fill = "lightblue",
#     density = "unbounded", trim = FALSE) +
#   scale_y_continuous(expand = c(0.02,0), breaks = NULL, name = NULL) +
#   scale_x_continuous(expand = c(0.2,0), n.breaks = 6) +
#   theme_classic() +
#   labs(x = expression(symbol(E)~"["*alpha*"]"))
