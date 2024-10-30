
# Prepare workspace -------------------------------------------------------

## Load libraries ----
library(ggplot2)
library(brms)
library(data.table)
library(ggpubr)

## Custom functions ----

## Main equation
RRi_model <- function(t, alpha, beta, c, lambda, phi, tau, delta) {
  alpha +
    (beta / (1 + exp(lambda * (t - tau)))) +
    ((-beta * c) / (1 + exp(phi * (t - tau - delta))))
}

## Plot posterior density of a chosen parameter
plot_post_dens <- function(posterior, x, xlab) {
  ggplot(posterior, aes(x = {{x}})) +
    geom_histogram(aes(y = after_stat(density)), 
                   col = "darkblue", fill = "lightblue", bins = 20) +
    stat_density(outline.type = "both", fill = NA, 
                 col = "gray20", adjust = 1.5,) +
    scale_y_continuous(expand = c(0,0,0.05,0), breaks = NULL, name = NULL) +
    scale_x_continuous(expand = c(0,0)) +
    labs(x = xlab) +
    theme_classic(base_size = 12) +
    theme(axis.text = element_text(size = rel(.8)))
}

## Uniform sampling of a range of parameters for n subjects
sample_params <- function(n_id, n_rep, min, max) {
  rep(x = runif(n_id, min, max), each = n_rep)
}


# Define main model -------------------------------------------------------

## Main model
main_model <- bf(
  rri ~ alpha + 
    (beta / (1 + exp(lambda * (time - tau)))) + 
    ((-beta * c) / (1 + exp(phi * (time - tau - delta)))), 
  
  alpha + beta + c ~ 1 | id, 
  lambda + phi ~ 1 | id, 
  tau + delta ~ 1 | id, 
  nl = TRUE
)

# Define priors for the parameters
priors <- c(
  prior(normal(850, 50), nlpar = alpha, lb = 0),
  prior(normal(-400, 50), nlpar = beta, ub = 0),
  prior(normal(1, 0.2), nlpar = c, lb = 0),
  prior(normal(-3, 3), nlpar = lambda, ub = 0),
  prior(normal(-3, 3), nlpar = phi, ub = 0),
  prior(normal(5, 0.5), nlpar = tau, lb = 0),
  prior(normal(5, 0.5), nlpar = delta, lb = 0),
  prior(student_t(3, 0, 50), class = sigma, lb = 0)
)

len_t <- 100
len_n <- 5

new_data <- data.table(
  time = seq(0, 20, length.out = len_t),
  alpha = sample_params(len_n, len_t, 500, 1500),
  beta = sample_params(len_n, len_t, -800, -200),
  c = sample_params(len_n, len_t, .5, 1.5),
  lambda = sample_params(len_n, len_t, log(1-.9), log(1-.5)),
  phi = sample_params(len_n, len_t, log(1-.8), log(1-.4)),
  tau = sample_params(len_n, len_t, 4, 6),
  delta = sample_params(len_n, len_t, 4, 6),
  id = rep(1:len_n, each = len_t)
)

new_data$rri <- with(
  data = new_data, 
  RRi_model(time, alpha, beta, c, lambda, phi, tau, delta))

m_prior_only <- brm(
  formula = main_model,
  data = new_data,
  family = gaussian(),
  prior = priors,
  seed = 1234,
  iter = 15000, warmup = 5000,
  chains = 5, cores = 5,
  sample_prior = "only",
  file = "models/m_prior_only.RDS"
)

time_points <- seq(0, 20, length.out = len_t)

pred <- predict(
  object = m_prior_only, 
  newdata = data.frame(time = time_points), 
  probs = c(.025, .1, .2, .8, .9, .975),
  re_formula = NA
)

fig_a <- ggplot(pred, aes(time_points, Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "#E3F2FDFF") +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), fill = "#BADEFAFF") +
  geom_ribbon(aes(ymin = Q20, ymax = Q80), fill = "#90CAF8FF") +
  geom_line(show.legend = FALSE, linewidth = 1, col = "blue4") +
  labs(x = "Time (minutes)", y = "Predicted RRi") +
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(size = rel(.8)))

posterior <- as_draws_df(m_prior_only) |> as.data.table()

fig_b1 <- plot_post_dens(posterior, b_alpha_Intercept, expression(alpha))
fig_b2 <- plot_post_dens(posterior, b_beta_Intercept, expression(beta))
fig_b3 <- plot_post_dens(posterior, b_c_Intercept, expression(italic(c)))
fig_b4 <- plot_post_dens(posterior, b_lambda_Intercept, expression(lambda))
fig_b5 <- plot_post_dens(posterior, b_phi_Intercept, expression(phi))
fig_b6 <- plot_post_dens(posterior, b_tau_Intercept, expression(tau))
fig_b7 <- plot_post_dens(posterior, b_delta_Intercept, expression(delta))

figure <- ggpubr::ggarrange(
  ggpubr::ggarrange(fig_b1, fig_b2, fig_b3, nrow = 1, align = "hv"),
  fig_a,
  ggpubr::ggarrange(fig_b4, fig_b5, fig_b6, fig_b7, nrow = 1, align = "hv"),
  nrow = 3, heights = c(1,2,1)
)

ggsave("figures/prior-model.pdf", figure, "pdf", 
       width = 7, height = 4, units = "in")
ggsave("figures/prior-model.png", figure, "png", 
       width = 7, height = 4, units = "in", dpi = 500)
