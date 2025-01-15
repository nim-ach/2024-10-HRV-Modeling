
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
    ggdist::stat_slabinterval(
      fill = "gray",
      trim = FALSE, density = "unbounded") +
    scale_y_continuous(expand = c(0.1,0,0.05,0), breaks = NULL, name = NULL) +
    scale_x_continuous(expand = c(0,0)) +
    labs(x = xlab) +
    theme_classic(base_line_size = 1/4, base_size = 12) +
    theme(axis.text = element_text(size = rel(.8)))
}

## Uniform sampling of a range of parameters for n subjects
sample_params <- function(n_id, n_rep, min, max) {
  rep(x = runif(n_id, min, max), each = n_rep)
}


# Define main model -------------------------------------------------------

## Main model
main_model <- bf(
  rri_std ~ alpha + 
    (beta / (1 + exp(lambda * (time - tau)))) + 
    ((-beta * c) / (1 + exp(phi * (time - tau - delta)))), 
  
  alpha + beta + c ~ 1, 
  lambda + phi ~ 1, 
  tau + delta ~ 1, 
  nl = TRUE
)

# Define priors for the parameters
priors <- c(
  ## Fix effects
  prior(normal(1, 0.5), nlpar = alpha), 
  prior(normal(-2.5, 0.5), nlpar = beta, ub = 0),
  prior(normal(0.8, 0.2), nlpar = c, lb = 0),
  prior(normal(-2, 0.5), nlpar = lambda, ub = 0),
  prior(normal(-2, 0.5), nlpar = phi, ub = 0),
  prior(normal(5, 0.5), nlpar = tau, lb = 0),
  prior(normal(5, 0.5), nlpar = delta, lb = 0),
  ## Sigma
  prior(student_t(3, 0, 1), class = sigma, lb = 0)
)

len_t <- 100
len_n <- 5

new_data <- data.table(
  time = seq(0, 20, length.out = len_t),
  id = rep(1:len_n, each = len_t),
  rri_std = rnorm(len_t)
)

m_prior_only <- brm(
  formula = main_model,
  data = new_data,
  family = gaussian(),
  prior = priors,
  seed = 1234,
  iter = 10000, warmup = 5000,
  chains = 5, cores = 5,
  sample_prior = "only",
  file = "models/m_prior_only.RDS"
)

time_points <- seq(0, 20, length.out = len_t)

pred <- predict(
  object = m_prior_only, 
  newdata = data.frame(time = time_points), 
  probs = c(.025, .1, .2, .8, .9, .975)
)

fig_a <- ggplot(pred, aes(time_points, Estimate)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), fill = "gray90") +
  geom_ribbon(aes(ymin = Q10, ymax = Q90), fill = "gray75") +
  geom_ribbon(aes(ymin = Q20, ymax = Q80), fill = "gray60") +
  geom_line(show.legend = FALSE, linewidth = 1, col = "gray10") +
  labs(x = "Time (minutes)", y = "Predicted RRi") +
  theme_classic(base_line_size = 1/4, base_size = 12) +
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
  fig_a,
  ggpubr::ggarrange(fig_b1, fig_b2, fig_b3, fig_b4, nrow = 4,
                    fig_b5, fig_b6, fig_b7, ncol = 2, align = "hv"),
  nrow = 1, ncol = 2, widths = c(2,1),
  labels = c("A.", "B."), font.label = list(size = 12)
)

ggsave("figures/figure-4.pdf", figure, "pdf", 
       width = 7, height = 4, units = "in")
ggsave("figures/figure-4.eps", figure, "eps", 
       width = 7, height = 4, units = "in")
ggsave("figures/figure-4.png", figure, "png", 
       width = 7, height = 4, units = "in", dpi = 500)
