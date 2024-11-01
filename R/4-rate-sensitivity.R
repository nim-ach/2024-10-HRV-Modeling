# Prepare workspace -------------------------------------------------------

## Load libraries ----
library(ggplot2)
library(brms)
library(data.table)
library(ggsci)

## Custom functions ----

## Main function to simulate data
RRi_sim <- function(t, alpha, beta, c, lambda, phi, tau, delta) {
  hrv <-
    alpha +
    (beta / (1 + exp(lambda * (t - tau)))) +
    ((-beta * c) / (1 + exp(phi * (t - tau - delta))))
  
  out <- data.table::data.table(time = t, hrv)
  return(out)
}

## Function to simulate combinations of different parameters
sim_parameters <- function(t,
                           alpha, 
                           beta,
                           c,
                           lambda,
                           phi,
                           tau,
                           delta) {
  
  params <- expand.grid(
    alpha = alpha, beta = beta, c = c, 
    lambda = lambda, phi = phi, 
    tau = tau, delta = delta)
  len_params <- nrow(params)
  
  sim_data <- vector("list", length = len_params)
  
  for (i in seq_len(len_params)) {
    sim_data[[i]] <- RRi_sim(
      t = t,
      alpha = params$alpha[i],
      beta = params$beta[i],
      c = params$c[i],
      lambda = params$lambda[i],
      phi = params$phi[i],
      tau = params$tau[i],
      delta = params$delta[i]
    )
    
    sim_data[[i]]$alpha <- params$alpha[i]
    sim_data[[i]]$beta <- params$beta[i]
    sim_data[[i]]$c <- params$c[i]
    sim_data[[i]]$lambda <- params$lambda[i]
    sim_data[[i]]$phi <- params$phi[i]
    sim_data[[i]]$tau <- params$tau[i]
    sim_data[[i]]$delta <- params$delta[i]
    # sim_data[[i]]$hrv <- sim_data[[i]]$hrv + rstudent_t(n_time, 5, 0, 50)
  }
  
  sim_data <- data.table::rbindlist(sim_data, idcol = "id")
  sim_data[, id := factor(id)][]
  return(sim_data)
}


# Simulate data -----------------------------------------------------------

sim_data <- sim_parameters(
  t = seq(0, 20, length.out = 120),
  alpha = 800, beta = -400, c = 0.9,
  lambda = log(1 - seq(0.3, 0.9, by = 0.2)),
  phi = log(1 - seq(0.3, 0.9, by = 0.2)),
  tau = 5, delta = 2
)

sim_data[, `:=`(
  phi = 1 - exp(phi),
  lambda = 1 - exp(lambda)
)]

sim_data[, phi := factor(phi, 
                         levels = c(0.3, 0.5, 0.7, 0.9),
                         labels = c("phi==0.3",
                                    "phi==0.5",
                                    "phi==0.7",
                                    "phi==0.9"))]

## Plot data 

figure <- ggplot(sim_data, aes(time, hrv)) +
  facet_wrap(~ phi, labeller = labeller(phi = label_parsed)) +
  geom_line(aes(col = as.factor(x = lambda), group = id), linewidth = 1) +
  geom_vline(xintercept = c(5,7), color = "gray50", linewidth = .25) +
  scale_y_continuous(breaks = c(500, 600, 700, 800)) +
  scale_alpha_manual(values = c(1, .85, .7, .55)) +
  ggsci::scale_color_observable() +
  theme_classic(base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        legend.position = "right",
        axis.text = element_text(size = rel(.8))) +
  labs(x = "Time (minutes)", y = "R-R intervals (ms)", 
       col = expression(lambda))

ggsave("figures/rate-sensitivity.pdf", figure, "pdf", 
       width = 7, height = 5, units = "in")
ggsave("figures/rate-sensitivity.png", figure, "png", 
       width = 7, height = 5, units = "in", dpi = 500)
