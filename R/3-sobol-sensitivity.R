
# Prepare workspace -------------------------------------------------------

## Load libraries ----

library(data.table)
library(ggplot2)
library(ggsci)

## Custom functions ----

## Define the RR interval model as a function of time
rr_interval_model <- function(params, time) {
  alpha <- params[1]
  beta <- params[2]
  c <- params[3]
  lambda <- params[4]
  phi <- params[5]
  tau <- params[6]
  delta <- params[7]
  
  # Apply time-dependent RR interval model with logistic functions
  rr_interval <- alpha + beta / (1 + exp(lambda * (time - tau))) +
    (-beta * c) / (1 + exp(phi * (time - delta)))
  
  return(rr_interval)
}

# Generate random uniform samples for each parameter
sobol_sample <- function(n_samples) {
  out <- data.frame(
    alpha = runif(n_samples, min = 400, max = 1200), 
    beta = runif(n_samples, min = -600, max = -200), 
    c = runif(n_samples, min = 0.5, max = 1.5), 
    lambda = runif(n_samples, min = -3.454, max = -1.151), 
    phi = runif(n_samples, min = -1.806, max = -0.602),
    tau = runif(n_samples, min = 2.5, max = 7.5), 
    delta = runif(n_samples, min = 1, max = 3)
  )
  
  return(out)
}

# Sobol analysis main function
sobol_variance <- function(model_function, samples, time_points) {
  # Generate total output for each sample to get baseline variance
  total_output <- sapply(1:nrow(samples), function(i) {
    model_function(as.numeric(samples[i, ]), time_points)
  })
  
  total_variance <- apply(total_output, 1, var) # Variance across samples for each time point
  
  # Initialize matrix for Sobol indices results
  sobol_indices <- matrix(0, nrow = length(time_points), ncol = ncol(samples),
                          dimnames = list(NULL, names(samples)))
  
  # Loop over each parameter to calculate first-order Sobol indices
  for (param in names(samples)) {
    
    perturbed_output <- sapply(1:nrow(samples), function(i) {
      # Perturb only the chosen parameter, while averaging the others
      modified_params <- as.numeric(samples[i, ])
      modified_params[names(samples) != param] <- apply(samples[names(samples) != param], 2, mean)
      model_function(modified_params, time_points)
    })
    
    # Compute variance due to the specific parameter across all samples and time points
    variance_contrib <- apply(perturbed_output, 1, var)
    sobol_indices[, param] <- variance_contrib / total_variance
  }
  
  # Return Sobol indices as a data frame for analysis
  return(data.frame(time = time_points, sobol_indices))
}


# Perform Bootstrapped Sobol analysis -------------------------------------

n_boot <- 1000                       ## Number of Monte Carlo simulations
n_samples <- 1000                    ## Number of samples per MC sims
time_points <- seq(0, 20, by = 0.1) ## Time points for RR interval calculations

boot <- vector(mode = "list", length = n_boot) ## Pre allocate list for results

if (!file.exists("R/sobol-sensitivity.RDS")) { ## Omit computation if already exist
  
  ## Seed for reproducibility
  set.seed(1234)
  
  ## Monte Carlo simulation
  for (i in seq_len(n_boot)) {
    boot[[i]] <- sobol_variance(model_function = rr_interval_model, 
                                samples = sobol_sample(n_samples), 
                                time_points = time_points)
  }
  
  ## Post processing results
  sobol_results <- data.table::rbindlist(boot, idcol = "boot")
  sobol_results <- data.table::melt.data.table(sobol_results, id.vars = c("boot", "time"))
  sobol_results <- sobol_results[, list(mean = mean(value), se = sd(value)), list(variable, time)]
  
  ## Save results to prevent future computation time
  saveRDS(object = sobol_results, file = "R/sobol-sensitivity.RDS")
} else {
  sobol_results <- readRDS("R/sobol-sensitivity.RDS")
}


# Plot results ------------------------------------------------------------

## Function to plot function
plot_sobol_sens <- function(vars, yexp, ylim, ind_cols) {
  cols <- ggsci::pal_bmj()(7)
  ggplot(sobol_results[time <= 12 & variable %in% vars], aes(time, mean)) +
    facet_grid(cols = vars(variable),
               labeller = label_parsed,
               scales = "free_y") +
    geom_line(aes(col = variable), show.legend = FALSE) +
    geom_ribbon(aes(fill = variable, 
                    ymin = mean - se * qnorm(.975),
                    ymax = mean + se * qnorm(.975)), 
                show.legend = FALSE, alpha = .3) +
    geom_ribbon(aes(fill = variable, 
                    ymin = mean - se * qnorm(.9),
                    ymax = mean + se * qnorm(.9)), 
                show.legend = FALSE, alpha = .3) +
    geom_ribbon(aes(fill = variable, 
                    ymin = mean - se * qnorm(.8),
                    ymax = mean + se * qnorm(.8)), 
                show.legend = FALSE, alpha = .3) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       name = "Sensitivity",
                       expand = yexp,
                       limits = ylim,
                       n.breaks = 5) +
    scale_x_continuous(name = "Time (minutes)",
                       expand = c(0,0),
                       n.breaks = 3) +
    scale_color_manual(values = cols[ind_cols], aesthetics = c("fill", "colour")) +
    theme_classic(base_size = 12) +
    theme(axis.text = element_text(size = rel(.8)),
          strip.background = element_rect(colour = NA, fill = "gray90"))
}

## Generate figures and arrange them in a grid
figure <- ggpubr::ggarrange(
  plot_sobol_sens(vars = c("alpha"), c(0,0,0,0.01), c(0, NA), 1),
  plot_sobol_sens(vars = c("beta", "c"), c(0,0,0,0), c(0, 0.25), 2:3),
  plot_sobol_sens(vars = c("lambda", "phi"), c(0,0,0,0), c(0, 0.035), 4:5),
  plot_sobol_sens(vars = c("tau", "delta"), c(0,0,0,0.0), c(0, 0.35), 6:7),
  ncol = 2, nrow = 2
)

## Save the final figure in pdf and png formats
ggsave("figures/sobol-sensitivity.pdf", figure, "pdf", 
       width = 7, height = 5, units = "in")
ggsave("figures/sobol-sensitivity.png", figure, "png", 
       width = 7, height = 5, units = "in", dpi = 500)
