# Prepare workspace -------------------------------------------------------

## Load libraries ----
library(ggplot2)
library(brms)
library(data.table)

## Load the data
data("rri_data")
m_data <- rri_data[!is.na(rri_std)]

## Add custom function
RRi_sim <- function(t, alpha, beta, c, lambda, phi, tau, delta) {
  alpha +
    (beta / (1 + exp(lambda * (t - tau)))) +
    ((-beta * c) / (1 + exp(phi * (t - tau - delta))))
}

if (!file.exists("models/stage_1_model.RDS")) {

  # Load model --------------------------------------------------------------
  m_model <- readRDS(file = "models/m_prior_only.RDS")
  
  # Identify unique ID to loop for ------------------------------------------
  unique_ids <- m_data$id |> unique()
  list_models <- vector(mode = "list", length = length(unique_ids))
  
  # Loop all ID -------------------------------------------------------------
  for (i in unique_ids) {
    print(paste0("Now on id ", i))
    file <- paste0("models/id_level/m_id_",i,".RDS")
    
    ## If model already exists, load it, then jump to next ID
    if (file.exists(file)) {
      list_models[[i]] <- readRDS(file)
      next
    }
    
    ## If not, sample the posterior for that ID
    list_models[[i]] <- update(
      object = m_model,
      newdata = m_data[id == i],
      seed = 1234,
      iter = 10000, warmup = 5000,
      chains = 5, cores = 5,
      control = list(adapt_delta = 0.99,
                     max_treedepth = 50),
      sample_prior = "no",
      file = file
    )
  }
  
  ## Merge all posterior samples into a single data.frame
  df_models <- lapply(list_models, as_draws_df) |> 
    rbindlist(idcol = "id")
  
  ## Save the resulting id-wise posterior in a file
  saveRDS(df_models, file = "models/stage_1_model.RDS", compress = "xz")
  
} else {
  ## Load the id-wise posterior file
  df_models <- readRDS(file = "models/stage_1_model.RDS")
}

## Transform alpha and beta parameters
df_models <- merge.data.table(
  x = df_models,
  y = unique(rri_data[, c("id", "rri_mean", "rri_sd")]),
  by = "id"
)

df_models[, b_alpha_Intercept := b_alpha_Intercept * rri_sd + rri_mean, id]
df_models[, b_beta_Intercept := b_beta_Intercept * rri_sd, id]
df_models[, sigma := sigma * rri_sd, id]

## Compute mean and SD for each parameter, for each ID
mu_data <- df_models[, lapply(.SD, mean), id, .SDcols = b_alpha_Intercept:sigma]
se_data <- df_models[, lapply(.SD, sd), id, .SDcols = b_alpha_Intercept:sigma]

names(mu_data) <- c("id", "alphaMu", "betaMu", "cMu", 
                    "lambdaMu", "phiMu", "tauMu", "deltaMu", 
                    "sigmaMu")
names(se_data) <- c("id", "alphaSE", "betaSE", "cSE", 
                    "lambdaSE", "phiSE", "tauSE", "deltaSE", 
                    "sigmaSE")

## Merge both summary statistics for each id
id_data <- merge.data.table(mu_data, se_data, by = "id")

## Save resulting data.frame in CSV file
fwrite(id_data, file = "models/id_level_posterior.csv")

## Generate data with estimated parameters and observed data
long_data <- merge.data.table(
  m_data[, .(id, time, rr_denoised)],
  id_data[, .SD, .SDcols = id:sigmaMu],
  by = "id",
)

## Add estimated RRi data for each id
long_data[, pred_rr := RRi_sim(
  t = time, alpha = alphaMu,
  beta = betaMu, c = cMu,
  lambda = lambdaMu, phi = phiMu,
  tau = tauMu, delta = deltaMu
), id]


# Re-fit problematic observations -----------------------------------------

check_id <- logical(length = 272)
for(i in seq_along(unique(error_phase$id))) {
  long_data[id == i, j = {
    plot(time, rr_denoised, type = "l", lwd = 1/2)
    lines(time, pred_rr, type = "l", lwd = 1/2)
    abline(v = c(tauMu, tauMu + deltaMu), lwd = 1/2, lty = 2)
  }]
  cat("Actual: ", i)
  if (askYesNo(msg = "Todo bien?")) {
    next
  } else {
    check_id[i] <- TRUE
  }
}

## Problematic recordings
ids <- c(19L, 21L, 26L, 27L, 34L, 50L, 52L, 86L, 100L, 102L, 105L, 132L, 
         135L, 139L, 150L, 154L, 159L, 233L, 246L, 258L)

models <- vector(mode = "list", length = length(ids))

for (i in ids) {
  print(paste0("Now on id ", i))
  file <- paste0("models/id_level/m_id_",i,".RDS")
  unlink(file)
  
  models[[i]] <- update(
    object = m_model,
    newdata = m_data[id == i],
    seed = 12345,
    iter = 10000, warmup = 5000,
    chains = 5, cores = 5,
    control = list(adapt_delta = 0.99,
                   max_treedepth = 50),
    sample_prior = "no",
    file = file
  )
} 

# Compute error metrics ---------------------------------------------------

## Compute error metrics
long_data[, error := rr_denoised - pred_rr, id]
long_data[, rr_bar := mean(rr_denoised, na.rm = TRUE), id]
long_data[, mape := mean(abs(error/rr_denoised), na.rm = T), id]
long_data[, rmse := sqrt(mean((error^2), na.rm = TRUE)), id]
long_data[, rsquared := 1-sum(error^2)/sum((rr_denoised-rr_bar)^2), id]

## Save the computed metrics
fwrite(long_data, file = "models/error_performance.csv")
