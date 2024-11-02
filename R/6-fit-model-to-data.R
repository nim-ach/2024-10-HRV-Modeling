# Prepare workspace -------------------------------------------------------

## Load libraries ----
library(ggplot2)
library(brms)
library(data.table)

## Load the data
data("rri_data")

m_data <- rri_data[!is.na(rri_std)]

# Load model --------------------------------------------------------------

m_model <- readRDS(file = "models/m_prior_only.RDS")

# Loop all ID -------------------------------------------------------------

unique_ids <- m_data$id |> unique()
list_models <- vector(mode = "list", length = length(unique_ids))

for (i in unique_ids) {
  print(paste0("Now on id ", i))
  list_models[[i]] <- update(
    object = m_model,
    newdata = m_data[id == i],
    seed = 1234,
    iter = 10000, warmup = 5000,
    chains = 5, cores = 5,
    sample_prior = "no",
    file = paste0("models/id_level/m_id_",i,".RDS")
  )
}

df_models <- lapply(list_models, as_draws_df) |> 
  rbindlist(idcol = "id")

fwrite(df_models, file = "models/id_level_posterior.csv")
