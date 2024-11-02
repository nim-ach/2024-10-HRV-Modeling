
# Prepare workspace -------------------------------------------------------

## Load packages
library(data.table)
library(brms)
library(ggplot2)

## Selection of txt files
files <- list.files(path = "data-raw/hrv_data", full.names = TRUE, pattern = "\\.txt$")

## Data import
rri_data <- lapply(files, \(i) {
  rr_interval <- as.double(x = readLines(i))
  not_na <- !is.na(rr_interval)
  rr_interval <- rr_interval[not_na & between(rr_interval, 400, 1200)]
  time <- cumsum(rr_interval)/60000
  data.table(rr_interval = rr_interval[time <= 20],
             time = time[time <= 20],
             file = gsub("data-raw/", "", i, fixed = TRUE))
}) |> 
  rbindlist(idcol = "id")

## Data filtering
id_to_keep <- rri_data[, max(time) > 10, id][V1 == TRUE, id]
id_to_move <- rri_data[, max(time) > 10, id][V1 == FALSE, id]
files_to_move <- rri_data[id %in% id_to_keep, files[!files %in% paste0("data-raw/", file)]]


if (length(id_to_move) > 0) {
  for (i in id_to_move) {
    file <- rri_data[id == i, unique(file)]
    from <- paste0("data-raw/", file)
    to <- paste0("data-raw/hrv_data/unused/", gsub("hrv_data/", "", file))
    file.copy(from = from, to = to, overwrite = TRUE)
    file.remove(from)
  }
}

if (length(files_to_move) > 0) {
  for (i in files_to_move) {
    to <- paste0("data-raw/hrv_data/unused/", gsub("data-raw/hrv_data/", "", i))
    file.copy(from = i, to = to, overwrite = TRUE)
    file.remove(i)
  }
}

rm(files, files_to_move, id_to_move)

rri_data <- rri_data[id %in% id_to_keep]


# Processing signal data --------------------------------------------------

## Denoising the signal data
denoise <- function(x, w = 0.1, n = 3) {
  signal::filtfilt(filt = signal::butter(n, w), x)
}

## Applying Butterworth 10 Hz low-pass filter
rri_data[, j = rr_denoised := denoise(rr_interval), id]

trim <- function(x, abs) {
  x_len <- length(x)
  ind <- c(1:abs, (x_len - abs):x_len)
  x[ind] <- NA
  x
}

## Remove overshoot heartbeats in tails due filtering
rri_data[, rr_denoised := trim(rr_denoised, abs = 5), id]

## Visually explore a manually remove data with noise-onyl measurements
if (FALSE) {
  for (i in id_to_keep) {
    rri_data[id == i, plot(time, rr_denoised, ylim = c(400, 1200), 
                       xlim = c(0,20), type = "l")]
    
    move_file <- askYesNo(msg = "¿Keep data?", default = TRUE)
    if (isFALSE(move_file)) {
      file <- rri_data[id == i, unique(file)]
      from <- paste0("data-raw/", file)
      to <- paste0("data-raw/hrv_data/unused/", gsub("hrv_data/", "", file))
      file.copy(from = from, to = to, overwrite = TRUE)
      file.remove(from)
    }
  }
}

rri_data[, file := NULL][]

## Standardize as with the simulated dataset
rri_data[, rri_mean := mean(rr_denoised, na.rm = TRUE), by = id][]
rri_data[, rri_sd := sd(rr_denoised, na.rm = TRUE), by = id][]
rri_data[, rri_std := (rr_denoised - rri_mean) / rri_sd, by = id][]

save(rri_data, file = "data/rri_data.RData")
fwrite(rri_data, file = "data/rri_data.csv")
