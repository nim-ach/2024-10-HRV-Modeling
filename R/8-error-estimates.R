
# Prepare workspace -------------------------------------------------------

## Load libraries ----
library(data.table)
library(ggplot2)
library(ggdist)

## Load data ----
error_data_long <- fread(file = "models/error_performance.csv")

## Prepare data ----

### Unique MAPE and RMSE for each id
error_data <- error_data_long[, lapply(.SD, unique), id, .SDcols = c(15:17)]


### PACF for each id
acf_data <- error_data_long[
  j = list(acf = as.numeric(pacf(error, lag.max = 30, plot = F)$acf),
           lag = 1:30), 
  by = id]

### Bootstrap MAPE and RMSE for better estimates
set.seed(1234)
boot_error <- error_data[, lapply(.SD, function(x) {
  boot = 50000
  x_len = length(x)
  vapply(seq_len(boot), function(i) {
    mean(x[sample.int(x_len, x_len, replace = TRUE)])
  }, FUN.VALUE = 1)
}), .SDcols = mape:rsquared]

boot_error[, mc_sample := seq_len(.N)]

# -------------------------------------------------------------------------

f_1 <- ggplot(error_data, aes(mape, rmse)) +
  geom_point(col = "gray20", cex = 3/4) +
  scale_y_continuous(name = "RMSE (log-scale)", expand = c(0,0.05),
                     breaks = c(1,2,4,9,20)*10,
                     transform = "log10") +
  scale_x_continuous(labels = scales::label_percent(),
                     breaks = c(1,2,4,9,20)/100,
                     name = "MAPE (log-scale)", expand = c(0,0.05),
                     transform = "log10") +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(axis.text = element_text(size = rel(.8)))

error_long <- melt.data.table(boot_error, id.vars = "mc_sample")
error_long[, variable := `levels<-`(variable, c("MAPE", "RMSE", "R^2"))]

f_2a <- ggplot(error_long[variable == "MAPE"], aes(variable, value)) +
  facet_wrap(~ variable, scales = "free") +
  ggdist::stat_halfeye(fill = "gray") +
  scale_y_continuous(name = NULL, expand = c(0.1,0),
                     labels = scales::label_percent(),
                     n.breaks = 7) +
  scale_x_discrete(name = NULL, breaks = NULL, 
                   expand = c(0.1,0,0,0)) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        axis.text = element_text(size = rel(.8)),
        plot.margin = margin(0.1,0.1,0.1,0.1,"in"))

f_2b <- ggplot(error_long[variable == "RMSE"], aes(variable, value)) +
  facet_wrap(~ variable, scales = "free") +
  ggdist::stat_halfeye(fill = "gray") +
  scale_y_continuous(name = NULL, expand = c(0.1,0),
                     n.breaks = 6) +
  scale_x_discrete(name = NULL, breaks = NULL, 
                   expand = c(0.1,0,0,0)) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        axis.text = element_text(size = rel(.8)),
        plot.margin = margin(0.1,0.1,0.1,0.1,"in"))

f_2c <- ggplot(error_long[variable == "R^2"], aes(variable, value)) +
  facet_wrap(~ variable, scales = "free", labeller = label_parsed) +
  ggdist::stat_halfeye(fill = "gray") +
  scale_y_continuous(name = NULL, expand = c(0.1,0),
                     n.breaks = 6) +
  scale_x_discrete(name = NULL, breaks = NULL, 
                   expand = c(0.1,0,0,0)) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        axis.text = element_text(size = rel(.8)),
        plot.margin = margin(0.1,0.1,0.1,0.1,"in"))

f_3 <- ggplot(acf_data, aes(lag, acf)) +
  ggdist::stat_interval(.width = c(.5, .8, .95)) +
  scale_y_continuous(expand = c(0,0.05), limits = c(-1,1),
                     name = "Partial ACF") +
  scale_x_continuous(name = "Lag", breaks = c(1:4, c(1:6)*5), limits = c(1,30)) +
  scale_color_grey(labels = c("95%", "80%", "50%"),
                     name = "CI", start = .8, end = .2) +
  geom_hline(yintercept = c(-.1, .1), linetype = 2, col = "gray20", linewidth = .3) +
  geom_hline(yintercept = 0, linetype = 1, col = "gray0", linewidth = .3) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(axis.text = element_text(size = rel(.8)))

fig_errors <- 
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      ggpubr::ggarrange(f_2a, f_2b, f_2c, nrow = 1, ncol = 3),
      f_1, ncol = 2, nrow = 1, labels = c("A.","B."), font.label = list(size = 12)
    ), f_3, ncol = 1, nrow = 2, heights = c(3,2), labels = c("","C."), font.label = list(size = 12)
  )


# Add predicted RRi -------------------------------------------------------

# ID: id %in% c(1:3,7:9) 1,2,3,7,8,9

f_pred_rr <- ggplot(error_data_long[id %in% c(1:3,7:9)], aes(time, rr_denoised, group = id)) +
  facet_wrap(~ id, nrow = 2, ncol = 3) +
  geom_ribbon(aes(ymin = pred_rr - sigmaMu * qnorm(.975), ymax = pred_rr + sigmaMu * qnorm(.975)), 
              #alpha = .15, 
              fill = "gray90"
              ) +
  geom_ribbon(aes(ymin = pred_rr - sigmaMu * qnorm(.9), ymax = pred_rr + sigmaMu * qnorm(.9)), 
              #alpha = .15, 
              fill = "gray80"
              ) +
  geom_ribbon(aes(ymin = pred_rr - sigmaMu * qnorm(.75), ymax = pred_rr + sigmaMu * qnorm(.75)), 
              #alpha = .15, 
              fill = "gray70"
              ) +
  geom_line(lwd = 1/4, na.rm = TRUE, col = "gray40") +
  geom_line(aes(y = pred_rr), linewidth = 1/2) +
  labs(x = "Time (min)", y = "RRi (ms)") +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(strip.background = element_blank(), 
        strip.text = element_blank())

figure <- 
  ggpubr::ggarrange(
    fig_errors, f_pred_rr, 
    ncol = 1, nrow = 2, heights = c(4,3), labels = c("","D."), font.label = list(size = 12)
  )

# -------------------------------------------------------------------------


ggsave("figures/figure-6.pdf", figure, "pdf", 
       width = 7, height = 9, units = "in")
ggsave("figures/figure-6.eps", figure, "eps", 
       width = 7, height = 9, units = "in")
ggsave("figures/figure-6.png", figure, "png", 
       width = 7, height = 9, units = "in", dpi = 1000)


# Additional statistics asked by reviewers --------------------------------

## New dataset for this query
error_phase <- copy(error_data_long[, .SD, .SDcols = id:pred_rr])

# Define phases
error_phase[, phase := "Exercise"]
error_phase[time < 5, phase := "Rest"]
error_phase[time >= (5 + 2 + 0.1), phase := "Recovery"]

# Re-Compute error metrics
error_phase[, error := rr_denoised - pred_rr, by = id]
error_phase[, rr_bar := mean(rr_denoised, na.rm = TRUE), by = id]

# Check for NaN or infinite values
error_phase <- error_phase[!is.nan(error) & !is.infinite(error)]
error_phase <- error_phase[!is.nan(rr_denoised) & !is.infinite(rr_denoised)]

# Re-Compute performance metrics
error_phase[, mape := mean(abs(error / rr_denoised), na.rm = TRUE), by = .(id, phase)]
error_phase[, rmse := sqrt(mean((error^2), na.rm = TRUE)), by = .(id, phase)]

# Compute R-squared
error_phase[, rsquared := 1 - sum(error^2) / sum((rr_denoised - rr_bar)^2), by = .(id, phase)]
error_phase[, rsquared := pmin(1, pmax(-1, rsquared))]

plot.new(); plot.window(xlim = c(0, 20), ylim = c(400, 1000)); axis(1); axis(2)
for (i in error_phase[rsquared < -1, unique(id)]) {
  error_phase[id == i, lines(time, rr_denoised, col = i)]
  error_phase[id == i, lines(time, pred_rr, col = i, lty = 2)]
}

### Unique MAPE and RMSE for each id 
error_phase <- error_phase[, lapply(.SD, unique), list(id, phase), .SDcols = mape:rsquared]

error_phase[phase == "Rest", hist(rsquared)]
error_phase[phase == "Exercise", hist(rsquared)]
error_phase[phase == "Recovery", hist(rsquared)]

error_phase[, mean(rsquared), phase]
error_phase[, mean(rmse), phase]
error_phase[, mean(mape), phase]

### Bootstrap MAPE and RMSE for each phase
set.seed(1234)
boot_error_phase <- error_phase[, lapply(.SD, function(x) {
  boot = 50000
  x_len = length(x)
  vapply(seq_len(boot), function(i) {
    mean(x[sample.int(x_len, x_len, replace = TRUE)])
  }, FUN.VALUE = 1)
}), phase, .SDcols = mape:rsquared]

boot_error_phase[, mc_sample := seq_len(.N), list(phase)]

boot_error_phase[, lapply(.SD, mean), .SDcols = mape:rsquared, phase]


