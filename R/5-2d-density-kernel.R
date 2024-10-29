
# Prepare workspace -------------------------------------------------------

## Load libraries
library(ggplot2)
library(data.table)

## Load data
data("rri_data")


# Generate figure ---------------------------------------------------------

rri_data[, time_cat := cut(time, breaks = seq(0, 20, by = 1), 
               labels = seq(0, 20, by = 1)[-1]) |> 
           as.character() |> as.numeric()]

estimate <- rri_data[, list(mean = mean(rr_denoised, na.rm = T),
                            se = sd(rr_denoised, na.rm = T)), 
                     list(time = as.numeric(as.character(time_cat)))]

figure <- ggplot(rri_data, aes(time, rr_denoised)) +
  stat_density_2d_filled(show.legend = FALSE, adjust = 1/3, na.rm = TRUE) +
  scale_x_continuous(expand = c(0,0), name = "Time (minutes)") +
  scale_y_continuous(expand = c(0,0), name = "RRi (ms)") +
  scale_fill_brewer() +
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(size = rel(0.8)))

ggsave("figures/2d-density-kernel.pdf", figure, "pdf", 
       width = 7, height = 4, units = "in")
ggsave("figures/2d-density-kernel.png", figure, "png", 
       width = 7, height = 4, units = "in", dpi = 500)
