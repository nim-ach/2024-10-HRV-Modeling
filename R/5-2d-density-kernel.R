
# Prepare workspace -------------------------------------------------------

## Load libraries
library(ggplot2)
library(data.table)

## Load data
data("rri_data")


# Generate figure ---------------------------------------------------------

figure <- ggplot(rri_data, aes(time, rri_std)) +
  stat_density_2d_filled(show.legend = FALSE, adjust = 0.75, na.rm = TRUE) +
  scale_x_continuous(expand = c(0,0), name = "Time (minutes)") +
  scale_y_continuous(expand = c(0,0), name = "RRi (ms)") +
  scale_fill_brewer() +
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(size = rel(0.8)))

ggsave("figures/2d-density-kernel.pdf", figure, "pdf", 
       width = 7, height = 4, units = "in")
ggsave("figures/2d-density-kernel.png", figure, "png", 
       width = 7, height = 4, units = "in", dpi = 500)
