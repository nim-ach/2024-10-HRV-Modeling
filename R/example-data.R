
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(ggplot2)

## Load data
load("data/rri_data.RData")

## Generate graphics
figure <- ggplot(rri_data[id %in% c(1:3,7:9)], aes(time, rr_denoised)) +
  facet_wrap(~ id, nrow = 2, ncol = 3) +
  geom_line(lwd = 1/4, na.rm = TRUE) +
  labs(x = "Time (min)", y = "RRi (ms)") +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(strip.background = element_blank(), strip.text = element_blank())
  
## Save the plot
ggsave("figures/figure-1.pdf", figure, "pdf", 
       width = 7, height = 5, units = "in")
ggsave("figures/figure-1.eps", figure, "eps", 
       width = 7, height = 5, units = "in")
ggsave("figures/figure-1.png", figure, "png", 
       width = 7, height = 5, units = "in", dpi = 500)
  
