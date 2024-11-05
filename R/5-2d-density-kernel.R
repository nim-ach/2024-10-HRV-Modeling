
# Prepare workspace -------------------------------------------------------

## Load libraries
library(ggplot2)
library(data.table)

## Load data
data("rri_data")


# Generate figure ---------------------------------------------------------

figure_a <- ggplot(rri_data, aes(time, rri_std)) +
  stat_density_2d_filled(show.legend = FALSE, adjust = 0.75, na.rm = TRUE) +
  scale_x_continuous(expand = c(0,0), name = "Time (minutes)") +
  scale_y_continuous(expand = c(0,0), name = "Mean Standardized RRi",
                     breaks = -3:2) +
  scale_fill_brewer() +
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(size = rel(0.8)))

figure_b <- 
  rri_data |> 
  melt.data.table(
    id.vars = "id", 
    measure.vars = c("rri_mean","rri_sd")
  ) |> within({
    levels(variable) <- c("Mean", "SD")
  }) |> 
  ggplot(aes(value)) +
  facet_grid(~ variable, scales = "free_x") +
  geom_histogram(aes(y = after_stat(density), 
                     fill = variable), 
                 bins = 15, show.legend = FALSE,
                 col = "gray10") +
  stat_density(geom = "line",
               adjust = 4,
               col = "gray10") +
  geom_boxplot(aes(y = -.001, fill = variable),
               width = .0005, orientation = "y", staplewidth = 0,
               show.legend = FALSE) +
  scale_fill_brewer() +
  scale_y_continuous(breaks = NULL, name = NULL) +
  scale_x_continuous(name = "RRi (ms)", n.breaks = 4) +
  theme_classic(base_size = 12) +
  theme(axis.text = element_text(size = rel(0.8)),
        plot.margin = margin(0.05,.15,0,.5, "in"),
        strip.background = element_rect(colour = NA, fill = "gray90"))

figure <- ggpubr::ggarrange(
  figure_b, figure_a, ncol = 1, nrow = 2,
  heights = c(2,3)
)

ggsave("figures/2d-density-kernel.pdf", figure, "pdf", 
       width = 7, height = 5, units = "in")
ggsave("figures/2d-density-kernel.png", figure, "png", 
       width = 7, height = 5, units = "in", dpi = 500)
