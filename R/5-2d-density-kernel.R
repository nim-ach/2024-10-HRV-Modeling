
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
  scale_fill_grey(start = 1, end = 0) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
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
  facet_wrap(~ variable, nrow = 2, scales = "free_x") +
  ggdist::stat_slabinterval(
    fill = "gray",
    normalize = "panels",
    adjust = 20, trim = FALSE,
    density = "unbounded",
    show.legend = FALSE
  ) +
  scale_y_continuous(breaks = NULL, name = NULL, expand = c(0.1,0,0.1,0)) +
  scale_x_continuous(name = "RRi (ms)", n.breaks = 4) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(axis.text = element_text(size = rel(0.8)),
        plot.margin = margin(0.0825,.25,0.0825,.25, "in"),
        strip.background = element_rect(colour = NA, fill = "gray90"))

figure <- ggpubr::ggarrange(
  figure_b, figure_a, ncol = 2, nrow = 1,
  widths = c(1,2), labels = c("A.", "B."), font.label = list(size = 12),
  hjust = -0.2, vjust = 2
)

ggsave("figures/2d-density-kernel.pdf", figure, "pdf", 
       width = 7, height = 4, units = "in")
ggsave("figures/2d-density-kernel.png", figure, "png", 
       width = 7, height = 4, units = "in", dpi = 500)
