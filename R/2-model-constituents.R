# Prepare workspace -------------------------------------------------------

## Load libraries ----
library(ggplot2)
library(data.table)
library(ggsci)

# Simulate data -----------------------------------------------------------

sim_data <- data.table(t = seq(0, 20, length.out = 80))

sim_data[, `:=`(alpha = 800,
                `italic(f)[1](t)` = round(-400 / (1 + exp(log(1 - 0.7)*(t - 5)))),
                `italic(f)[2](t)` = round((400 * 0.8) / (1 + exp(log(1 - 0.7)*(t - 7)))))]

sim_data[, `alpha + italic(f)[1](t) + italic(f)[2](t)` := 
           alpha + `italic(f)[1](t)` + `italic(f)[2](t)`]

sim_data <- melt(sim_data, id.vars = "t", value.name = "RRi", variable.name = "Parameter")


# Visualize model constituents --------------------------------------------

figure <- ggplot(sim_data, aes(t, RRi)) +
  facet_wrap(~ Parameter, scales = "free_y", labeller = label_parsed, nrow = 1) +
  geom_line(col = "black", show.legend = FALSE, linewidth = 1/4) +
  geom_vline(xintercept = c(5,7), color = "gray50", linewidth = 1/4, linetype = 2) +
  scale_y_continuous(expand = c(.05, 5), n.breaks = 4) +
  scale_x_continuous(limits = c(0, 15)) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        axis.text = element_text(size = rel(.8))) +
  labs(x = "Time (minutes)", y = "RRi (ms)")

ggsave("figures/model-constituents.pdf", figure, "pdf", 
       width = 7, height = 4, units = "in")
ggsave("figures/model-constituents.png", figure, "png", 
       width = 7, height = 4, units = "in", dpi = 500)
