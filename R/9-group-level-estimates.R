
# Prepare workspace -------------------------------------------------------

## Load libraries
library(data.table)
library(ggplot2)
library(ggdist)

## Load data
m_fits_df <- fread(file = "models/group_level_posterior.csv")
m_fits_df[, Parameter := factor(Parameter, 
                                levels = c("alpha", "beta", "c", "lambda", 
                                           "phi", "delta", "tau", "sigma"))]



# Plot dens only ----------------------------------------------------------

f_1 <- ggplot(m_fits_df, aes(Parameter, b_Intercept)) +
  facet_wrap(~ Parameter, nrow = 1, ncol = 8, scales = "free",
             labeller = label_parsed) +
  ggdist::stat_halfeye(fill = "gray",
                       show.legend = FALSE,
                       normalize = "panels",
                       trim = FALSE, density = "unbounded") +
  scale_x_discrete(expand = c(0.2,0,0.2,0), 
                   name = NULL, breaks = NULL) +
  scale_y_continuous(name = expression(symbol(E)~"["*theta*"]")) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        axis.text = element_text(size = rel(.8)))

f_2 <- 
  m_fits_df[Parameter %in% c("lambda", "phi"), 1 - exp(b_Intercept), Parameter] |> 
  ggplot(aes(V1, Parameter)) +
  facet_wrap(~ Parameter, nrow = 1, ncol = 2, scales = "free",
             labeller = label_parsed) +
  ggdist::stat_halfeye(fill = "gray",
                       show.legend = FALSE,
                       normalize = "panels",
                       trim = FALSE, density = "unbounded") +
  scale_y_discrete(expand = c(0.15,0,0.2,0), 
                   name = NULL, breaks = NULL) +
  scale_x_continuous(name = "Rate of change per minute (%)",
                     labels = scales::label_percent(),
                     n.breaks = 4,
                     expand = c(0.2,0)) +
  theme_classic(base_line_size = 1/4, base_size = 12) +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        axis.text = element_text(size = rel(.8)))

figure <- ggpubr::ggarrange(
  f_1, f_2, ncol = 1, nrow = 2, heights = c(7,3),
  labels = c("A.", "B."), font.label = list(size = 12),
  vjust = c(1.5, 2.5)
)

ggsave("figures/group-level-estimates.pdf", figure, "pdf", 
       width = 7, height = 5, units = "in")
ggsave("figures/group-level-estimates.png", figure, "png", 
       width = 7, height = 5, units = "in", dpi = 500)

# Plot data ---------------------------------------------------------------

# dens_plot <- function(param, limits = c(NA,NA), col = 1) {
#   color_opts <- ggsci::pal_observable()(8)
#   ggplot(m_fits_df[Parameter == param], aes(Parameter, b_Intercept)) +
#     ggdist::stat_halfeye(fill = color_opts[col],
#                          show.legend = FALSE,
#                          normalize = "panels",
#                          trim = FALSE, density = "unbounded") +
#     scale_x_discrete(expand = c(0.1,0,0.2,0), 
#                      name = NULL, breaks = NULL) +
#     scale_y_continuous(name = NULL, breaks = NULL,
#                        limits = limits) +
#     theme_classic(base_size = 12) +
#     theme(strip.background = element_rect(fill = "grey90", colour = NA),
#           axis.text = element_text(size = rel(.8)),
#           plot.margin = margin(0.1,0.1,0.1,0,"in"))
# }
# 
# trace_plot <- function(param, limits = c(NA, NA), col = 1, adj = .1) {
#   color_opts <- ggsci::pal_observable()(8)
#   m_fits_df[Parameter == param,] |> 
#     ggplot(aes(.iteration, b_Intercept, group = ordered(.chain), alpha = ordered(.chain))) +
#     geom_line(show.legend = FALSE, col = color_opts[col]) +
#     scale_y_continuous(name = str2expression(paste0("symbol(E)~'['*",param,"*']'")),
#                        limits = limits) +
#     scale_x_continuous(name = NULL, breaks = NULL,
#                        expand = c(0,0)) +
#     theme_classic(base_size = 12) +
#     theme(axis.text = element_text(size = rel(.8)),
#           plot.margin = margin(0.1,0,0.1,0.1,"in"),
#           axis.text.y.left = element_text(
#             margin = margin(0,.025,0,adj,"in")
#           ))
# }
# 
# plot_param <- function(param, limits = NULL, col = 1, adj = .05) {
#   if (is.null(limits)) {
#     limits <- m_fits_df[Parameter == param, range(b_Intercept)]
#     limits[1] <- limits[1] - diff(limits) * 0.06
#     limits[2] <- limits[2] + diff(limits) * 0.06
#   }
#   ggpubr::ggarrange(
#     trace_plot(param, limits, col, adj), 
#     dens_plot(param, limits, col), 
#     ncol = 2, widths = c(4,1), align = "h"
#   )
# }
# 
# figure <- ggpubr::ggarrange(
#   plot_param("alpha", col = 1, adj = .088)
#   , plot_param("beta", col = 2, adj = .0)
#   , plot_param("c", col = 3)
#   , plot_param("lambda", col = 4, adj = .045)
#   , plot_param("phi", col = 5, adj = .04)
#   , plot_param("tau", col = 6, adj = .1325)
#   , plot_param("delta", col = 7, adj = .045)
#   , plot_param("sigma", col = 8, adj = .1725)
#   , nrow = 4, ncol = 2, align = "hv"
# )
# 
# ggsave("figures/group-level-estimates.pdf", figure, "pdf", 
#        width = 7, height = 5, units = "in")
# ggsave("figures/group-level-estimates.png", figure, "png", 
#        width = 7, height = 5, units = "in", dpi = 500)
