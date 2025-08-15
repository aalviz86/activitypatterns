# 06_conditional_effects.R
source(here::here("scripts","00_setup.R"))
fit_final_nested <- readRDS(file.path(MODS,"fit_final_nested.rds"))

labels_map <- c(
  crops     = "Cropland %",
  forest_d  = "Dense Forest",
  forest_g  = "Gallery and Riparian Forest",
  forest_a  = "Open Forest",
  d_streams = "Distance to streams"
)
covariates <- names(labels_map)
y_lab <- NULL  # we’ll add a shared y label via cowplot

plot_list <- list()
for (cov in covariates) {
  ceff <- conditional_effects(fit_final_nested, effects = cov, re_formula = NA)
  df <- ceff[[1]] %>%
    mutate(Hour = (estimate__/(2*pi))*24,
           Lo   = (lower__/(2*pi))*24,
           Hi   = (upper__/(2*pi))*24)
  
  raw_df <- fit_final_nested$data %>%
    mutate(Hour = (HourRadians/(2*pi))*24) %>%
    select(all_of(cov), Hour)
  
  p <- ggplot(df, aes(x = .data[[cov]], y = Hour)) +
    geom_ribbon(aes(ymin = Lo, ymax = Hi), fill = "gray80", alpha = 0.3) +
    geom_line(linewidth = 1.1, color = "black") +
    geom_line(aes(y = Lo), color = "gray60", linetype = "dashed", linewidth = 0.4) +
    geom_line(aes(y = Hi), color = "gray60", linetype = "dashed", linewidth = 0.4) +
    geom_point(data = raw_df, aes(x = .data[[cov]], y = Hour),
               inherit.aes = FALSE, color = "black", alpha = 0.4, size = 1.5) +
    labs(x = labels_map[[cov]], y = NULL) +
    theme_classic(base_size = 16) +
    theme(axis.text = element_text(color = "black"),
          axis.title.x = element_text(face = "bold"))
  plot_list[[cov]] <- p
}

p_combined <- cowplot::plot_grid(plotlist = plot_list, ncol = 2, align = "hv")
final_plot <- cowplot::plot_grid(
  ggdraw() + draw_label("Predicted Peak Activity Time (h)", angle = 90, fontface = "bold", size = 16),
  p_combined, ncol = 2, rel_widths = c(0.07, 1)
)

ggsave(file.path(FIGS,"conditional_effects_combined_with_raw_points.png"),
       final_plot, width = 10, height = 6, dpi = 300)
