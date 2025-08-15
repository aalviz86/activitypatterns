# 11_interaction_models.R
source(here::here("scripts","00_setup.R"))
df_clean <- readRDS(here::here("data","processed","df_clean.rds"))

covariates <- c("forest_d","forest_g","crops","forest_a","d_streams")
pairs <- combn(covariates, 2, simplify = FALSE)

out_dir <- here::here("results","models","interaction_models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

priors <- c(
  prior(normal(0, 0.5), class = "b"),
  prior(gamma(3, 1), class = "sd"),
  prior(gamma(2, 0.5), class = "kappa"),
  prior(normal(0, 1), class = "Intercept")
)

for (pair in pairs) {
  iv1 <- pair[1]; iv2 <- pair[2]
  name <- paste(iv1, iv2, sep = "_")
  form_str <- paste0("HourRadians ~ forest_d + forest_g + crops + forest_a + ",
                     iv1, ":", iv2, " + (1|Site/CT)")
  message("Fitting interaction: ", iv1, " × ", iv2)
  
  fit_i <- brm(
    formula = bf(as.formula(form_str)),
    family  = von_mises(link = "tan_half"),
    data    = df_clean,
    prior   = priors,
    chains  = 8, iter = 16000, warmup = 6000,
    cores   = max(1, parallel::detectCores()-1),
    control = list(adapt_delta = 0.99, max_treedepth = 20),
    seed    = 123
  )
  
  saveRDS(fit_i, file = file.path(out_dir, paste0("fit_", name, ".rds")))
  coef_df <- as.data.frame(summary(fit_i)$fixed)
  readr::write_csv(coef_df, file.path(out_dir, paste0("coef_", name, ".csv")))
  
  ce <- conditional_effects(fit_i, effects = paste0(iv1, ":", iv2), re_formula = NULL)[[1]]
  p_ce <- ggplot(ce, aes(x = .data[[iv1]], y = estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "grey80", alpha = 0.5) +
    geom_line(size = 1) +
    labs(title = paste("Interaction", iv1, "×", iv2), x = iv1, y = paste("Predicted Hour (effect of", iv2, ")"))
  ggsave(file.path(out_dir, paste0("interaction_", name, ".png")), p_ce, width = 7, height = 5, dpi = 300)
}
