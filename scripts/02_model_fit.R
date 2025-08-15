# 02_model_fit.R
source(here::here("scripts","00_setup.R"))
df_clean <- readRDS(here::here("data","processed","df_clean.rds"))

priors <- c(
  prior(normal(0, 0.5), class = "b"),
  prior(gamma(3, 1), class = "sd"),
  prior(gamma(2, 0.5), class = "kappa"),
  prior(normal(0, 1), class = "Intercept")
)

fit_final <- brm(
  HourRadians ~ forest_d + crops + forest_g + forest_a + (1 | Site),
  family = von_mises(link = "tan_half"),
  data = df_clean, prior = priors,
  chains = 8, cores = 4, iter = 16000, warmup = 6000,
  control = list(adapt_delta = 0.99, max_treedepth = 20),
  seed = 123
)

fit_final_nested <- brm(
  HourRadians ~ forest_d + crops + forest_g + forest_a + d_streams + (1 | Site/CT),
  family = von_mises(link = "tan_half"),
  data = df_clean, prior = priors,
  chains = 8, cores = 4, iter = 16000, warmup = 6000,
  control = list(adapt_delta = 0.99, max_treedepth = 20),
  seed = 123
)

loo_final         <- loo(fit_final)
loo_final_nested  <- loo(fit_final_nested)

saveRDS(fit_final,         file = file.path(MODS,"fit_final.rds"))
saveRDS(fit_final_nested,  file = file.path(MODS,"fit_final_nested.rds"))
saveRDS(loo_final,         file = file.path(TABS,"loo_final.rds"))
saveRDS(loo_final_nested,  file = file.path(TABS,"loo_final_nested.rds"))
