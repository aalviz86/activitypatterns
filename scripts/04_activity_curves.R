# 04_activity_curves.R
source(here::here("scripts","00_setup.R"))
fit_final_nested <- readRDS(file.path(MODS,"fit_final_nested.rds"))
df_clean <- readRDS(here::here("data","processed","df_clean.rds"))

posterior_draws <- as_draws_df(fit_final_nested)
ct_cols <- names(posterior_draws)[grepl("^r_Site:CT\\[.*?,Intercept\\]$", names(posterior_draws))]
ct_terms <- dplyr::select(posterior_draws, dplyr::all_of(ct_cols))
ct_names <- names(ct_terms) %>% str_remove("r_Site:CT\\[") %>% str_remove(",Intercept\\]")
ct_effects <- ct_terms + posterior_draws$b_Intercept
colnames(ct_effects) <- ct_names

angles <- circular(seq(0, 2*pi, length.out = 500), units = "radians")
kappa_est <- posterior_summary(fit_final_nested, variable = "kappa")[,"Estimate"]

library(future.apply)
plan(multisession)

get_vm_density_parallel <- function(mu_samples_list, kappa, angles, n_draws = 1000) {
  future_lapply(mu_samples_list, function(mu_vec) {
    mu_vec <- sample(mu_vec, n_draws)
    dens_matrix <- sapply(mu_vec, function(m) dvonmises(angles, mu = m, kappa = kappa))
    tibble(
      mean  = rowMeans(dens_matrix),
      lower = apply(dens_matrix, 1, quantile, probs = 0.1),
      upper = apply(dens_matrix, 1, quantile, probs = 0.9)
    )
  })
}

mu_samples_list <- as.list(as.data.frame(ct_effects))
density_list <- get_vm_density_parallel(mu_samples_list, kappa_est, angles, n_draws = 1000)
names(density_list) <- names(mu_samples_list)

density_df <- purrr::imap_dfr(density_list, function(df, name) {
  df %>% mutate(angle = as.numeric(angles), CT = name)
}) %>%
  mutate(hour = (angle * 12 / pi) %% 24,
         Site = stringr::str_extract(CT, "^[^:_\\-]+")) %>%
  group_by(CT) %>%
  mutate(mean = mean / max(mean, na.rm = TRUE),
         lower = lower / max(mean, na.rm = TRUE),
         upper = upper / max(mean, na.rm = TRUE)) %>%
  ungroup()

# mean curve per site
density_site <- density_df %>%
  group_by(Site, angle, hour) %>%
  summarise(mean = mean(mean), lower = mean(lower), upper = mean(upper), .groups = "drop")

p_site <- ggplot(density_site, aes(x = hour, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, fill="grey80") +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Site, scales = "free_y") +
  labs(x = "Time of Day", y = "Normalized Activity Density")
ggsave(file.path(FIGS,"tapir_mean_activity_per_site.png"), p_site, width = 10, height = 6, dpi = 600)

# all CTs per site
p_all_ct <- ggplot(density_df, aes(x = hour, y = mean, group = CT)) +
  geom_line(alpha = 0.4, linewidth = 0.5, color = "black") +
  facet_wrap(~ Site, scales = "free_y") +
  labs(x = "Time of Day", y = "Normalized Activity Density")
ggsave(file.path(FIGS,"tapir_activity_curves_per_CT.png"), p_all_ct, width = 10, height = 6, dpi = 300)

write.csv(density_df, file.path(TABS,"tapir_density_curves_per_CT.csv"), row.names = FALSE)
