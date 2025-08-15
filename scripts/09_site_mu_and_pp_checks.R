# 09_site_mu_and_pp_checks.R
source(here::here("scripts","00_setup.R"))
fit_final_nested <- readRDS(file.path(MODS,"fit_final_nested.rds"))
df_clean <- readRDS(here::here("data","processed","df_clean.rds"))

# site-level μ draws and hour-site summary
site_mu_draws <- fit_final_nested %>%
  spread_draws(b_Intercept, r_Site[Site,Intercept]) %>%
  mutate(mu_site = b_Intercept + r_Site,
         hour_site = ((mu_site + pi) / (2*pi)) * 24)

site_mu_summ <- site_mu_draws %>%
  group_by(Site) %>%
  summarize(mean_hour = median(hour_site),
            lo_hour = quantile(hour_site, 0.025),
            hi_hour = quantile(hour_site, 0.975), .groups = "drop")

# observed concentration per site for the scatter with CIs
df_summary <- read.csv(file.path(TABS,"tapir_site_hourly_distribution.csv"))
site_conc_obs <- df_summary %>%
  group_by(Site) %>%
  summarize(
    theta = list((midHour/24)*2*pi),
    w = list(prop), .groups="drop"
  ) %>%
  mutate(concentration_rho = purrr::map2_dbl(theta, w, ~{
    xbar <- sum(.y * cos(.x)); ybar <- sum(.y * sin(.x))
    sqrt(xbar^2 + ybar^2) / sum(.y)
  })) %>% select(Site, concentration_rho)

site_summary <- left_join(site_mu_summ, site_conc_obs, by = "Site")

p_sites <- ggplot(site_summary, aes(x = mean_hour, y = concentration_rho, label = Site)) +
  geom_errorbarh(aes(xmin = lo_hour, xmax = hi_hour), height = 0.08, color = "gray50", linewidth = 0.9) +
  geom_point(size = 4, color = "#2171B5", fill = "#2171B5", shape = 21, stroke = 1.1) +
  ggrepel::geom_text_repel(size = 5, fontface = "bold", color = "black", max.overlaps = 10) +
  scale_x_continuous(limits = c(12,24), breaks = seq(12,24,2),
                     labels = c("12:00","14:00","16:00","18:00","20:00","22:00","00:00")) +
  scale_y_continuous(limits = c(0,0.8), breaks = seq(0,0.8,0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Mean Peak Hour", y = "Concentration (ρ)")
ggsave(file.path(FIGS,"Site_peak_and_concentration.png"), p_sites, width=8, height=5, dpi=600)

# Posterior predictive checks (binned to hours, per site)
y_pp <- posterior_predict(fit_final_nested, ndraws = 200)

df_pp <- as_tibble(y_pp) %>%
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, names_to = "obs", values_to = "rad") %>%
  mutate(obs = as.integer(str_remove(obs, "V")),
         Site = df_clean$Site[obs],
         hour = ((rad + pi) / (2*pi)) * 24,
         HourBin = floor(hour))

pred_site <- df_pp %>%
  group_by(draw, Site, HourBin) %>% summarise(n = n(), .groups="drop") %>%
  group_by(draw, Site) %>% mutate(prop = n/sum(n)) %>% ungroup()

obs_site <- df_summary

p_ppc <- ggplot() +
  stat_summary(
    data = pred_site,
    aes(x = HourBin + 0.5, y = prop, group = Site),
    fun.data = function(x) {
      qs <- quantile(x, c(0.025, 0.5, 0.975))
      data.frame(ymin = qs[1], y = qs[2], ymax = qs[3])
    },
    geom = "ribbon", fill = "#377eb8", alpha = 0.3
  ) +
  geom_col(data = obs_site, aes(x = midHour, y = prop), width = 1, fill = "black") +
  coord_polar(start = 0, direction = 1) +
  facet_wrap(~ Site, ncol = 3) +
  scale_x_continuous(breaks = seq(0,24,4), labels = sprintf("%02d", seq(0,24,4)), expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  labs(x = "Hour of Day", y = "Proportion of Activity")

ggsave(file.path(FIGS,"ppc_by_site_observed_vs_predicted.png"), p_ppc, width = 10, height = 6, dpi = 300)
