# 08_site_concentration_scatter.R
source(here::here("scripts","00_setup.R"))
df_clean  <- readRDS(here::here("data","processed","df_clean.rds"))

df_summary <- read.csv(file.path(TABS,"tapir_site_hourly_distribution.csv"))
if (!"midHour" %in% names(df_summary)) {
  # re-create if missing (when running this script standalone)
  df_summary <- df_clean %>%
    mutate(Hour = ((HourRadians * 2) / (2*pi)) * 24,
           HourBin = floor(Hour)) %>%
    group_by(Site, HourBin) %>% summarise(count = n(), .groups = "drop") %>%
    group_by(Site) %>% mutate(prop = count/sum(count)) %>% ungroup() %>%
    mutate(midHour = HourBin + 0.5)
}

# concentration per site (ρ) and join covariates
site_conc_obs <- df_summary %>%
  group_by(Site) %>%
  summarise(
    theta = list((midHour/24)*2*pi),
    w = list(prop), .groups = "drop"
  ) %>%
  mutate(concentration_rho = purrr::map2_dbl(theta, w, ~{
    xbar <- sum(.y * cos(.x)); ybar <- sum(.y * sin(.x))
    sqrt(xbar^2 + ybar^2) / sum(.y)
  })) %>% select(Site, concentration_rho)

site_covs <- df_clean %>%
  group_by(Site) %>%
  summarize(
    forest_d = mean(forest_d), forest_g = mean(forest_g),
    crops = mean(crops), forest_a = mean(forest_a),
    d_streams = mean(d_streams),
    .groups = "drop"
  )

cov_rel <- left_join(site_conc_obs, site_covs, by="Site") %>%
  pivot_longer(cols = -c(Site, concentration_rho),
               names_to = "covariate", values_to = "value")

# site-level n and CI for ρ
site_stats <- df_summary %>%
  group_by(Site) %>%
  summarise(
    n_obs = sum(count),
    concentration_rho = {θ <- (midHour/24)*2*pi; w <- prop
    sqrt((sum(w*cos(θ)))^2 + (sum(w*sin(θ)))^2) / sum(w)},
    .groups = "drop"
  ) %>%
  mutate(se_rho = sqrt((1 - concentration_rho^2)/n_obs),
         rho_lower = pmax(0, concentration_rho - 1.96*se_rho),
         rho_upper = pmin(1, concentration_rho + 1.96*se_rho))

cov_rel2 <- cov_rel %>%
  mutate(Site = as.character(Site)) %>%
  left_join(site_stats %>% mutate(Site = as.character(Site)) %>%
              select(Site, rho_lower, rho_upper), by = "Site")

labels_map <- c(
  crops     = "Cropland %",
  forest_d  = "Dense Forest",
  forest_g  = "Gallery and Riparian Forest",
  forest_a  = "Open Forest",
  d_streams = "Distance to streams"
)

shapes_map <- c("Arauquita"=17,"Bita"=16,"Cravo Norte"=15,"Cumaribo"=18,"Puerto Gaitan"=19,"Puerto Rondon"=20)

p_covs_clean <- ggplot(cov_rel2, aes(x = value, y = concentration_rho, shape = Site)) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE,
              linetype = "dashed", color = "black", fill = "grey80",
              linewidth = 0.8, fullrange = FALSE, show.legend = FALSE) +
  geom_point(size = 3, color = "black") +
  facet_wrap(~ covariate, scales = "free_x", ncol = 3,
             labeller = as_labeller(labels_map), strip.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  scale_shape_manual(values = shapes_map,
                     guide = guide_legend(title = NULL, override.aes = list(size = 4))) +
  labs(x = NULL, y = "Activity Concentration (ρ)") +
  theme_classic(base_size = 15) +
  theme(strip.placement = "outside", strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        panel.spacing = grid::unit(0.25,"lines"),
        axis.line = element_line(color = "black"),
        legend.position = c(0.93, 0.07),
        legend.justification = c(1, 0.25),
        legend.background = element_rect(fill = "white", color = "grey80"))
ggsave(file.path(FIGS,"concentration_vs_cov_clean_ci_BW.png"),
       p_covs_clean, width = 9, height = 5, dpi = 300)
