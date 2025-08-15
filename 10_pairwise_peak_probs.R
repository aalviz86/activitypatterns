# 10_pairwise_peak_probs.R
source(here::here("scripts","00_setup.R"))
fit_final_nested <- readRDS(file.path(MODS,"fit_final_nested.rds"))

post <- as_draws_df(fit_final_nested)
re_site <- post %>%
  select(.draw, starts_with("r_Site[")) %>%
  pivot_longer(-.draw, names_to = "Site", values_to = "delta_mu") %>%
  mutate(Site = str_remove_all(Site, "^r_Site\\[|,Intercept\\]$"))

post_site <- re_site %>%
  left_join(post %>% select(.draw, mu_global = b_Intercept), by = ".draw") %>%
  mutate(mu_site = mu_global + delta_mu,
         hour_site = ((mu_site %% (2*pi)) / (2*pi)) * 24)

sites <- unique(post_site$Site)
pairwise_prob <- expand.grid(A=sites, B=sites, KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE) %>%
  filter(A != B) %>%
  rowwise() %>%
  mutate(p_A_before_B = mean(post_site$hour_site[post_site$Site==A] <
                               post_site$hour_site[post_site$Site==B])) %>%
  ungroup()

write.csv(pairwise_prob, file.path(TABS,"site_pairwise_peak_order_probs.csv"), row.names = FALSE)
