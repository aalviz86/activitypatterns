# 03_random_effects_plots.R
source(here::here("scripts","00_setup.R"))
fit_final_nested <- readRDS(file.path(MODS,"fit_final_nested.rds"))

re_nested <- ranef(fit_final_nested)

site_re <- as.data.frame(re_nested$Site[,,"Intercept"]) %>%
  tibble::rownames_to_column("Site") %>%
  rename(Estimate = Estimate, Lower = Q2.5, Upper = Q97.5)

ct_re <- as.data.frame(re_nested$`Site:CT`[,,"Intercept"]) %>%
  tibble::rownames_to_column("Site_CT") %>%
  rename(Estimate = Estimate, Lower = Q2.5, Upper = Q97.5)

p_site <- ggplot(site_re, aes(x = reorder(Site, Estimate), y = Estimate)) +
  geom_point() + geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  coord_flip() + labs(x = "Site", y = "Random Intercept (rad)")
ggsave(file.path(FIGS,"random_intercepts_site.png"), p_site, width = 6, height = 4, dpi = 300)

p_ct <- ggplot(ct_re, aes(x = reorder(Site_CT, Estimate), y = Estimate)) +
  geom_point() + geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
  coord_flip() + labs(x = "Site:CT", y = "Random Intercept (rad)")
ggsave(file.path(FIGS,"random_intercepts_ct.png"), p_ct, width = 6, height = 6, dpi = 300)
