# 07_polar_plots.R
source(here::here("scripts","00_setup.R"))
df_clean <- readRDS(here::here("data","processed","df_clean.rds"))

site_coords <- tibble(
  Site = c("Arauquita","Bita","Cumaribo","Cravo Norte","Puerto Gaitan","Puerto Rondon"),
  lat  = c(7.03, 5.70, 4.58, 6.38, 4.32, 6.47),
  lon  = c(-70.73, -70.89, -69.77, -70.21, -72.09, -71.18)
) %>% mutate(date = as.Date("2024-06-15"))

sun_times <- suncalc::getSunlightTimes(data = site_coords, keep = c("sunrise","sunset"), tz = "America/Bogota") %>%
  left_join(site_coords, by = c("lat","lon","date")) %>%
  mutate(sunrise = lubridate::hour(sunrise) + lubridate::minute(sunrise)/60,
         sunset  = lubridate::hour(sunset)  + lubridate::minute(sunset)/60) %>%
  select(Site, sunrise, sunset)

df_clean <- df_clean %>%
  mutate(Hour = ((HourRadians * 2) / (2*pi)) * 24) %>%
  mutate(
    Site = stringi::stri_trans_general(Site, "Latin-ASCII"),
    Site = stringr::str_replace_all(Site, "[.]", " "),
    Site = stringr::str_squish(Site),
    Site = factor(Site, levels = c("Arauquita","Bita","Cravo Norte","Cumaribo","Puerto Gaitan","Puerto Rondon"))
  )

df_summary <- df_clean %>%
  filter(!is.na(Site)) %>%
  mutate(HourBin = floor(Hour)) %>%
  group_by(Site, HourBin) %>% summarise(count = n(), .groups = "drop") %>%
  group_by(Site) %>% mutate(prop = count/sum(count)) %>% ungroup() %>%
  mutate(midHour = HourBin + 0.5) %>%
  group_by(Site) %>% filter(sum(count) > 0) %>% ungroup()

# night shading per site
night_shading <- sun_times %>%
  mutate(xmin1 = 0, xmax1 = sunrise, xmin2 = sunset, xmax2 = 24) %>%
  pivot_longer(cols = c(xmin1,xmax1,xmin2,xmax2),
               names_to = c(".value","interval"),
               names_pattern = "(xmin|xmax)(\\d)") %>%
  select(Site, xmin, xmax)

p_all <- ggplot(df_summary, aes(x = midHour, y = prop)) +
  geom_rect(data = night_shading, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf),
            fill = "gray80", alpha = 0.3) +
  geom_col(fill = "black", width = 1) +
  coord_polar(start = 0, direction = 1) +
  facet_wrap(~ Site, scales = "free_y", ncol = 3) +
  scale_x_continuous(limits = c(0,24), breaks = seq(0,24,4),
                     labels = function(x) sprintf("%02d", x %% 24), expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,0.2), expand = c(0,0)) +
  labs(x = "Hour of Day", y = "Proportion of Detections")

ggsave(file.path(FIGS,"tapir_activity_circular_by_site.png"), p_all, width = 12, height = 8, dpi = 600)
write.csv(df_summary, file.path(TABS,"tapir_site_hourly_distribution.csv"), row.names = FALSE)
