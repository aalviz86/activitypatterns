# 01_data_prep.R
source(here::here("scripts","00_setup.R"))

site_levels <- c("Arauquita","Bita","Cravo Norte","Cumaribo","Puerto Gaitan","Puerto Rondon")

df <- readxl::read_excel(here::here("data","processed","processed_activity_data_2.xlsx"))

df_clean <- df %>%
  mutate(
    HourRadians = case_when(
      abs(HourRadians - 2*pi) < 1e-6 ~ 2*pi,
      HourRadians < 0 ~ (HourRadians + ceiling(abs(HourRadians)/(2*pi)) * (2*pi)) %% (2*pi),
      TRUE ~ HourRadians %% (2*pi)
    )
  ) %>%
  filter(
    between(HourRadians, 0, 2*pi),
    d_roads < quantile(d_roads, 0.99, na.rm = TRUE),
    !is.na(Site)
  ) %>%
  mutate(HourRadians = HourRadians / 2) # [0,2π] -> [0,π] for tan_half

# sanity checks
stopifnot(min(df_clean$HourRadians) >= 0, max(df_clean$HourRadians) <= 2*pi, !any(is.na(df_clean$HourRadians)))

saveRDS(df_clean, file = here::here("data","processed","df_clean.rds"))
