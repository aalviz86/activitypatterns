# ============================================================
#  04_reload_session.R
#  Tapir Diel Activity — Colombian Orinoquia
#
#  Run top-to-bottom after any R restart (~3 min).
#  Reconstructs all objects needed for figures and downstream
#  analyses without re-running the full pipeline.
#
#  REQUIRES: models/, data/, covariates/ folders with outputs
#            from 01_, 02_, 03_ scripts.
#
#  CRITICAL LOAD ORDER:
#    Libraries → constants → mix_family → models → data → derived objects
#    mix_family MUST be defined before readRDS() of any model.
# ============================================================

library(tidyverse)
library(brms)
library(circular)
library(tidybayes)
library(ggridges)
library(ggrepel)
library(suncalc)
library(lubridate)
library(scales)
library(cowplot)
library(patchwork)
library(overlap)
library(stringi)
library(future)
library(mgcv)
library(lme4)

plan(multisession, workers = parallel::detectCores() - 2)
cat("Libraries loaded.\n")


# ════════════════════════════════════════════════════════════
#  CONSTANTS
# ════════════════════════════════════════════════════════════

WD <- "~/1. TTU/00. Danta/00. Occupancy/01. Activity Patterns_2"
setwd(WD)
cat("Working directory:", getwd(), "\n")

site_levels <- c("Arauquita", "Bita", "Cravo Norte",
                  "Cumaribo", "Puerto Gaitan", "Puerto Rondon")

cov_cols <- c("forest_d", "forest_g", "forest_a", "sec_veg", "crops")

labels_map <- c(
  forest_d = "Dense Forest (ha)",
  forest_g = "Gallery & Riparian Forest (ha)",
  forest_a = "Open Forest (ha)",
  sec_veg  = "Secondary Vegetation (ha)",
  crops    = "Cropland (ha)"
)

shapes_map <- c(
  "Arauquita"     = 17,
  "Bita"          = 16,
  "Cravo Norte"   = 15,
  "Cumaribo"      = 18,
  "Puerto Gaitan" = 19,
  "Puerto Rondon" = 20
)

rad_to_hour <- function(rad) ((rad + pi) / (2 * pi)) * 24

fix_site <- function(x) {
  x %>%
    str_replace_all("ó|Ã³|ò|ô", "o") %>%
    str_replace_all("á|Ã¡|à|â", "a") %>%
    str_replace_all("é|Ã©|è|ê", "e") %>%
    str_replace_all("í|Ã­|ì|î", "i") %>%
    str_replace_all("ú|Ãº|ù|û", "u") %>%
    str_replace_all("ñ|Ã±",     "n") %>%
    str_trim() %>%
    str_replace_all("\\s+", " ")
}

back_transform <- function(z, cov) {
  sd_val <- cov_sds[[cov]]
  if (sd_val < 1e-6) return(rep(cov_means[[cov]], length(z)))
  z * sd_val + cov_means[[cov]]
}


# ════════════════════════════════════════════════════════════
#  MIX_FAMILY — MUST PRECEDE MODEL LOADING
# ════════════════════════════════════════════════════════════

mix_family <- mixture(von_mises(), von_mises(), order = "mu")
cat("mix_family defined.\n")


# ════════════════════════════════════════════════════════════
#  LOAD MODELS
# ════════════════════════════════════════════════════════════

cat("Loading models...\n")
fit_base <- readRDS("models/rebuild_mixture_base.rds")
fit_cov  <- readRDS("models/rebuild_mixture_cov.rds")
fit_site <- readRDS("models/rebuild_mixture_site.rds")

cat("Convergence:\n")
for (nm in c("base","cov","site")) {
  fit <- get(paste0("fit_", nm))
  cat(sprintf("  %-5s  max_Rhat=%.4f  min_neff=%.4f\n",
              nm, max(rhat(fit)), min(neff_ratio(fit))))
}


# ════════════════════════════════════════════════════════════
#  LOAD DATA
# ════════════════════════════════════════════════════════════

cat("\nLoading data...\n")

df_model <- read_csv("data/tapir_model_data_corine.csv",
                     show_col_types = FALSE) %>%
  mutate(Site = factor(fix_site(Site), levels = site_levels))

df_clean <- readxl::read_excel("tapir_activity_clean.xlsx") %>%
  mutate(Site = factor(fix_site(Site), levels = site_levels))

covariates_raw <- read_csv("covariates/camera_covariates_corine_final.csv",
                           show_col_types = FALSE) %>%
  mutate(Site = factor(fix_site(Site), levels = site_levels))

cov_means <- readRDS("covariates/cov_means.rds")
cov_sds   <- readRDS("covariates/cov_sds.rds")

cat(sprintf("  df_model: %d detections, %d cameras, %d sites\n",
            nrow(df_model), n_distinct(df_model$CT),
            n_distinct(df_model$Site)))


# ════════════════════════════════════════════════════════════
#  POSTERIOR DRAWS
# ════════════════════════════════════════════════════════════

cat("Extracting posterior draws...\n")
post_base       <- as_draws_df(fit_base)
post_cov        <- as_draws_df(fit_cov)
post_site_draws <- as_draws_df(fit_site)

p1h <- rad_to_hour(post_base$b_mu1_Intercept)
p2h <- rad_to_hour(post_base$b_mu2_Intercept)


# ════════════════════════════════════════════════════════════
#  DERIVED OBJECTS FOR FIGURES
# ════════════════════════════════════════════════════════════

# Hourly summary per site
df_summary <- df_model %>%
  mutate(HourBin = floor(Hour)) %>%
  group_by(Site, HourBin) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Site) %>%
  mutate(prop = count / sum(count), midHour = HourBin + 0.5) %>%
  ungroup()

# Activity concentration per site (ρ)
site_conc <- df_summary %>%
  group_by(Site) %>%
  summarise(
    concentration_rho = {
      theta <- (midHour/24)*2*pi; w <- prop
      sqrt(sum(w*cos(theta))^2 + sum(w*sin(theta))^2) / sum(w)
    },
    n_obs = sum(count),
    .groups = "drop"
  ) %>%
  mutate(
    se_rho    = sqrt((1 - concentration_rho^2) / n_obs),
    rho_lower = pmax(0, concentration_rho - 1.96*se_rho),
    rho_upper = pmin(1, concentration_rho + 1.96*se_rho)
  )

# Site-level posterior peaks from fit_site
site_peaks <- tibble(
  Site     = "Arauquita",
  mu_draws = list(post_site_draws$b_mu1_Intercept)
) %>%
  bind_rows(tibble(
    Site = c("Bita","Cravo Norte","Cumaribo","Puerto Gaitan","Puerto Rondon"),
    mu_draws = list(
      post_site_draws$b_mu1_Intercept + post_site_draws$b_mu1_SiteBita,
      post_site_draws$b_mu1_Intercept + post_site_draws$b_mu1_SiteCravoNorte,
      post_site_draws$b_mu1_Intercept + post_site_draws$b_mu1_SiteCumaribo,
      post_site_draws$b_mu1_Intercept + post_site_draws$b_mu1_SitePuertoGaitan,
      post_site_draws$b_mu1_Intercept + post_site_draws$b_mu1_SitePuertoRondon
    )
  )) %>%
  mutate(
    mean_hour = map_dbl(mu_draws, ~median(rad_to_hour(.))),
    lo_hour   = map_dbl(mu_draws, ~quantile(rad_to_hour(.), .025)),
    hi_hour   = map_dbl(mu_draws, ~quantile(rad_to_hour(.), .975))
  )

site_summary_plot <- left_join(site_peaks, site_conc, by = "Site")

# Site sunrise/sunset
site_coords <- tibble(
  Site = site_levels,
  lat  = c(7.03, 5.70, 6.38, 4.58, 4.32, 6.47),
  lon  = c(-70.73, -70.89, -70.21, -69.77, -72.09, -71.18)
) %>% mutate(date = as.Date("2024-06-15"))

sun_times_clean <- getSunlightTimes(data=site_coords,
                                    keep=c("sunrise","sunset"),
                                    tz="America/Bogota") %>%
  left_join(site_coords, by=c("lat","lon","date")) %>%
  mutate(sunrise = hour(sunrise) + minute(sunrise)/60,
         sunset  = hour(sunset)  + minute(sunset)/60) %>%
  select(Site, sunrise, sunset)

night_shading <- sun_times_clean %>%
  mutate(xmin1=0, xmax1=sunrise, xmin2=sunset, xmax2=24) %>%
  pivot_longer(cols=c(xmin1,xmax1,xmin2,xmax2),
               names_to=c(".value","interval"),
               names_pattern="(xmin|xmax)(\\d)") %>%
  select(Site, xmin, xmax)

# Binned data for GAMM figures
df_binned <- df_model %>%
  mutate(HourBin = floor(Hour)) %>%
  count(Site, CT, HourBin, name="count") %>%
  complete(nesting(Site,CT), HourBin=0:23, fill=list(count=0)) %>%
  mutate(
    CT_id     = as.integer(factor(CT)),
    cos_hour  = cos(2*pi*HourBin/24),
    sin_hour  = sin(2*pi*HourBin/24),
    log_count = log(count + 0.5)
  )

# Mixture density curve (memory-safe: uses posterior medians only)
cat("Building mixture density curve...\n")
mu1 <- median(post_base$b_mu1_Intercept)
mu2 <- median(post_base$b_mu2_Intercept)
k1  <- median(post_base$kappa1)
k2  <- median(post_base$kappa2)
w1  <- median(post_base$theta1)
w2  <- median(post_base$theta2)

hour_seq  <- seq(0, 24, length.out=200)
theta_seq <- (hour_seq/24)*2*pi - pi

dens_mean <- w1*circular::dvonmises(theta_seq, mu=mu1, kappa=k1) +
             w2*circular::dvonmises(theta_seq, mu=mu2, kappa=k2)
dens_lo   <- w1*circular::dvonmises(theta_seq, mu=quantile(post_base$b_mu1_Intercept,.025), kappa=k1) +
             w2*circular::dvonmises(theta_seq, mu=quantile(post_base$b_mu2_Intercept,.025), kappa=k2)
dens_hi   <- w1*circular::dvonmises(theta_seq, mu=quantile(post_base$b_mu1_Intercept,.975), kappa=k1) +
             w2*circular::dvonmises(theta_seq, mu=quantile(post_base$b_mu2_Intercept,.975), kappa=k2)

density_df <- tibble(
  hour  = hour_seq,
  mean  = dens_mean / max(dens_mean),
  lower = pmin(dens_lo, dens_hi) / max(dens_mean),
  upper = pmax(dens_lo, dens_hi) / max(dens_mean)
)
rm(dens_mean, dens_lo, dens_hi); gc()


# ════════════════════════════════════════════════════════════
#  LOO (from cache or recompute)
# ════════════════════════════════════════════════════════════

if (file.exists("loo_cache.rds")) {
  loo_list <- readRDS("loo_cache.rds")
  loo_base <- loo_list$loo_base
  loo_cov  <- loo_list$loo_cov
  loo_site <- loo_list$loo_site
  cat("LOO loaded from cache.\n")
} else {
  cat("Computing LOO (~2 min)...\n")
  loo_base <- loo(fit_base)
  loo_cov  <- loo(fit_cov)
  loo_site <- loo(fit_site)
  saveRDS(list(loo_base=loo_base, loo_cov=loo_cov, loo_site=loo_site),
          "loo_cache.rds")
}


# ════════════════════════════════════════════════════════════
#  SUMMARY
# ════════════════════════════════════════════════════════════

cat("\n========== SESSION LOADED ==========\n")
cat(sprintf("Detections: %d  Cameras: %d  Sites: %d\n",
            nrow(df_model), n_distinct(df_model$CT),
            n_distinct(df_model$Site)))
cat(sprintf("\nPre-dawn: %02.0f:%02.0f h  [%02.0f:%02.0f–%02.0f:%02.0f]  κ=%.2f  θ=%.0f%%\n",
    floor(median(p1h)), (median(p1h)%%1)*60,
    floor(quantile(p1h,.025)), (quantile(p1h,.025)%%1)*60,
    floor(quantile(p1h,.975)), (quantile(p1h,.975)%%1)*60,
    median(post_base$kappa1), median(post_base$theta1)*100))
cat(sprintf("Evening:  %02.0f:%02.0f h  [%02.0f:%02.0f–%02.0f:%02.0f]  κ=%.2f  θ=%.0f%%\n",
    floor(median(p2h)), (median(p2h)%%1)*60,
    floor(quantile(p2h,.025)), (quantile(p2h,.025)%%1)*60,
    floor(quantile(p2h,.975)), (quantile(p2h,.975)%%1)*60,
    median(post_base$kappa2), median(post_base$theta2)*100))
cat("\nCovariate effects:\n")
for (cov in cov_cols) {
  b   <- post_cov[[paste0("b_mu1_", cov)]]
  sig <- if (quantile(b,.025)>0 | quantile(b,.975)<0) " ***" else ""
  cat(sprintf("  %-12s  β=%+.2f [%+.2f, %+.2f]%s\n",
              cov, median(b), quantile(b,.025), quantile(b,.975), sig))
}
cat("\nLOO:\n")
print(loo_compare(loo_base, loo_cov, loo_site))
cat("\nAvailable objects:\n")
cat("  fit_base, fit_cov, fit_site\n")
cat("  df_model, df_clean, df_binned, df_summary\n")
cat("  post_base, post_cov, post_site_draws\n")
cat("  site_peaks, site_conc, site_summary_plot\n")
cat("  density_df, night_shading, cov_means, cov_sds\n")
cat("  loo_base, loo_cov, loo_site\n")
cat("\nRun 05_figures.R section by section. Use gc() between sections.\n")
cat("====================================\n")
