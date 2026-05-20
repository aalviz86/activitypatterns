# ============================================================
#  03_mixture_models.R
#  Tapir Diel Activity — Colombian Orinoquia
#
#  Fits three Bayesian two-component von Mises mixture models
#  and compares them via PSIS-LOO.
#
#  INPUT:  data/tapir_model_data_corine.csv
#  OUTPUT: models/rebuild_mixture_base.rds
#          models/rebuild_mixture_cov.rds
#          models/rebuild_mixture_site.rds
#          loo_cache.rds
#
#  MODELS:
#    fit_base  — intercept-only (no covariates, no site effects)
#    fit_cov   — 5 covariates on μ₁ only; μ₂ ~ 1 (prevents label switching)
#    fit_site  — covariates + site fixed effects on μ₁; μ₂ ~ 1
#
#  PRIORS:
#    μ₁ intercept: Normal(-1, 1)   → centers on ~04:00 h pre-dawn peak
#    μ₂ intercept: Normal(1.5, 1)  → centers on ~18:00 h evening peak
#    β (covariates): Normal(0, 0.5) → ≤ ±3.8 h shift per covariate SD
#    κ₁, κ₂: Gamma(2, 0.5)        → moderate concentration, heavy right tail
#
#  REQUIRES: brms, tidyverse, future
#
#  CRITICAL: mix_family must be defined BEFORE loading any saved model.
#            Run this script top-to-bottom; do not reorder sections.
# ============================================================

library(brms)
library(tidyverse)
library(future)

plan(multisession, workers = parallel::detectCores() - 2)

# ── Constants ─────────────────────────────────────────────────────────────────
WD <- "~/1. TTU/00. Danta/00. Occupancy/01. Activity Patterns_2"
setwd(WD)
dir.create("models", showWarnings = FALSE)

site_levels <- c("Arauquita", "Bita", "Cravo Norte",
                  "Cumaribo", "Puerto Gaitan", "Puerto Rondon")

cov_cols <- c("forest_d", "forest_g", "forest_a", "sec_veg", "crops")

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

# ── Load data ─────────────────────────────────────────────────────────────────
df_model <- read_csv("data/tapir_model_data_corine.csv",
                     show_col_types = FALSE) %>%
  mutate(Site = factor(fix_site(Site), levels = site_levels))

cat("Loaded:", nrow(df_model), "detections,",
    n_distinct(df_model$CT), "cameras,",
    n_distinct(df_model$Site), "sites\n")


# ════════════════════════════════════════════════════════════
#  MIXTURE FAMILY
#  MUST be defined before any brm() call or readRDS() of a
#  mixture model. order="mu" prevents label switching.
# ════════════════════════════════════════════════════════════

mix_family <- mixture(von_mises(), von_mises(), order = "mu")
cat("mix_family defined.\n")

cov_formula <- paste(cov_cols, collapse = " + ")


# ════════════════════════════════════════════════════════════
#  PRIORS
# ════════════════════════════════════════════════════════════

priors_base <- c(
  prior(normal(-1, 1),  class = "Intercept", dpar = "mu1"),
  prior(normal(1.5, 1), class = "Intercept", dpar = "mu2"),
  prior(gamma(2, 0.5),  class = "kappa1"),
  prior(gamma(2, 0.5),  class = "kappa2")
)

priors_cov <- c(
  prior(normal(-1, 1),  class = "Intercept", dpar = "mu1"),
  prior(normal(1.5, 1), class = "Intercept", dpar = "mu2"),
  prior(normal(0, 0.5), class = "b",         dpar = "mu1"),
  prior(gamma(2, 0.5),  class = "kappa1"),
  prior(gamma(2, 0.5),  class = "kappa2")
)

MCMC <- list(
  chains  = 8,
  cores   = 4,
  iter    = 16000,
  warmup  = 6000,
  control = list(adapt_delta = 0.99, max_treedepth = 20),
  seed    = 123
)


# ════════════════════════════════════════════════════════════
#  MODEL A — Intercept-only (base)
# ════════════════════════════════════════════════════════════

cat("\n--- Fitting base model ---\n")
fit_base <- do.call(brm, c(
  list(formula = bf(HourRadians ~ 1),
       family  = mix_family,
       data    = df_model,
       prior   = priors_base),
  MCMC
))
saveRDS(fit_base, "models/rebuild_mixture_base.rds")
cat("BASE saved. Max Rhat:", round(max(rhat(fit_base)), 4), "\n")
print(summary(fit_base))


# ════════════════════════════════════════════════════════════
#  MODEL B — Covariate model (covariates on μ₁ only)
#
#  Covariates restricted to μ₁ for two reasons:
#  1. Ecologically: pre-dawn timing is more likely shaped
#     by habitat quality than the diffuse evening bout.
#  2. Statistically: applying identical predictors to both
#     components causes label switching, as the sampler can
#     no longer distinguish which component is which —
#     constraining μ₂ to an intercept keeps them identifiable
#     (Stephens, 2000).
# ════════════════════════════════════════════════════════════

cat("\n--- Fitting covariate model ---\n")
fit_cov <- do.call(brm, c(
  list(formula = bf(
         as.formula(paste("HourRadians ~", cov_formula)),
         mu2 ~ 1
       ),
       family  = mix_family,
       data    = df_model,
       prior   = priors_cov),
  MCMC
))
saveRDS(fit_cov, "models/rebuild_mixture_cov.rds")
cat("COV saved. Max Rhat:", round(max(rhat(fit_cov)), 4), "\n")
print(summary(fit_cov))


# ════════════════════════════════════════════════════════════
#  MODEL C — Site + covariate model
#
#  Site treated as fixed effect (not random) because n=6 sites
#  is insufficient for reliable variance component estimation
#  (Gelman & Hill, 2007). Fixed contrasts provide direct,
#  interpretable offsets relative to Arauquita (reference level).
# ════════════════════════════════════════════════════════════

cat("\n--- Fitting site + covariate model ---\n")
fit_site <- do.call(brm, c(
  list(formula = bf(
         as.formula(paste("HourRadians ~", cov_formula, "+ Site")),
         mu2 ~ 1
       ),
       family  = mix_family,
       data    = df_model,
       prior   = priors_cov),
  MCMC
))
saveRDS(fit_site, "models/rebuild_mixture_site.rds")
cat("SITE saved. Max Rhat:", round(max(rhat(fit_site)), 4), "\n")
print(summary(fit_site))


# ════════════════════════════════════════════════════════════
#  LOO MODEL COMPARISON
# ════════════════════════════════════════════════════════════

cat("\nComputing LOO (~2 min)...\n")
loo_base <- loo(fit_base)
loo_cov  <- loo(fit_cov)
loo_site <- loo(fit_site)

saveRDS(list(loo_base=loo_base, loo_cov=loo_cov, loo_site=loo_site),
        "loo_cache.rds")
cat("LOO cached: loo_cache.rds\n")

cat("\n=== LOO COMPARISON ===\n")
print(loo_compare(loo_base, loo_cov, loo_site))
for (nm in c("base","cov","site")) {
  l <- get(paste0("loo_", nm))
  cat(sprintf("  %-5s  LOOIC=%.1f  SE=%.1f  p_loo=%.1f  k>0.7: %d\n",
              nm,
              l$estimates["looic","Estimate"],
              l$estimates["looic","SE"],
              l$estimates["p_loo","Estimate"],
              sum(l$diagnostics$pareto_k > 0.7)))
}


# ════════════════════════════════════════════════════════════
#  KEY RESULTS SUMMARY
# ════════════════════════════════════════════════════════════

post_base <- as_draws_df(fit_base)
post_cov  <- as_draws_df(fit_cov)
p1h <- rad_to_hour(post_base$b_mu1_Intercept)
p2h <- rad_to_hour(post_base$b_mu2_Intercept)

cat("\n=== KEY RESULTS ===\n")
cat(sprintf("Pre-dawn: %02.0f:%02.0f h  [%02.0f:%02.0f–%02.0f:%02.0f]  kappa=%.2f  wt=%.0f%%\n",
    floor(median(p1h)), (median(p1h)%%1)*60,
    floor(quantile(p1h,.025)), (quantile(p1h,.025)%%1)*60,
    floor(quantile(p1h,.975)), (quantile(p1h,.975)%%1)*60,
    median(post_base$kappa1), median(post_base$theta1)*100))
cat(sprintf("Evening:  %02.0f:%02.0f h  [%02.0f:%02.0f–%02.0f:%02.0f]  kappa=%.2f  wt=%.0f%%\n",
    floor(median(p2h)), (median(p2h)%%1)*60,
    floor(quantile(p2h,.025)), (quantile(p2h,.025)%%1)*60,
    floor(quantile(p2h,.975)), (quantile(p2h,.975)%%1)*60,
    median(post_base$kappa2), median(post_base$theta2)*100))

cat("\nCovariate effects on μ₁ (pre-dawn):\n")
for (cov in cov_cols) {
  b   <- post_cov[[paste0("b_mu1_", cov)]]
  sig <- if (quantile(b,.025)>0 | quantile(b,.975)<0) " *** EXCLUDES ZERO" else ""
  cat(sprintf("  %-12s  β=%+.2f  [%+.2f, %+.2f]%s\n",
              cov, median(b), quantile(b,.025), quantile(b,.975), sig))
}
cat("Proceed to 04_reload_session.R\n")
