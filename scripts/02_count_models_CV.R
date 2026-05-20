# ============================================================
#  02_count_models_CV.R
#  Tapir Diel Activity — Colombian Orinoquia
#
#  Fits three count-based hierarchical models to binned
#  hourly detection counts and evaluates predictive
#  performance using repeated site-stratified 5-fold CV.
#
#  INPUT:  data/tapir_model_data_corine.csv
#  OUTPUT: cv_results_corrected_dataset.csv
#          cv_summary_corrected_dataset.csv
#
#  MODELS:
#    GAMM  — negative binomial, cyclic smoother (mgcv)
#    GLMM  — negative binomial, harmonic predictors (lme4)
#    LMM   — Gaussian on log-transformed counts (lme4)
#
#  REQUIRES: mgcv, lme4, tidyverse
# ============================================================

library(mgcv)
library(lme4)
library(tidyverse)

# ── Constants ─────────────────────────────────────────────────────────────────
WD <- "~/1. TTU/00. Danta/00. Occupancy/01. Activity Patterns_2"
setwd(WD)

site_levels <- c("Arauquita", "Bita", "Cravo Norte",
                  "Cumaribo", "Puerto Gaitan", "Puerto Rondon")

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

# ── Load modelling dataset ────────────────────────────────────────────────────
df_model <- read_csv("data/tapir_model_data_corine.csv",
                     show_col_types = FALSE) %>%
  mutate(Site = factor(fix_site(Site), levels = site_levels))

cat("Loaded df_model:", nrow(df_model), "detections\n")

# ── Build binned hourly count dataset ─────────────────────────────────────────
# CT_id is a numeric identifier required by mgcv (avoids special character issues)
df_binned <- df_model %>%
  mutate(HourBin = floor(Hour)) %>%
  count(Site, CT, HourBin, name = "count") %>%
  complete(nesting(Site, CT), HourBin = 0:23, fill = list(count = 0)) %>%
  mutate(
    CT_id     = as.integer(factor(CT)),
    cos_hour  = cos(2 * pi * HourBin / 24),
    sin_hour  = sin(2 * pi * HourBin / 24),
    log_count = log(count + 0.5)
  )

cat("Binned dataset rows:", nrow(df_binned),
    " | Cameras:", n_distinct(df_binned$CT), "\n")


# ════════════════════════════════════════════════════════════
#  FIT FULL MODELS (for AIC reporting and overall curve)
# ════════════════════════════════════════════════════════════

cat("\nFitting GAMM (full dataset)...\n")
fit_gamm <- gam(
  count ~ s(HourBin, bs = "cc", k = 12) + s(CT_id, bs = "re"),
  data   = df_binned,
  family = nb(),
  method = "REML"
)
cat("GAMM AIC:", round(AIC(fit_gamm), 1),
    "| Deviance explained:", round(summary(fit_gamm)$dev.expl * 100, 1), "%\n")

cat("\nFitting GLMM (full dataset)...\n")
fit_glmm <- glmer.nb(
  count ~ cos_hour + sin_hour + (1 | CT_id),
  data = df_binned
)
cat("GLMM AIC:", round(AIC(fit_glmm), 1), "\n")

cat("\nFitting LMM (full dataset)...\n")
fit_lmm <- lmer(
  log_count ~ cos_hour + sin_hour + (1 | CT_id),
  data = df_binned,
  REML = FALSE
)
cat("LMM AIC:", round(AIC(fit_lmm), 1), "\n")


# ════════════════════════════════════════════════════════════
#  CROSS-VALIDATION
#  Site-stratified, camera-blocked 5-fold CV (5 repetitions)
#  Metric: mean log predictive density
# ════════════════════════════════════════════════════════════

cat("\nRunning cross-validation (5 folds × 5 reps — takes ~10 min)...\n")
set.seed(42)
n_folds <- 5
n_reps  <- 5

cv_results <- map_dfr(seq_len(n_reps), function(rep) {

  fold_ids <- df_binned %>%
    distinct(CT_id, CT, Site) %>%
    group_by(Site) %>%
    mutate(fold = sample(rep(1:n_folds, length.out = n()), n())) %>%
    ungroup() %>%
    select(CT_id, CT, fold)

  map_dfr(seq_len(n_folds), function(fold_k) {
    test_ids  <- fold_ids$CT_id[fold_ids$fold == fold_k]
    train_ids <- fold_ids$CT_id[fold_ids$fold != fold_k]
    train <- df_binned %>% filter(CT_id %in% train_ids)
    test  <- df_binned %>% filter(CT_id %in% test_ids)

    # GAMM: exclude camera random effect for new-camera prediction
    gamm_score <- tryCatch({
      m     <- gam(count ~ s(HourBin, bs="cc", k=12) + s(CT_id, bs="re"),
                   data=train, family=nb(), method="REML")
      preds <- predict(m, newdata=test, type="response",
                       exclude="s(CT_id)", newdata.guaranteed=TRUE)
      theta <- m$family$getTheta(TRUE)
      mean(dnbinom(test$count, mu=pmax(preds,1e-6), size=theta, log=TRUE))
    }, error=function(e) { cat("GAMM fold failed:", e$message, "\n"); NA_real_ })

    # GLMM: predict without random effects (re.form=NA)
    glmm_score <- tryCatch({
      m     <- glmer.nb(count ~ cos_hour + sin_hour + (1|CT_id), data=train)
      preds <- predict(m, newdata=test, type="response", re.form=NA)
      theta <- getME(m, "glmer.nb.theta")
      mean(dnbinom(test$count, mu=pmax(preds,1e-6), size=theta, log=TRUE))
    }, error=function(e) { cat("GLMM fold failed:", e$message, "\n"); NA_real_ })

    # LMM: predict without random effects
    lmm_score <- tryCatch({
      m       <- lmer(log_count ~ cos_hour + sin_hour + (1|CT_id),
                      data=train, REML=FALSE)
      preds   <- predict(m, newdata=test, re.form=NA)
      sigma_e <- sigma(m)
      mean(dnorm(test$log_count, mean=preds, sd=sigma_e, log=TRUE))
    }, error=function(e) { cat("LMM fold failed:", e$message, "\n"); NA_real_ })

    tibble(rep=rep, fold=fold_k,
           GAMM=gamm_score, GLMM=glmm_score, LMM=lmm_score)
  })
})

cv_summary <- cv_results %>%
  pivot_longer(c(GAMM, GLMM, LMM), names_to="model", values_to="log_score") %>%
  group_by(model) %>%
  summarise(
    mean_log_score = round(mean(log_score, na.rm=TRUE), 3),
    sd_log_score   = round(sd(log_score,   na.rm=TRUE), 3),
    n_valid_folds  = sum(!is.na(log_score)),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_log_score))

cat("\n=== CROSS-VALIDATION RESULTS ===\n")
print(cv_summary)

write_csv(cv_results, "cv_results_corrected_dataset.csv")
write_csv(cv_summary, "cv_summary_corrected_dataset.csv")
cat("Saved: cv_results_corrected_dataset.csv\n")
cat("Saved: cv_summary_corrected_dataset.csv\n")
cat("Proceed to 03_mixture_models.R\n")
