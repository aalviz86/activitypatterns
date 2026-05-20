# ============================================================
#  05_figures.R
#  Tapir Diel Activity — Colombian Orinoquia
#
#  Produces all main-text and supplementary figures.
#  Run 04_reload_session.R first, then this script
#  ONE SECTION AT A TIME with gc() between sections.
#
#  FIGURES PRODUCED:
#   Fig 2a  tapir_gamm_activity_curve.png
#   Fig 2b  tapir_overall_polar_clean.png
#   Fig 3   conditional_effects_mu1_covariates.png
#   Fig 4   tapir_site_gamm_curves.png
#   Fig 5   concentration_vs_covariates_Fig5.png
#   Fig S1  watson_heatmap.png
#   Fig S2  diel_overlap_matrix_heatmap.png
#   Fig S3  distribution_activity_times_by_site.png
#   Fig S4  Site_peak_and_concentration.png
#   Fig S5  tapir_mixture_overall_activity_linear.png
#
#  REQUIRES: 04_reload_session.R to have been run first.
#            All objects from that script must be in memory.
# ============================================================

library(tidyverse)
library(brms)
library(circular)
library(ggridges)
library(ggrepel)
library(suncalc)
library(lubridate)
library(scales)
library(cowplot)
library(overlap)
library(mgcv)

# ── Verify required objects exist ─────────────────────────────────────────────
required_objects <- c("df_model","df_summary","df_binned","fit_base",
                       "fit_cov","fit_site","post_base","post_cov",
                       "post_site_draws","density_df","night_shading",
                       "site_conc","cov_means","cov_sds","rad_to_hour",
                       "back_transform","site_levels","labels_map","shapes_map")
missing_obj <- required_objects[!sapply(required_objects, exists)]
if (length(missing_obj) > 0)
  stop(paste("Run 04_reload_session.R first. Missing:",
             paste(missing_obj, collapse=", ")))

cat("All required objects found. Starting figures.\n\n")


# ════════════════════════════════════════════════════════════
#  FIG 2a — OVERALL GAMM ACTIVITY CURVE
# ════════════════════════════════════════════════════════════

cat("Fig 2a: GAMM activity curve...\n")
fit_gamm_full <- gam(
  count ~ s(HourBin, bs="cc", k=12) + s(CT_id, bs="re"),
  data=df_binned, family=nb(), method="REML"
)
pred_grid <- tibble(HourBin=seq(0,24,length.out=200), CT_id=1L)
pred_out  <- predict(fit_gamm_full, newdata=pred_grid, type="link",
                     se.fit=TRUE, exclude="s(CT_id)",
                     newdata.guaranteed=TRUE)
pred_df <- tibble(
  hour = pred_grid$HourBin,
  fit  = exp(pred_out$fit),
  lo   = exp(pred_out$fit - 1.96*pred_out$se.fit),
  hi   = exp(pred_out$fit + 1.96*pred_out$se.fit)
)

p_gamm <- ggplot(pred_df, aes(x=hour)) +
  geom_rect(data=tibble(xmin=c(0,17), xmax=c(7,24), ymin=0, ymax=Inf),
            inherit.aes=FALSE,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="gray85", alpha=0.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi), fill="gray60", alpha=0.4) +
  geom_line(aes(y=fit), color="black", linewidth=1.1) +
  scale_x_continuous(breaks=seq(0,24,4), labels=c("0","4","8","12","16","20","24"),
                     limits=c(0,24), expand=c(0,0)) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  labs(x="Hour of day", y="Activity rate (events per camera-day)") +
  theme_classic(base_size=14) +
  theme(axis.title=element_text(face="bold"))

ggsave("tapir_gamm_activity_curve.png", p_gamm, width=8, height=4, dpi=300)
cat("Saved: tapir_gamm_activity_curve.png\n"); gc()


# ════════════════════════════════════════════════════════════
#  FIG 2b — OVERALL POLAR ROSE
# ════════════════════════════════════════════════════════════

cat("Fig 2b: Polar rose...\n")
global_summary <- df_model %>%
  mutate(HourBin=floor(Hour)) %>% count(HourBin) %>%
  mutate(prop=n/sum(n))

p_polar <- ggplot(global_summary, aes(x=HourBin, y=prop)) +
  geom_rect(data=tibble(xmin=c(0,17.5), xmax=c(6.5,24)),
            inherit.aes=FALSE,
            aes(xmin=xmin, xmax=xmax, ymin=0, ymax=0.08),
            fill="gray80", alpha=.3) +
  geom_col(fill="black", width=1) +
  coord_polar(start=0, direction=1) +
  scale_x_continuous(limits=c(0,24), breaks=seq(0,24,4),
                     labels=sprintf("%02d:00", seq(0,24,4)), expand=c(0,0)) +
  scale_y_continuous(labels=percent_format(accuracy=1),
                     expand=c(0,0), limits=c(0,.1)) +
  labs(x=NULL, y=NULL) +
  theme_classic(base_size=14) +
  theme(axis.line=element_blank(),
        panel.grid.major=element_line(color="gray80"),
        panel.background=element_rect(fill="white", color=NA),
        plot.background=element_rect(fill="white", color=NA))

ggsave("tapir_overall_polar_clean.png", p_polar, width=6, height=6, dpi=400)
cat("Saved: tapir_overall_polar_clean.png\n"); gc()


# ════════════════════════════════════════════════════════════
#  FIG 3 — CONDITIONAL EFFECTS ON PRE-DAWN PEAK
# ════════════════════════════════════════════════════════════

cat("Fig 3: Conditional effects...\n")
set.seed(42)
post_thin <- post_cov[sample(nrow(post_cov), 1000), ]
plot_list <- list()

for (cov in cov_cols) {
  n_nonzero <- sum(df_model[[cov]] > min(df_model[[cov]]))
  z_lo <- if (n_nonzero < 20) min(df_model[[cov]]) else quantile(df_model[[cov]], .05)
  z_hi <- if (n_nonzero < 20) max(df_model[[cov]]) else quantile(df_model[[cov]], .95)
  z_seq  <- seq(z_lo, z_hi, length.out=50)
  x_orig <- back_transform(z_seq, cov)

  pred_df <- map_dfr(seq_along(z_seq), function(j) {
    mu1_j <- post_thin$b_mu1_Intercept +
              post_thin[[paste0("b_mu1_",cov)]] * z_seq[j]
    h <- pmin(pmax(rad_to_hour(mu1_j), 0), 12)
    tibble(x_orig=x_orig[j],
           Hour=median(h), Lo=quantile(h,.025), Hi=quantile(h,.975))
  })
  raw_df <- df_model %>%
    mutate(x_orig=back_transform(.data[[cov]],cov),
           Hour=rad_to_hour(HourRadians)) %>%
    filter(Hour >= 0, Hour <= 8)

  p <- ggplot(pred_df, aes(x=x_orig, y=Hour)) +
    geom_ribbon(aes(ymin=Lo, ymax=Hi), fill="gray80", alpha=0.5) +
    geom_line(linewidth=1.1, color="black") +
    geom_point(data=raw_df, aes(x=x_orig, y=Hour),
               inherit.aes=FALSE, color="black", alpha=0.35, size=1.5) +
    { if (n_nonzero < 20)
        geom_rug(data=raw_df, aes(x=x_orig), inherit.aes=FALSE,
                 color="black", alpha=0.4, sides="b",
                 length=unit(0.03,"npc"))
      else list() } +
    labs(x=labels_map[[cov]], y=NULL) +
    scale_y_continuous(limits=c(0,12), breaks=c(0,3,6,9,12),
                       labels=c("00:00","03:00","06:00","09:00","12:00"),
                       minor_breaks=NULL,
                       expand=expansion(mult=c(0,.02))) +
    coord_cartesian(xlim=range(x_orig)) +
    theme_classic(base_size=14) +
    theme(axis.text=element_text(color="black"),
          axis.title.x=element_text(face="bold"))
  plot_list[[cov]] <- p
}

final_cov <- plot_grid(
  ggdraw() + draw_label("Predicted Peak Activity Time (h)",
                        angle=90, fontface="bold", size=14),
  plot_grid(plotlist=plot_list, ncol=2, align="hv"),
  ncol=2, rel_widths=c(0.07, 1)
)
ggsave("conditional_effects_mu1_covariates.png", final_cov,
       width=10, height=10, dpi=300)
cat("Saved: conditional_effects_mu1_covariates.png\n")
rm(post_thin); gc()


# ════════════════════════════════════════════════════════════
#  FIG 4 — SITE-SPECIFIC GAMM CURVES
# ════════════════════════════════════════════════════════════

cat("Fig 4: Site GAMM curves...\n")
pred_site_gamm <- map_dfr(site_levels, function(s) {
  df_site <- df_model %>% filter(Site==s) %>%
    mutate(HourBin=floor(Hour)) %>%
    count(HourBin, name="count") %>%
    complete(HourBin=0:23, fill=list(count=0))
  tryCatch({
    m <- gam(count ~ s(HourBin, bs="cc", k=10),
             data=df_site, family=nb(), method="REML")
    grid  <- tibble(HourBin=seq(0,24,length.out=200))
    preds <- predict(m, newdata=grid, type="link", se.fit=TRUE)
    tibble(Site=s, hour=grid$HourBin,
           fit=exp(preds$fit),
           lo=exp(preds$fit-1.96*preds$se.fit),
           hi=exp(preds$fit+1.96*preds$se.fit))
  }, error=function(e) NULL)
}) %>%
  group_by(Site) %>%
  mutate(total=sum(fit)*(24/200), fit=fit/total, lo=lo/total, hi=hi/total) %>%
  ungroup() %>%
  mutate(Site=factor(Site, levels=site_levels))

p_site_gamm <- ggplot(pred_site_gamm, aes(x=hour)) +
  geom_rect(data=night_shading, inherit.aes=FALSE,
            aes(xmin=xmin, xmax=xmax, ymin=0, ymax=Inf, group=Site),
            fill="gray85", alpha=0.5) +
  geom_ribbon(aes(ymin=lo, ymax=hi), fill="gray70", alpha=0.4) +
  geom_line(aes(y=fit), color="black", linewidth=1.0) +
  facet_wrap(~Site, ncol=3) +
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24),
                     labels=c("0","4","8","12","16","20","24"),
                     limits=c(0,24), expand=c(0,0)) +
  scale_y_continuous(labels=percent_format(accuracy=1),
                     limits=c(0,NA), expand=expansion(mult=c(0,.08))) +
  labs(x="Hour of day", y="Relative activity probability") +
  theme_classic(base_size=13) +
  theme(strip.text=element_text(face="bold"), strip.background=element_blank(),
        panel.spacing=unit(0.8,"lines"), axis.title=element_text(face="bold"))

ggsave("tapir_site_gamm_curves.png", p_site_gamm, width=10, height=6, dpi=300)
cat("Saved: tapir_site_gamm_curves.png\n"); gc()


# ════════════════════════════════════════════════════════════
#  FIG 5 — CONCENTRATION VS COVARIATES
# ════════════════════════════════════════════════════════════

cat("Fig 5: Concentration vs covariates...\n")
labels_fig5 <- c(
  crops    = "Cropland (ha)",
  forest_a = "Open Forest (ha)",
  forest_d = "Dense Forest (ha)",
  forest_g = "Gallery and Riparian Forest (ha)",
  sec_veg  = "Secondary Vegetation (ha)"
)

site_covs_orig <- df_model %>%
  group_by(Site) %>%
  summarise(across(all_of(cov_cols), mean), .groups="drop") %>%
  mutate(across(all_of(cov_cols),
                ~. * cov_sds[[cur_column()]] + cov_means[[cur_column()]]))

cov_conc_site <- site_conc %>%
  left_join(site_covs_orig, by="Site") %>%
  pivot_longer(all_of(cov_cols), names_to="covariate", values_to="value") %>%
  left_join(site_conc %>% select(Site, rho_lower, rho_upper), by="Site") %>%
  filter(covariate %in% names(labels_fig5)) %>%
  mutate(covariate=factor(covariate, levels=names(labels_fig5),
                           labels=labels_fig5))

p_conc_fig5 <- ggplot(cov_conc_site,
                      aes(x=value, y=concentration_rho, shape=Site)) +
  geom_smooth(aes(group=1), method="lm", se=TRUE, linetype="dashed",
              color="black", fill="grey80", linewidth=0.8, show.legend=FALSE) +
  geom_point(size=3, color="black") +
  facet_wrap(~covariate, scales="free_x", ncol=3, strip.position="bottom") +
  scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0,1)) +
  scale_shape_manual(values=shapes_map,
                     guide=guide_legend(title=NULL, override.aes=list(size=4))) +
  labs(x=NULL, y="Activity Concentration (ρ)") +
  theme_classic(base_size=14) +
  theme(strip.placement="outside", strip.background=element_blank(),
        strip.text=element_text(face="bold"), panel.spacing=unit(.4,"lines"),
        legend.position=c(.83,.28),
        legend.background=element_rect(fill="white", color="grey80"),
        legend.text=element_text(size=10))

ggsave("concentration_vs_covariates_Fig5.png", p_conc_fig5,
       width=10, height=6, dpi=300)
cat("Saved: concentration_vs_covariates_Fig5.png\n"); gc()


# ════════════════════════════════════════════════════════════
#  FIG S1 — WATSON U² HEATMAP
# ════════════════════════════════════════════════════════════

cat("Fig S1: Watson heatmap...\n")
all_sites <- levels(df_model$Site)
set.seed(42)

watson_results <- map_dfr(combn(all_sites, 2, simplify=FALSE), function(pair) {
  s1 <- df_model$HourRadians[as.character(df_model$Site)==pair[1]]
  s2 <- df_model$HourRadians[as.character(df_model$Site)==pair[2]]
  tryCatch({
    test <- watson.two.test(circular(s1), circular(s2))
    tibble(site1=pair[1], site2=pair[2], U2=round(test$statistic,3))
  }, error=function(e) tibble(site1=pair[1], site2=pair[2], U2=NA_real_))
})
write_csv(watson_results, "Watson_two_sample_test_results.csv")

watson_plot_df <- expand.grid(site1=all_sites, site2=all_sites,
                               stringsAsFactors=FALSE) %>%
  left_join(watson_results, by=c("site1","site2")) %>%
  left_join(watson_results %>% rename(site1=site2, site2=site1),
            by=c("site1","site2"), suffix=c("",".rev")) %>%
  mutate(U2=coalesce(U2, U2.rev)) %>%
  select(site1,site2,U2) %>%
  filter(match(site1,all_sites) > match(site2,all_sites)) %>%
  mutate(site1=factor(site1,levels=rev(all_sites)),
         site2=factor(site2,levels=all_sites))

p_watson <- ggplot(watson_plot_df, aes(x=site2, y=site1, fill=U2)) +
  geom_tile(color="white", linewidth=.3) +
  geom_text(aes(label=ifelse(is.na(U2),"?",sprintf("%.2f",U2))),
            color="white", size=4.5, fontface="bold") +
  scale_fill_gradient2(low="#f7fbff", mid="#6baed6", high="#08306b",
                       midpoint=.25, limits=c(0,.5), na.value="gray70",
                       name=expression("Watson U"^2)) +
  labs(x=NULL, y=NULL) +
  theme_minimal(base_size=14) +
  theme(axis.text.x=element_text(angle=45,hjust=1,size=12),
        axis.text.y=element_text(size=12),
        panel.grid=element_blank()) +
  coord_fixed()

ggsave("watson_heatmap.png", p_watson, width=7, height=6, dpi=300)
cat("Saved: watson_heatmap.png\n"); gc()


# ════════════════════════════════════════════════════════════
#  FIG S2 — OVERLAP HEATMAP
# ════════════════════════════════════════════════════════════

cat("Fig S2: Overlap heatmap...\n")
sites_u <- levels(df_model$Site)

overlap_df <- expand.grid(Site1=sites_u, Site2=sites_u,
                           stringsAsFactors=FALSE) %>%
  filter(Site1 < Site2) %>%
  rowwise() %>%
  mutate(Delta = overlapEst(
    df_model$HourRadians[as.character(df_model$Site)==Site1] + pi,
    df_model$HourRadians[as.character(df_model$Site)==Site2] + pi,
    type="Dhat4"
  )) %>% ungroup()
write_csv(overlap_df, "pairwise_overlap_coefficients.csv")

overlap_plot_df <- expand.grid(Site1=sites_u, Site2=sites_u,
                                stringsAsFactors=FALSE) %>%
  left_join(overlap_df, by=c("Site1","Site2")) %>%
  left_join(overlap_df %>% rename(Site1=Site2,Site2=Site1),
            by=c("Site1","Site2"), suffix=c("",".rev")) %>%
  mutate(Delta=coalesce(Delta,Delta.rev)) %>%
  select(Site1,Site2,Delta) %>%
  filter(match(Site1,sites_u) > match(Site2,sites_u)) %>%
  mutate(Delta=round(Delta,2),
         Site1=factor(Site1,levels=rev(sites_u)),
         Site2=factor(Site2,levels=sites_u))

p_overlap <- ggplot(overlap_plot_df, aes(x=Site2, y=Site1, fill=Delta)) +
  geom_tile(color="white", linewidth=.3) +
  geom_text(aes(label=sprintf("%.2f",Delta)), size=4.5) +
  scale_fill_gradient2(low="#f7fbff", mid="#6baed6", high="#3182bd",
                       midpoint=.7, name="Overlap (Δ)") +
  labs(x=NULL, y=NULL) +
  theme_minimal(base_size=14) +
  theme(axis.text.x=element_text(angle=45,hjust=1,size=12),
        axis.text.y=element_text(size=12), panel.grid=element_blank()) +
  coord_fixed()

ggsave("diel_overlap_matrix_heatmap.png", p_overlap, width=7, height=6, dpi=300)
cat("Saved: diel_overlap_matrix_heatmap.png\n"); gc()


# ════════════════════════════════════════════════════════════
#  FIG S3 — RIDGELINE PLOT BY SITE
# ════════════════════════════════════════════════════════════

cat("Fig S3: Ridgeline plot...\n")
samples_df <- df_model %>%
  mutate(hour=rad_to_hour(HourRadians),
         Site=factor(as.character(Site), levels=rev(site_levels)))

p_ridge <- ggplot(samples_df, aes(x=hour, y=Site,
                                   fill=Site)) +
  geom_density_ridges(scale=1.3, alpha=.6, color="black", linewidth=.3) +
  scale_x_continuous(breaks=c(0,6,12,18,24),
                     labels=c("Midnight","6 AM","Noon","6 PM","Midnight")) +
  scale_fill_manual(values=colorRampPalette(c("#b3c6ff","#002266"))(6)) +
  labs(x="Hour of Day", y="Site") +
  theme_classic(base_size=14) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position="none")

ggsave("distribution_activity_times_by_site.png", p_ridge,
       dpi=300, width=10, height=6)
cat("Saved: distribution_activity_times_by_site.png\n"); gc()


# ════════════════════════════════════════════════════════════
#  FIG S4 — SITE PEAKS + CONCENTRATION
# ════════════════════════════════════════════════════════════

cat("Fig S4: Site peaks + concentration...\n")
site_obs_peak <- df_summary %>%
  group_by(Site) %>%
  summarise(
    mean_hour = {
      theta  <- (midHour/24)*2*pi; w <- prop
      mu_rad <- as.numeric(mean.circular(circular(theta,units="radians"),
                                          weights=w))
      ((mu_rad/(2*pi))*24 + 24) %% 24
    },
    concentration_rho = {
      theta <- (midHour/24)*2*pi; w <- prop
      sqrt(sum(w*cos(theta))^2 + sum(w*sin(theta))^2)/sum(w)
    },
    .groups="drop"
  ) %>%
  left_join(site_conc %>% select(Site,rho_lower,rho_upper), by="Site")

p_sites <- ggplot(site_obs_peak,
                  aes(x=mean_hour, y=concentration_rho, label=Site)) +
  geom_errorbar(aes(ymin=rho_lower,ymax=rho_upper),
                width=0.15, color="gray50", linewidth=0.8) +
  geom_point(size=4, color="#2171B5", fill="#2171B5", shape=21, stroke=1.1) +
  geom_text_repel(size=4.5, fontface="bold", color="black",
                  max.overlaps=10, box.padding=0.4) +
  scale_x_continuous(limits=c(0,24), breaks=c(0,4,8,12,16,20,24),
                     labels=c("00:00","04:00","08:00","12:00",
                               "16:00","20:00","24:00")) +
  scale_y_continuous(limits=c(0,.8), breaks=seq(0,.8,.2),
                     labels=percent_format(accuracy=1)) +
  labs(x="Mean Activity Peak Hour", y="Activity Concentration (ρ)") +
  theme_classic(base_size=14) +
  theme(axis.title=element_text(face="bold"), axis.text=element_text(size=12))

ggsave("Site_peak_and_concentration.png", p_sites, width=8, height=5, dpi=300)
cat("Saved: Site_peak_and_concentration.png\n"); gc()


# ════════════════════════════════════════════════════════════
#  FIG S5 — MIXTURE DENSITY CURVE (Supplementary)
# ════════════════════════════════════════════════════════════

cat("Fig S5: Mixture density curve...\n")
obs_hist <- df_model %>%
  mutate(hour_bin=floor(Hour)+0.5) %>% count(hour_bin) %>%
  mutate(prop_norm=n/sum(n)/max(n/sum(n)))

p_mixture <- ggplot() +
  annotate("rect",xmin=0, xmax=6, ymin=0,ymax=Inf,fill="gray90",alpha=.5) +
  annotate("rect",xmin=18,xmax=24,ymin=0,ymax=Inf,fill="gray90",alpha=.5) +
  geom_ribbon(data=density_df,aes(x=hour,ymin=lower,ymax=upper),
              fill="#6baed6",alpha=.3) +
  geom_line(data=density_df,aes(x=hour,y=mean),
            color="#08519c",linewidth=1.2) +
  geom_col(data=obs_hist,aes(x=hour_bin,y=prop_norm),
           width=.9,fill="black",alpha=.4) +
  scale_x_continuous(breaks=seq(0,24,4),
                     labels=c("00:00","04:00","08:00","12:00","16:00","20:00","24:00"),
                     limits=c(0,24),expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1.05),expand=expansion(mult=c(0,.02)),
                     labels=NULL) +
  labs(x="Hour of Day", y="Relative Activity") +
  theme_classic(base_size=14) +
  theme(axis.title=element_text(face="bold"),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())

ggsave("tapir_mixture_overall_activity_linear.png", p_mixture,
       width=10, height=5, dpi=300)
cat("Saved: tapir_mixture_overall_activity_linear.png\n"); gc()


cat("\n=== ALL FIGURES SAVED ===\n")
cat("Fig 2a  tapir_gamm_activity_curve.png\n")
cat("Fig 2b  tapir_overall_polar_clean.png\n")
cat("Fig 3   conditional_effects_mu1_covariates.png\n")
cat("Fig 4   tapir_site_gamm_curves.png\n")
cat("Fig 5   concentration_vs_covariates_Fig5.png\n")
cat("Fig S1  watson_heatmap.png\n")
cat("Fig S2  diel_overlap_matrix_heatmap.png\n")
cat("Fig S3  distribution_activity_times_by_site.png\n")
cat("Fig S4  Site_peak_and_concentration.png\n")
cat("Fig S5  tapir_mixture_overall_activity_linear.png\n")
