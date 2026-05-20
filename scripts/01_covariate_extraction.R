# ============================================================
#  01_covariate_extraction.R
#  Tapir Diel Activity — Colombian Orinoquia
#
#  Extracts five land cover covariates from the Colombian
#  Ecosistemas 2018 shapefile (Corine Land Cover; IDEAM 2018)
#  and builds the final modelling dataset.
#
#  INPUT:
#    - tapir_activity_clean.xlsx (working directory root)
#    - Ecosistemas 2018 shapefile (SHPDIR path below)
#
#  OUTPUT:
#    - covariates/camera_covariates_corine.csv
#    - covariates/camera_covariates_corine_final.csv
#    - covariates/cov_means.rds
#    - covariates/cov_sds.rds
#    - data/tapir_model_data_corine.csv
#
#  COVARIATES (all in ha, 1-km buffer):
#    forest_d  = Dense forest (Bosque denso; 5 sub-classes)
#    forest_g  = Gallery/riparian forest (Bosque de galería y ripario)
#    forest_a  = Open forest (Bosque abierto; 4 sub-classes)
#    sec_veg   = Secondary vegetation (Vegetación secundaria; 3 sub-classes)
#    crops     = Cropland (all Level 2.x agricultural classes; 14 sub-classes)
#
#  NOT INCLUDED:
#    savanna/pasture — dominant matrix, near-zero variance across buffers
#    d_streams      — OSM missing for 3/6 sites; would drop 106 detections
#
#  REQUIRES: sf, terra, tidyverse, stringi, readxl
# ============================================================

library(sf)
library(terra)
library(tidyverse)
library(stringi)

# ── Paths — adjust if needed ──────────────────────────────────────────────────
WD <- "~/1. TTU/00. Danta/00. Occupancy/01. Activity Patterns_2"

SHPDIR <- paste0(
  "C:/Users/aalvi/OneDrive - Texas Tech University/",
  "01. Cartografía/1. Proyectos/2. Info Base/",
  "2. Ecosistemas, coberturas y áreas protegidas/",
  "Ecosistemas 2018/shape coberturas 2018"
)

setwd(WD)
dir.create("covariates", showWarnings = FALSE)
dir.create("data",       showWarnings = FALSE)
cat("Working directory:", getwd(), "\n")

# ── Shared constants ──────────────────────────────────────────────────────────
site_levels <- c("Arauquita", "Bita", "Cravo Norte",
                  "Cumaribo", "Puerto Gaitan", "Puerto Rondon")

cov_cols <- c("forest_d", "forest_g", "forest_a", "sec_veg", "crops")

# Handles Windows/OneDrive encoding issues with accented site names
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


# ════════════════════════════════════════════════════════════
#  STEP 1 — Load Ecosistemas 2018 shapefile
# ════════════════════════════════════════════════════════════

cat("\nLoading Ecosistemas 2018 shapefile (1-2 min)...\n")

shp_file <- list.files(SHPDIR, pattern = "\\.shp$",
                        full.names = TRUE, recursive = TRUE)[1]

if (is.na(shp_file) || !file.exists(shp_file))
  stop(paste("No .shp file found in:", SHPDIR,
             "\nCheck the SHPDIR path at the top of this script."))

cat("Found:", basename(shp_file), "\n")
ecosistemas <- st_read(shp_file, quiet = FALSE)

cat("Features:", nrow(ecosistemas), "\n")
cat("CRS EPSG:", st_crs(ecosistemas)$epsg, "\n")

# The land cover column in cobertura_tierra_clc_2018.shp is 'leyenda'
lc_col          <- "leyenda"
classes_present <- sort(unique(ecosistemas[[lc_col]]))
cat("Unique land cover classes:", length(classes_present), "\n")


# ════════════════════════════════════════════════════════════
#  STEP 2 — Define exact Corine class mappings
#  Verified against the actual shapefile attribute table.
#  DO NOT change these unless you have confirmed the class
#  names in a different version of the shapefile.
# ════════════════════════════════════════════════════════════

# Dense forest: all sub-classes of 3.1.1 except Manglar and Palmares
FOREST_D_CLASSES <- classes_present[
  grepl("^3\\.1\\.1\\.", classes_present) &
  !grepl("Manglar|Palmares", classes_present, ignore.case = TRUE)
]
# Confirmed 5 classes:
# 3.1.1.1.1. Bosque denso alto de tierra firme
# 3.1.1.1.2. Bosque denso alto inundable
# 3.1.1.1.2.1. Bosque denso alto Inundable heterogéneo
# 3.1.1.2.1. Bosque denso bajo de tierra firme
# 3.1.1.2.2. Bosque denso bajo inundable

# Gallery and riparian forest: single Corine class
FOREST_G_CLASSES <- "3.1.4. Bosque de galería y ripario"
# Confirmed: 1 class

# Open forest: all 4 sub-classes of 3.1.2
FOREST_A_CLASSES <- classes_present[grepl("^3\\.1\\.2\\.", classes_present)]
# Confirmed 4 classes:
# 3.1.2.1.1. Bosque abierto alto de tierra firme
# 3.1.2.1.2. Bosque abierto alto inundable
# 3.1.2.2.1. Bosque abierto bajo de tierra firme
# 3.1.2.2.2. Bosque abierto bajo inundable

# Secondary vegetation: all 3 sub-classes of 3.2.3
# NOTE: excludes 3.1.3.2 (Bosque fragmentado con vegetación secundaria)
# which is a forest class, not secondary vegetation
SEC_VEG_CLASSES <- classes_present[grepl("^3\\.2\\.3\\.", classes_present)]
# Confirmed 3 classes:
# 3.2.3. Vegetación secundaria o en transición
# 3.2.3.1. Vegetación secundaria alta
# 3.2.3.2. Vegetación secundaria baja

# Cropland: all Level 2.x agricultural classes
# NOTE: excludes urban (1.x.x) — use grepl("^2\\.") to avoid this
CROPS_CLASSES <- classes_present[
  grepl("^2\\.", classes_present) &
  grepl("cultiv|mosaico de cultiv|agroforestal|confinado",
        classes_present, ignore.case = TRUE)
]
# Confirmed 14 classes (transitorios, permanentes, mosaicos, agroforestales)

cat("\n=== CLASS MAPPING VERIFICATION ===\n")
cat(sprintf("forest_d: %d classes (expected 5)\n",  length(FOREST_D_CLASSES)))
cat(sprintf("forest_g: %d classes (expected 1)\n",  length(FOREST_G_CLASSES)))
cat(sprintf("forest_a: %d classes (expected 4)\n",  length(FOREST_A_CLASSES)))
cat(sprintf("sec_veg:  %d classes (expected 3)\n",  length(SEC_VEG_CLASSES)))
cat(sprintf("crops:    %d classes (expected 14)\n", length(CROPS_CLASSES)))

stopifnot(
  length(FOREST_D_CLASSES) == 5,
  length(FOREST_G_CLASSES) == 1,
  length(FOREST_A_CLASSES) == 4,
  length(SEC_VEG_CLASSES)  == 3,
  length(CROPS_CLASSES)    == 14
)
cat("All class counts verified.\n")


# ════════════════════════════════════════════════════════════
#  STEP 3 — Load camera trap locations
# ════════════════════════════════════════════════════════════

cat("\nLoading camera trap data...\n")

df_clean <- readxl::read_excel("tapir_activity_clean.xlsx") %>%
  mutate(Site = factor(fix_site(Site), levels = site_levels))

cameras <- df_clean %>%
  select(CT, Site, Latitude, Longitude) %>%
  distinct(CT, .keep_all = TRUE) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(Site_clean = fix_site(as.character(Site)))

cat(sprintf("Cameras: %d across %d sites\n",
            nrow(cameras), n_distinct(cameras$Site)))
print(cameras %>% count(Site))

pts_wgs  <- st_as_sf(cameras, coords = c("Longitude","Latitude"), crs = 4326)
pts_proj <- st_transform(pts_wgs, 3116)  # Colombia metric CRS


# ════════════════════════════════════════════════════════════
#  STEP 4 — Subset shapefile and repair geometries
# ════════════════════════════════════════════════════════════

cat("\nReprojecting and subsetting Ecosistemas to study area...\n")

ecosistemas_proj <- st_transform(ecosistemas, 3116)
study_bbox       <- st_bbox(st_buffer(pts_proj, 5000))
eco_study        <- st_crop(ecosistemas_proj, study_bbox)
cat("Polygons in study area:", nrow(eco_study), "\n")

n_invalid <- sum(!st_is_valid(eco_study))
if (n_invalid > 0) {
  cat("Repairing", n_invalid, "invalid geometries...\n")
  eco_study <- st_make_valid(eco_study)
}

rm(ecosistemas, ecosistemas_proj); gc()
cat("National shapefile removed from memory.\n")


# ════════════════════════════════════════════════════════════
#  STEP 5 — Extract land cover area per camera (1-km buffer)
# ════════════════════════════════════════════════════════════

extract_lc_area <- function(i) {
  buf <- pts_proj[i, ] %>% st_buffer(1000)
  tryCatch({
    intersection <- st_intersection(eco_study, buf) %>%
      mutate(area_ha = as.numeric(st_area(geometry)) / 10000)
    if (nrow(intersection) == 0)
      return(tibble(CT=cameras$CT[i],
                    forest_d=0, forest_g=0, forest_a=0, sec_veg=0, crops=0))
    lc <- intersection[[lc_col]]
    tibble(
      CT       = cameras$CT[i],
      forest_d = sum(intersection$area_ha[lc %in% FOREST_D_CLASSES], na.rm=TRUE),
      forest_g = sum(intersection$area_ha[lc %in% FOREST_G_CLASSES], na.rm=TRUE),
      forest_a = sum(intersection$area_ha[lc %in% FOREST_A_CLASSES], na.rm=TRUE),
      sec_veg  = sum(intersection$area_ha[lc %in% SEC_VEG_CLASSES],  na.rm=TRUE),
      crops    = sum(intersection$area_ha[lc %in% CROPS_CLASSES],    na.rm=TRUE)
    )
  }, error = function(e) {
    warning(paste("Failed:", cameras$CT[i], "-", e$message))
    tibble(CT=cameras$CT[i], forest_d=NA, forest_g=NA,
           forest_a=NA, sec_veg=NA, crops=NA)
  })
}

cat("\nExtracting land cover (5-10 min)...\n")
lc_results <- map_dfr(seq_len(nrow(cameras)), function(i) {
  if (i %% 10 == 0 || i == 1)
    cat(sprintf("  [%d/%d] %-30s (%s)\n",
                i, nrow(cameras), cameras$CT[i],
                as.character(cameras$Site[i])))
  extract_lc_area(i)
})

cat("\nExtraction complete. NA counts:\n")
print(colSums(is.na(lc_results[, -1])))

cat("\n=== COVARIATE SUMMARY PER SITE ===\n")
print(
  lc_results %>%
    left_join(cameras %>%
                mutate(Site = fix_site(as.character(Site))) %>%
                select(CT, Site), by = "CT") %>%
    group_by(Site) %>%
    summarise(across(all_of(cov_cols), ~round(mean(., na.rm=TRUE), 1)),
              n = n(), .groups = "drop")
)

# Plausibility check: 1-km buffer ≈ 314 ha
n_implausible <- lc_results %>%
  mutate(total = forest_d + forest_g + forest_a + sec_veg + crops) %>%
  filter(total > 320) %>% nrow()
if (n_implausible > 0)
  warning(paste(n_implausible, "cameras have total LC > 320 ha — check extraction"))

write_csv(lc_results, "covariates/camera_covariates_corine.csv")
cat("Saved: covariates/camera_covariates_corine.csv\n")


# ════════════════════════════════════════════════════════════
#  STEP 6 — Pearson correlation check
# ════════════════════════════════════════════════════════════

cat("\n=== PEARSON CORRELATIONS ===\n")
cov_mat <- lc_results %>%
  select(all_of(cov_cols)) %>%
  filter(complete.cases(.)) %>%
  cor(use = "complete.obs")
print(round(cov_mat, 2))
cat(sprintf("Max |r|: %.2f\n", max(abs(cov_mat[upper.tri(cov_mat)]))))


# ════════════════════════════════════════════════════════════
#  STEP 7 — Build modelling dataset
# ════════════════════════════════════════════════════════════

# Join on BOTH CT and Site to avoid any cross-site CT name collisions
covariates_with_site <- cameras %>%
  mutate(Site = fix_site(as.character(Site))) %>%
  left_join(lc_results, by = "CT")

write_csv(covariates_with_site,
          "covariates/camera_covariates_corine_final.csv")

# Save scaling parameters for back-transformation in figures
cov_means <- covariates_with_site %>%
  summarise(across(all_of(cov_cols), ~mean(., na.rm=TRUE)))
cov_sds <- covariates_with_site %>%
  summarise(across(all_of(cov_cols), ~sd(., na.rm=TRUE)))

saveRDS(cov_means, "covariates/cov_means.rds")
saveRDS(cov_sds,   "covariates/cov_sds.rds")

df_model <- df_clean %>%
  mutate(Site = fix_site(as.character(Site))) %>%
  left_join(
    covariates_with_site %>% select(CT, Site, all_of(cov_cols)),
    by = c("CT", "Site")
  ) %>%
  filter(if_all(all_of(cov_cols), ~!is.na(.))) %>%
  mutate(across(all_of(cov_cols), ~as.numeric(scale(.)))) %>%
  mutate(HourRadians = (Hour / 24) * 2 * pi - pi) %>%
  mutate(Site = factor(Site, levels = site_levels)) %>%
  filter(!is.na(Site))

# Verify encoding
stopifnot(
  min(df_model$HourRadians, na.rm=TRUE) >= -pi,
  max(df_model$HourRadians, na.rm=TRUE) <=  pi,
  !any(is.na(df_model$HourRadians))
)
cat("Circular encoding verified: range =",
    round(range(df_model$HourRadians), 3), "\n")

cat("\n=== FINAL MODELLING DATASET ===\n")
print(df_model %>% count(Site))
cat("TOTAL:", nrow(df_model), "independent detections\n")
cat("Cameras:", n_distinct(df_model$CT), "\n")

# Bimodality check
cat("\n=== BIMODALITY CHECK ===\n")
df_model %>%
  mutate(h = floor(Hour)) %>% count(h) %>%
  mutate(bar = strrep("|", n)) %>%
  pwalk(function(h, n, bar)
    cat(sprintf("  %02d:00  %-35s (%d)\n", h, bar, n)))

write_csv(df_model, "data/tapir_model_data_corine.csv")
cat("\nSaved: data/tapir_model_data_corine.csv\n")
cat("Proceed to 02_count_models_CV.R\n")
