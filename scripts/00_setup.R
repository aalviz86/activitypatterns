# 00_setup.R
# Packages, options, helpers (run first)

packages <- c(
  "tidyverse","brms","cmdstanr","future","circular","tidyr","purrr","stringr",
  "patchwork","lubridate","suncalc","ggridges","tidybayes","ggrepel","ggpubr",
  "stringi","readxl","scales","grid","gridExtra","cowplot","ggcorrplot","overlap","here"
)
invisible(lapply(setdiff(packages, rownames(installed.packages())), install.packages))
invisible(lapply(packages, library, character.only = TRUE))

# Parallel
plan(multisession, workers = max(1, parallel::detectCores() - 1))

# CmdStan (only if you actually use cmdstanr backend)
# set_cmdstan_path()   # uncomment if needed

# Paths
DATA   <- here::here("data")
PROC   <- here::here("data","processed")
FIGS   <- here::here("results","figures")
TABS   <- here::here("results","tables")
MODS   <- here::here("results","models")
dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)
dir.create(TABS, recursive = TRUE, showWarnings = FALSE)
dir.create(MODS, recursive = TRUE, showWarnings = FALSE)

# Plot theme
theme_set(theme_classic())
