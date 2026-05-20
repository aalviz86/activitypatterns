# Rhythms in the Landscape: Bayesian Circular Models Reveal Spatial Structure in Tapir Activity Patterns

This repository contains all data, scripts, and outputs supporting the manuscript:

> *Rhythms in the Landscape: Bayesian Circular Models Reveal Spatial Structure in Tapir Activity Patterns*  
> Álviz et al. — submitted to *Journal of Animal Ecology* (2026)

All analyses are conducted in R using Bayesian two-component von Mises mixture models, with a focus on diel activity timing, temporal concentration, and landscape covariates across six sites in the Colombian Orinoquia.

---

## Repository Structure

```
activitypatterns/
│
├── README.md                  ← project overview and reproducibility guide
│
├── data/
│   ├── raw/                   ← original detection data (never modified)
│   └── processed/             ← filtered 317 independent detections (30-min threshold)
│
├── covariates/                ← landscape covariate extractions per camera (1-km buffer)
│
├── scripts/
│   ├── 01_data_preparation.R  ← independence filter, solar time conversion to radians
│   ├── 02_model_comparison.R  ← GAMM, GLMM, LMM cross-validation (count-based frameworks)
│   ├── 03_mixture_models.R    ← base, covariate, and site+covariate von Mises mixture models
│   ├── 04_model_diagnostics.R ← R-hat, ESS, trace plots, LOO/PSIS-LOO diagnostics
│   └── 05_figures.R           ← all manuscript figures (Figures 2–5 and supplementary)
│
└── outputs/
    ├── model_fits/            ← saved brms model objects (.rds)
    └── figures/               ← final figures as exported
```

---

## Description

This project applies a Bayesian two-component von Mises mixture modeling framework to camera-trap detection times of the lowland tapir (*Tapirus terrestris*) recorded at 111 camera stations across six sites in the Colombian Orinoquia (2018–2023). The workflow:

1. **Quantifies bimodal diel activity patterns** — estimating separate peak times (μ), temporal concentration (κ), and mixing weights (θ) for evening and pre-dawn activity components.
2. **Evaluates competing statistical frameworks** — comparing count-based hierarchical models (GAMM, GLMM, LMM) and a unimodal circular model against the mixture approach using cross-validation.
3. **Models landscape drivers of activity timing** — assessing the effects of dense forest, gallery/riparian forest, open forest, secondary vegetation, and cropland cover on pre-dawn peak timing across sites.
4. **Provides site-specific conservation insights** — quantifying site-level variation in temporal concentration (ρ) and activity timing across a gradient of habitat types and anthropogenic pressure.

---

## Requirements

- **R ≥ 4.5.0** (analyses conducted in R 4.5.0; earlier versions may work but are untested)
- **Stan** (required by brms; install via `install.packages("rstan")`)

### R packages

| Package | Version used | Purpose |
|---------|-------------|---------|
| brms | ≥ 2.21 | Bayesian mixture model fitting via Stan |
| circular | ≥ 0.5 | Circular statistics and Watson's U² tests |
| loo | ≥ 2.7 | PSIS-LOO model comparison |
| ggplot2 | ≥ 3.5 | Visualization |
| dplyr | ≥ 1.1 | Data manipulation |
| tidyr | ≥ 1.3 | Data reshaping |
| readr | ≥ 2.1 | Data import |
| sf | ≥ 1.0 | Spatial data and covariate extraction |
| terra | ≥ 1.7 | Raster processing for landscape covariates |

### Install all packages

```r
install.packages(c("brms", "circular", "loo", "ggplot2", 
                   "dplyr", "tidyr", "readr", "sf", "terra"))
```

> **Note:** Installing `brms` will prompt installation of `rstan`. Follow the [RStan Getting Started guide](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) if you encounter compilation issues.

---

## How to Reproduce

Run scripts in numerical order:

```r
source("scripts/01_data_preparation.R")
source("scripts/02_model_comparison.R")
source("scripts/03_mixture_models.R")
source("scripts/04_model_diagnostics.R")
source("scripts/05_figures.R")
```

> ⚠️ Script `03_mixture_models.R` is computationally intensive (8 chains × 16,000 iterations per model). Pre-fitted model objects are available in `outputs/model_fits/` as `.rds` files and can be loaded directly to skip refitting.

---

## Outputs

- Posterior parameter estimates (μ, κ, θ) for all three mixture model variants
- Conditional effect plots for five landscape covariates on pre-dawn peak timing (Figure 4)
- Site-specific diel activity curves (Figure 3)
- Model comparison table (LOOIC, ΔELPD, SE; Table in manuscript)
- Pairwise Watson's U² and temporal overlap coefficients (Δ̂) across sites (Figure S3)
- Session info for full reproducibility (`outputs/sessionInfo.txt`)

---

## Citation

If you use this code or data, please cite:

> Álviz Á, Salazar-Bravo J, van Gestel N, Stevens RD (2026). Rhythms in the Landscape: Bayesian Circular Models Reveal Spatial Structure in Tapir Activity Patterns. *Journal of Animal Ecology* (under review).

---

## License

This repository is licensed under **CC BY 4.0** — you are free to share and adapt the material for any purpose, provided appropriate credit is given.  
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

---

## Contact

For questions about the data or analyses, please open a GitHub Issue or contact the corresponding author via the journal submission.
