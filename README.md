**Tapir Activity Modeling**

This repository contains scripts, data, and outputs related to the Bayesian circular mixed-effects modeling of lowland tapir diel activity patterns in the Orinoquia region. Analyses are conducted in R using the brms, circular, and ggplot2 packages, with a focus on environmental and spatial covariates.

**Repository Structure**
.
├── data/            # Raw and processed datasets

├── scripts/         # Analysis scripts (R scripts for modeling, visualization, and statistics)

├── results/         # Model outputs, plots, and tables

├── README.md        # Project description and instructions

**Description**
The project aims to:
* Quantify diel activity patterns of lowland tapirs.
* Explore environmental and anthropogenic drivers of activity variation.
* Evaluate behavioral plasticity using Bayesian circular mixed-effects models.
* Provide site-specific conservation insights.

**Requirements**
* R (≥ 4.0.0)
* R packages:
  - brms
  - circular
  - ggplot2
  - dplyr
  - tidyr
  - readr
  - loo

**Install all packages using:**
install.packages(c("brms", "circular", "ggplot2", "dplyr", "tidyr", "readr", "loo"))

**Outputs**
Conditional effect plots for covariates.
Site-specific diel activity curves.
Model comparison tables (LOOIC, Bayesian R²).
Pairwise Watson’s U² results.

**License**
This repository is licensed under the MIT **License CC BY 4.0.**
