# Tapir Diel Activity Analysis

This repository processes camera-trap detections of lowland tapirs, fits Bayesian circular mixed-effects models, 
and produces diagnostic & visualization outputs.

## 📂 Folder structure
```text
├── data/               # raw & processed input files
│   └── processed_activity_data_2.xlsx
├── R/                  # modular R scripts
│   ├── 01_data.R
│   ├── 02_models.R
│   └── 03_plots.R
├── outputs/            # figures, CSVs, RDS model objects
├── renv.lock           # lockfile for package versions
├── run_all.R           # top‐level script that runs the full pipeline
└── README.md


### 2. Flesh out “Setup” with exact commands

Add the `install.packages("renv")` step and the git clone line:

```markdown
## 🚀 Setup

1. Clone this repo:
   ```bash
   git clone https://github.com/aalviz86/activitypatterns.git
   cd activitypatterns
