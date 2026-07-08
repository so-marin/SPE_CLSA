# SPE_CLSA

Last Updated: July 8th 2026

## Overview

This repository contains the data processing and statistical analysis pipeline accompanying Marinou et al. (2026), *Does the serial position effect identify those at risk for dementia? Findings from the Canadian Longitudinal Study on Aging.*

The repository provides reproducible workflows for both cross-sectional and longitudinal analyses of serial position effect (SPE) measures in the Canadian Longitudinal Study on Aging (CLSA). Shared utility functions are used across both workflows for data cleaning, visualization, model diagnostics, statistical modelling, and reporting.

---

## Data Requirements

**Important:** This repository does **not** include code for preparing the source CLSA datasets.

All scripts assume that the required CLSA data files have already been merged into a single analysis dataset. Specifically, the merged dataset should include:

- CLSA alphanumeric data from Baseline, Follow-up 1 (FUP1), and Follow-up 2 (FUP2)
- CLSA raw cognitive assessment data from Follow-up 1 (FUP1) and Follow-up 2 (FUP2)

Because access to CLSA data is restricted, neither the data nor the data-merging procedures are included in this repository.

---

## Repository Structure

```text
.
├── CrossSectional_DataCleaning.R      # Data cleaning for cross-sectional analyses
├── CrossSectional_Analysis.R          # Cross-sectional statistical analyses
├── Longitudinal_DataCleaning.R        # Data cleaning for longitudinal analyses
├── Longitudinal_Analysis.R            # Longitudinal statistical analyses
├── analysis_functions.R               # Shared helper functions
├── renv/                              # renv project files
├── renv.lock                          # Package dependency lockfile
├── requirements.txt                   # R version requirements
└── README.md
```

---

## Analysis Workflow

The cross-sectional and longitudinal analyses follow the same general workflow. For each pipeline, the data cleaning script should be run before the corresponding analysis script.

The data cleaning scripts take the merged CLSA dataset as input and produce cleaned `.csv` files that are subsequently used by the analysis scripts.

### General workflow

1. Import the merged CLSA dataset.
2. Apply inclusion and exclusion criteria.
3. Derive variables of interest.
4. Perform data cleaning procedures (e.g., missing value handling and outlier screening).
5. Evaluate model assumptions and diagnostics.
6. Fit statistical models.
7. Generate formatted model outputs and summary tables.

### Script order

#### Cross-sectional analyses

```r
source("CrossSectional_DataCleaning.R")
source("CrossSectional_Analysis.R")
```

#### Longitudinal analyses

```r
source("Longitudinal_DataCleaning.R")
source("Longitudinal_Analysis.R")
```

---

## Functions

The file `analysis_functions.R` contains reusable functions used throughout both analysis pipelines.

### Data cleaning

Functions are provided to:

- replace study-specific missing value codes with `NA`
- detect multivariate outliers using Mahalanobis distance
- remove univariate outliers based on within-group standardized (z) scores
- perform group-wise mean imputation for missing values

### Data visualization

Functions generate diagnostic plots to assess variable distributions prior to modelling, including:

- histograms with density overlays
- Q–Q plots
- boxplots stratified by group

Plots are automatically exported to PDF.

### Model diagnostics

Diagnostic functions are available for both linear regression and mixed-effects models.

### Statistical modelling

Functions are available to:

- fit linear regression models for cross-sectional analyses
- fit linear mixed-effects models for longitudinal analyses
- estimate standardized and unstandardized model coefficients
- compute estimated marginal means
- apply Benjamini–Hochberg false discovery rate correction to p-values
- format model outputs into Microsoft Word tables using the `officer` and `flextable` packages.


## Reproducibility

This project uses **renv** for reproducible package management.

To recreate the project environment:

```r
install.packages("renv")
renv::restore()
```

Package versions are recorded in `renv.lock`.

---

## Running the Analyses

To reproduce the analyses:

1. Obtain access to the required CLSA datasets.
2. Merge the CLSA alphanumeric and raw cognitive data files.
3. Place the merged dataset(s) in the expected project directory.
4. Update file paths in the data cleaning scripts if necessary.
5. Run the appropriate data cleaning script followed by the corresponding analysis script.
