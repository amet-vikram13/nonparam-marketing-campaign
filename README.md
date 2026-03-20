# Non-Parametric Statistical Analysis of Fast Food Marketing Campaign Strategies

**Author:** Amet Vikram
**Advisor:** Dr. Tirthankar Dasgupta
**Program:** M.S. Statistics, Rutgers University

## Overview

This project evaluates three competing promotional strategies deployed across 137 fast food store locations using non-parametric statistical methods. The analysis determines whether significant differences exist in sales performance among the promotions and models sales trends over time using non-parametric curve fitting.

## Dataset

**File:** `WA_Marketing-Campaign.csv`
**Source:** Watson Analytics Marketing Campaign dataset

| Variable | Description |
|---|---|
| MarketID | Market identifier (1–10) |
| MarketSize | Small, Medium, or Large |
| LocationID | Unique store identifier |
| AgeOfStore | Store age in years |
| Treatment | Promotion type (1, 2, or 3) |
| Week | Observation week (1–4) |
| SalesInThousands | Weekly sales revenue (thousands $) |

**Size:** 548 observations, 137 locations, 10 markets, 4 weeks

## Methods

### Hypothesis Testing
- **Kruskal-Wallis H-test** — omnibus test for differences across three promotion groups
- **Wilcoxon-Mann-Whitney U tests** — pairwise post-hoc comparisons
- **Bonferroni & Holm corrections** — multiple testing adjustment
- **Median comparisons** — directional verification of differences

### Non-Parametric Curve Fitting
- **LOWESS** (Locally Weighted Scatterplot Smoothing) with smoothing parameter f = 0.5
- **Smooth Splines** with cross-validated smoothing parameter selection

### Model Comparison
- Goodness-of-fit metrics: RMSE, MAE, MAPE, R², AIC, BIC
- Leave-One-Out Cross-Validation (LOOCV)
- Prediction on new time points (weeks 1–6)

## Project Structure

```
nonparam_project/
├── WA_Marketing-Campaign.csv          # Dataset
├── marketing_campaign_analysis.r      # Main analysis: EDA, Kruskal-Wallis,
│                                      #   pairwise tests, curve fitting
├── model_comparison_and_prediction.r  # Model comparison, CV, predictions
├── utils.r                            # Generates all tables & figures for the report
├── report.tex                         # LaTeX report (auto-populates from utils.r output)
├── report_output/                     # Generated tables (.tex/.csv) and plots (.png)
├── nonparam_project.Rproj             # RStudio project file
└── README.md
```

## How to Run

### Prerequisites

Install required R packages:

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "BSDA", "gridExtra", "RColorBrewer"))
```

### Execution

1. **Run the main analysis** (exploratory analysis, hypothesis tests, curve fitting):
   ```bash
   Rscript marketing_campaign_analysis.r
   ```

2. **Run model comparison** (GOF metrics, cross-validation, predictions):
   ```bash
   Rscript model_comparison_and_prediction.r
   ```

3. **Generate report assets** (all tables and figures into `report_output/`):
   ```bash
   Rscript utils.r
   ```

4. **Compile the report:**
   ```bash
   pdflatex report.tex
   ```

### Output

Running `utils.r` generates the `report_output/` directory containing:

| File | Description |
|---|---|
| `tab_summary_stats.tex` | Summary statistics by promotion |
| `tab_kw_results.tex` | Kruskal-Wallis test results |
| `tab_pairwise.tex` | Pairwise comparison p-values |
| `tab_median_comp.tex` | Median difference comparisons |
| `tab_spline_params.tex` | Smooth spline parameters |
| `tab_model_comparison.tex` | LOWESS vs Spline GOF metrics |
| `tab_cv_results.tex` | Cross-validation results |
| `plot_boxplots.png` | Box plots by promotion |
| `plot_violins.png` | Violin plots by promotion |
| `plot_lowess.png` | LOWESS smoothing curves |
| `plot_splines.png` | Smooth spline curves |
| `plot_comparison.png` | LOWESS vs Spline overlay |
| `prediction_comparison.png` | Model predictions (weeks 1–6) |

All tables are also saved as `.csv` files for programmatic access.

## Key Findings

- The Kruskal-Wallis test detects statistically significant differences among the three promotions.
- Post-hoc pairwise tests (with Bonferroni/Holm corrections) identify which specific promotion pairs differ.
- Both LOWESS and smooth splines adequately capture weekly sales trends, with LOOCV guiding model selection.
