
## Overview

This script generates results for the study examining a diet-modifiable, longitudinal proteomic signature that captures a hyperglycemic brain aging pattern across multiple cohorts (GNHS, PPMI, ADNI).

---

## Requirements

### R version
R ≥ 4.1.0 recommended.

### Required packages

```r
install.packages(c(
  "openxlsx", "dplyr", "tidyverse", "reshape2",
  "car", "pROC", "plotROC",
  "ggplot2", "ggrepel", "ggpubr", "ggsignif", "ggridges",
  "ggExtra", "aplot", "cowplot", "scales", "pheatmap",
  "RColorBrewer", "colorspace", "MetBrewer", "ggsci"
))
```

---
## Script structure

The script is divided into self-contained sections, each prefixed with a `#' [section name]` header. Sections can be run independently provided the relevant input data objects are loaded first.

| Section | Description 
|---|---|
| Ridge plot (GNHS cohort) | Chronological age distribution by subtype 
| Boxplot (Cognition, GNHS) | Cognitive score by subtype 
| Brain age prediction scatter | Predicted vs chronological age + marginal density 
| Age-gap distribution | Ridge plot across disease groups 
| External validation — PPMI | Cognitive score, age gap, chronologic age by subtype 
| External validation — ADNI | Same panel for ADNI cohort 
| Polar chart | Radar/rose chart of mean ROIs ranks by cohort 
| ROI × subtype dot plot | Divergent atrophy ROIs (SuStain) 
| Atrophy trajectory | Loess curve of atrophy value by SuStain stage 
| Disease & brain subtypes | Forest plot: OR for disease by subtype 
| Traits & brain subtypes | Forest plot: beta coefficient for traits 
| Brain × Protein bar chart | Count of significantly associated proteins per ROI 
| Volcano plot  | Differential proteins between subtypes 
| GO enrichment | Bar chart of enriched terms colored by −log10(adj-P) 
| Marker gene (hippocampus) | Lollipop chart: logFC + significance
| Hippocampus–protein association | Bar chart of Spearman correlation by brain region 
| ROC curves (hippocampus L/R) | Basic vs full model AUC comparison 
| Protein × time-points | Loess trajectory of protein concentration over years 
| SHBG × MIND interaction | Scatter with stratified regression lines 
| SHBG subgroup (MIND diet) | OR forest plot stratified by MIND diet score 
| PRS validation | Forest plot of OR for genetic risk scores (ADNI) 

---

