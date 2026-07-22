# R-Code-SS-Hake-Sex

This is the R code to reproduce the models and figures from the manuscript ‘Investigating the dynamics of sex-structured stock assessment models under sexual size dimorphism’.

The structure consists of two folders. The first one, ‘01 Models’, where we will find the 3 configurations of the Stock Synthesis hake model: Single sex C, Single sex F and Two sex. In the second one, ‘02 Figures’, we will find a script that allows us to make the figures of the article from the 3 models.

01 Models - For each of these 3 model configurations, we find 5 scripts (common to all of them). 

- 01 Run.R: run base model
- 02 Retros.R: carry out retrospective analysis and plots
- 03 Diags.R: perform model diagnostics through ss3diags package
- 04 Forecast.R: do forecast for different F multipliers (from 0 to Flim)
- 05 F scenarios.R: prepare, run and forecast for 2 alternative Fleet F scenarios

02 Figures - Once the 01 Model scripts are run, make the article Figures.

- Figures.R: create output plots

## Reproducibility

Analyses were run in **R 4.5.1** with **Stock Synthesis 3.30.18**.
Core R package versions:

| Package  | Version |
|----------|---------|
| r4ss     | 1.54.0  |
| ss3diags | 1.10.3  |
| ggplot2  | 4.0.3   |
| dplyr    | 1.2.1   |
| tidyr    | 1.3.2   |

Figures/tables also use reshape2 (1.4.5), ggpubr (0.6.1) and,
for the supplementary tables, officer (0.7.5) + flextable (0.9.11).

Install a specific version, e.g.:
install.packages("remotes")
remotes::install_version("ggplot2", "4.0.3")
remotes::install_github("r4ss/r4ss@v1.54.0")
remotes::install_github("PIFSCstockassessments/ss3diags@v1.10.3")

## References:

https://vlab.noaa.gov/web/stock-synthesis

https://github.com/r4ss/r4ss

https://pifscstockassessments.github.io/ss3diags/
