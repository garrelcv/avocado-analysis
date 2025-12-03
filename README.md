# Statistical Analysis of U.S. Avocado Prices (2015-2018)

**Authors:** Christian Garrelts, Andrew Doherty, Savannah Manning  
**Course:** STAT383 - Probability and Statistics  
**Date:** Fall 2025

## Project Overview
This repository contains the raw data and R source code used to analyze the drivers of avocado pricing in the United States. The analysis investigates the impact of seasonality, product type (organic vs. conventional), and geographic region on market value using inferential statistics and regression modeling.

## Repository Contents
* `avocado.csv`: The raw dataset containing 18,249 observations of retail scan data (Source: Hass Avocado Board / Kaggle).
* `avocado_analysis.R`: The complete R script used to clean data, perform T-tests and ANOVA, build the regression model, and generate Figures 1-4.

## Key Outputs
Running the script generates the following visualizations:
* **Figure 1:** Monthly price trends (Seasonality analysis).
* **Figure 2:** Price distribution comparison (Organic vs. Conventional).
* **Figure 3:** Top 10 most expensive regions (Bar chart).
* **Figure 4:** Regression model diagnostics (Linearity, Normality, Variance checks).

## How to Run
To replicate the analysis:
1.  Download `avocado.csv` and `avocado_analysis.R`.
2.  Place both files in the same directory.
3.  Open the script in R Studio.
4.  Set your working directory to the source file location.
5.  Run the script.
