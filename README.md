# Food Insecurity in the United States

A reproducible R analysis and Quarto reveal.js presentation built for a 
Congressional brief on food insecurity in the U.S. Authored by Candace Grant 
(Birds and Roses LLC) for CUNY SPS DATA 620 / capstone preparation.

## Quick start

git clone https://github.com/Aicoaching2025/FoodInsecurity.git
cd FoodInsecurity

# In R or RStudio:
source("00_setup.R")           # installs packages (~5 min first time)
source("01_fetch_data.R")      # downloads ~12 MB of federal data
source("02_clean_data.R")      # tidies the data  
source("03_analysis.R")        # statistical analysis
source("04_visualizations.R")  # builds 5 chart PNGs
source("06_demographic_charts.R")  # builds 3 demographic charts

# Charts land in figures/ at 300 DPI ready for slides.

## What you'll get

[embed all 7 final chart PNGs here — interviewer can see results without running anything]

## The 5-slide story arc

1. The Problem — 47.4M Americans food insecure
2. Poverty drives it — 5× higher rate below the poverty line
3. The trend reversal — we fixed it in 2021, then unfixed it
4. The disease burden — NHANES weighted prevalence + Berkowitz cost
5. The fix — 8 states acted, 42 are waiting

## Data sources

| Source | What | Used for |
|---|---|---|
| USDA ERS ERR-358 (2024) | National + state food insecurity | Slides 1, 2, 3 |
| Census ACS 2023 | State poverty rates | Slide 1 |
| NHANES 2017–March 2020 | Microdata for prevalence | Slide 4 |
| Berkowitz et al. 2018 | $52.9B cost figure | Slide 4 |
| Hoynes et al. 2016 | Lifetime trajectory | Slide 5 (lifetime cost) |
| FRAC 2024 | Universal meals outcomes | Slide 5 (the fix) |

## Methodology notes

NHANES uses complex survey design (stratified clustering with weights). 
We use the `survey` package with `WTMECPRP`, `SDMVSTRA`, and `SDMVPSU` 
specified, and `nest = TRUE` because PSUs nest within strata.

Food insecurity defined per USDA convention: FSDHH categories 3 (low) 
and 4 (very low) combined.
