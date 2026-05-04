# =============================================================================
# 00_setup.R
# -----------------------------------------------------------------------------
# Project : Food Insecurity in the United States -- A Congressional Brief
# Author  : Candace Grant (Birds and Roses LLC)
#
# Run this ONCE when you first set up the project. It will:
#   1. Install renv (if missing)
#   2. Initialize renv (creates renv/ folder + renv.lock for reproducibility)
#   3. Install every package the pipeline needs
#   4. Snapshot the lockfile so package versions are pinned
#
# After this, future contributors only need to run `renv::restore()` to get
# the same environment.
# =============================================================================

# ---- Step 1: Make sure renv itself is installed -----------------------------
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# ---- Step 2: Initialize the project -----------------------------------------
# `bare = TRUE` means renv won't auto-detect packages from your code -- we
# install them explicitly below, which is cleaner and more predictable.
# `restart = FALSE` keeps the current R session alive.
renv::init(bare = TRUE, restart = FALSE)

# ---- Step 3: Install every package the pipeline needs -----------------------
# Grouped by purpose. Comments explain what each one is for.

required_packages <- c(
  # --- Project plumbing ---
  "here",          # project-root-relative paths (works from any working dir)
  "renv",          # reproducibility

  # --- Data fetching ---
  "httr2",         # modern HTTP client for the Census API call
  "jsonlite",      # parse the Census API JSON response

  # --- Reading source files ---
  "readxl",        # read USDA ERS .xlsx workbook
  "readr",         # fast CSV; better defaults than base read.csv()
  "haven",         # read NHANES .xpt SAS transport files

  # --- Tidying and analysis ---
  "dplyr",         # data manipulation grammar
  "tidyr",         # pivot wide<->long, separate/unite columns
  "stringr",       # string cleanup on USDA labels
  "purrr",         # iteration over list-columns
  "tibble",        # modern data frames
  "janitor",       # clean_names() to standardize column names

  # --- Survey analysis (REQUIRED for NHANES) ---
  # NHANES uses a complex sample design: stratified clustering + weights.
  # Treating it as a simple random sample produces wrong prevalence estimates
  # AND wrong confidence intervals. The `survey` package is the industry
  # standard; `srvyr` wraps it in a tidyverse-friendly grammar.
  "survey",        # Lumley's complex-survey-design analysis
  "srvyr",         # dplyr-style wrapper around `survey`

  # --- Visualization ---
  "ggplot2",       # the chart engine
  "ggtext",        # rich-text labels for annotated titles in coral
  "ggrepel",       # smart label placement on the poverty scatter
  "scales",        # axis label formatting (percent_format, comma_format)
  "patchwork",     # combine multiple ggplots into one figure

  # --- Presentation ---
  "quarto",        # render the .qmd reveal.js deck from R
  "knitr",         # chunk processing
  "rmarkdown"      # support library Quarto pulls in
)

# Only install what's missing -- saves time on re-runs.
to_install <- setdiff(required_packages, rownames(installed.packages()))
if (length(to_install) > 0) {
  message(sprintf("Installing %d packages...", length(to_install)))
  install.packages(to_install)
} else {
  message("All required packages already installed.")
}

# ---- Step 4: Snapshot the lockfile ------------------------------------------
# This freezes the current package versions into renv.lock. Anyone who later
# clones this project and runs `renv::restore()` gets the exact same versions.
renv::snapshot(prompt = FALSE)

message("\n[OK] Setup complete. Next: source('01_fetch_data.R')\n")