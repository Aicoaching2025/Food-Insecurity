# =============================================================================
# 01_fetch_data.R
# -----------------------------------------------------------------------------
# Project : Food Insecurity in the United States -- A Congressional Brief
# Author  : Candace Grant (Birds and Roses LLC)
#
# Pull the raw datasets that underpin the 5-slide story:
#   Slide 1: The Problem      -- USDA scale + ACS poverty side panel
#   Slide 2: The Trend        -- USDA 2001-2024 trend
#   Slide 3: Disease Burden   -- NHANES food security x health outcomes
#   Slide 4: Lifetime Cost    -- Hoynes et al. (encoded inline in 03)
#   Slide 5: The Fix          -- Universal meals data (encoded inline in 03)
#
# Run this script ONCE. Outputs land in data/raw/.
# Subsequent scripts read from data/raw/ and never re-download.
# =============================================================================


# ---- 0. SETUP ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(httr2)
  library(here)
})


# ---- 1. PATHS ---------------------------------------------------------------
raw_dir <- here::here("data", "raw")
if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)

# A friendly download helper. Wraps download.file() so we get:
#   - a consistent User-Agent (some federal servers reject default R UA)
#   - a skip-if-already-downloaded behaviour (fast re-runs)
#   - clear console messages
fetch <- function(url, dest, label, force = FALSE) {
  out <- file.path(raw_dir, dest)
  if (file.exists(out) && !force) {
    size_kb <- file.size(out) / 1024
    message(sprintf("  [skip] %-50s already on disk (%.1f KB)", label, size_kb))
    return(invisible(out))
  }
  message(sprintf("  [get]  %-50s -> %s", label, dest))
  # mode = "wb" is REQUIRED for binary files (xlsx, pdf, zip, xpt) on Mac.
  # Without it, R writes in text mode and corrupts the file.
  utils::download.file(
    url      = url,
    destfile = out,
    mode     = "wb",
    quiet    = TRUE,
    headers  = c("User-Agent" = "Mozilla/5.0 (Food Insecurity Research Project)")
  )
  invisible(out)
}


# ---- 2. USDA ECONOMIC RESEARCH SERVICE -------------------------------------
# The Interactive Charts data file is the master USDA workbook. It contains
# every chart's underlying data: trends 2001-2024 (Slide 2), state-level
# 3-yr averages, and demographic breakdowns. Single most important file for
# headline numbers.

message("\n[1/4] USDA Economic Research Service")

fetch(
  url   = "https://www.ers.usda.gov/media/649/data-file-for-interactive-charts.xlsx",
  dest  = "usda_ers_interactive_charts.xlsx",
  label = "USDA ERS interactive charts (XLSX)"
)

fetch(
  url   = "https://www.ers.usda.gov/media/799/food-security-csv-data-files.zip",
  dest  = "usda_ers_csv_bundle.zip",
  label = "USDA ERS interactive charts (CSV zip)"
)

# Citation reference; not parsed.
fetch(
  url   = "https://ers.usda.gov/sites/default/files/_laserfiche/publications/113623/ERR-358.pdf",
  dest  = "usda_err358_2024_report.pdf",
  label = "USDA ERS ERR-358 (2024 report PDF)"
)


# ---- 3. CDC NCHS DATA BRIEF 465 --------------------------------------------
# Citation reference for sex/age/disability disparity callouts. We use the
# published prevalence numbers (women 6.5%, men 5.2%, disability 15.0%) but
# the chart data on Slide 1 comes from USDA.

message("\n[2/4] CDC NCHS Data Brief 465")

fetch(
  url   = "https://www.cdc.gov/nchs/data/databriefs/db465.pdf",
  dest  = "cdc_nchs_db465_brief.pdf",
  label = "CDC NCHS Data Brief 465 (main)"
)

fetch(
  url   = "https://www.cdc.gov/nchs/data/databriefs/db465-tables.pdf",
  dest  = "cdc_nchs_db465_tables.pdf",
  label = "CDC NCHS Data Brief 465 (tables)"
)


# ---- 4. NHANES 2017-MARCH 2020 PRE-PANDEMIC FILES (Slide 3) ----------------
# Key data pull for the disease-burden slide. We compute food-insecurity-
# stratified prevalence of obesity, hypertension, and diabetes from
# microdata. NHANES uses a complex sample design (stratified clustering with
# non-self-representing PSUs); we use the survey package with weights in
# 03_analysis.R.
#
# Why 2017-March 2020? Most recent nationally representative cycle. The
# 2019-2020 partial-cycle data alone aren't representative, so CDC combined
# them with 2017-2018 and re-weighted. CDC explicitly recommends this file
# for cross-sectional prevalence work.
#
# Files we need and the variables we'll extract in 02_clean_data.R:
#   P_DEMO  -> SEQN, RIAGENDR (sex), RIDAGEYR (age),
#              SDMVPSU (PSU), SDMVSTRA (stratum), WTMECPRP (MEC weight)
#   P_FSQ   -> SEQN, FSDHH (household food security: 1 full -> 4 very low)
#   P_BMX   -> SEQN, BMXBMI (body mass index)
#   P_BPXO  -> SEQN, BPXOSY1, BPXODI1 (blood pressure, oscillometric)
#   P_GHB   -> SEQN, LBXGH  (glycohemoglobin %; >=6.5% indicates diabetes)
#
# We use BPXO (oscillometric) not BPX because the 2017-March 2020 cycle
# transitioned to automated oscillometric measurement.

message("\n[3/4] CDC NHANES 2017-March 2020 pre-pandemic files")

nhanes_base <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/"

fetch(paste0(nhanes_base, "P_DEMO.xpt"), "nhanes_p_demo.xpt",
      "NHANES P_DEMO (demographics + weights)")
fetch(paste0(nhanes_base, "P_FSQ.xpt"),  "nhanes_p_fsq.xpt",
      "NHANES P_FSQ (food security)")
fetch(paste0(nhanes_base, "P_BMX.xpt"),  "nhanes_p_bmx.xpt",
      "NHANES P_BMX (body measures, for BMI)")
fetch(paste0(nhanes_base, "P_BPXO.xpt"), "nhanes_p_bpxo.xpt",
      "NHANES P_BPXO (blood pressure, oscillometric)")
fetch(paste0(nhanes_base, "P_GHB.xpt"),  "nhanes_p_ghb.xpt",
      "NHANES P_GHB (glycohemoglobin, for diabetes)")


# ---- 5. CENSUS ACS 2023 -- STATE POVERTY RATES (Slide 1 side panel) -------
# State poverty rates for the side panel showing poverty drives food
# insecurity. Variable S1701_C03_001E = % of population below poverty level.
# No API key required for this single call (limit is 500/day per IP).

message("\n[4/4] U.S. Census Bureau -- ACS 1-year 2023")

census_out <- file.path(raw_dir, "census_acs_2023_state_poverty.json")

if (!file.exists(census_out)) {
  message("  [get]  ACS 2023 S1701 (state poverty %)             -> census_acs_2023_state_poverty.json")

  resp <- httr2::request("https://api.census.gov/data/2023/acs/acs1/subject") |>
    httr2::req_url_query(
      get   = "NAME,S1701_C03_001E",
      `for` = "state:*"
    ) |>
    httr2::req_user_agent("Food Insecurity Research Project") |>
    httr2::req_perform()

  writeLines(httr2::resp_body_string(resp), census_out)
} else {
  message("  [skip] ACS 2023 S1701                                already on disk")
}


# ---- 6. SUMMARY -------------------------------------------------------------
message("\n--- Summary ----------------------------------------------------------")
files <- list.files(raw_dir, full.names = TRUE)
sizes <- file.size(files) / 1024  # KB
for (i in seq_along(files)) {
  message(sprintf("  %-50s %10.1f KB", basename(files[i]), sizes[i]))
}
message(sprintf("\nTotal: %d files, %.2f MB on disk\n",
                length(files), sum(sizes) / 1024))


# ---- NOTES ON SOURCES ENCODED INLINE ----------------------------------------
# These published summary statistics are encoded as small tibbles directly
# in 03_analysis.R rather than fetched. Standard practice for published
# headline numbers from peer-reviewed papers and reports.
#
#   * Berkowitz et al. (2018), Health Affairs -- $52.9B annual healthcare
#     cost attributable to food insecurity; $1,834 per food-insecure adult.
#     Used in Slide 3 cost callout.
#
#   * Hoynes, Schanzenbach & Almond (2016), AER -- adult outcomes for
#     children with early-life food assistance access:
#       -27% adult metabolic syndrome
#       +29% economic self-sufficiency (women)
#       -13% adult reliance on transfer programs
#     Used as Slide 4's narrative spine.
#
#   * FRAC State of Healthy School Meals for All (2024) and California
#     Universal Meals follow-up data -- used in Slide 5.
# =============================================================================