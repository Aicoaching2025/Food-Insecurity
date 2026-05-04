# =============================================================================
# patch_trend.R
# -----------------------------------------------------------------------------
# A targeted fix for the broken trend parsing in 02_clean_data.R.
#
# Problem: the original script picked "Food insecure-1,000" (raw counts in
# thousands) instead of "Food insecure-percent" because both columns match
# the "food_insecur" pattern, and it picked the first.
#
# This patch:
#   1. Re-reads the trend sheet correctly
#   2. Filters to Category == "All households" (one row per year)
#   3. Picks the -percent columns explicitly
#   4. Overwrites usda_trend.rds
#   5. Re-runs the Slide 2 analysis from 03_analysis.R
#
# Run this AFTER 02_clean_data.R and 03_analysis.R have been run once.
# After this patch, you can proceed directly to writing 04_visualizations.R.
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(readxl)
  library(dplyr)
  library(janitor)
  library(tibble)
})

raw_dir  <- here::here("data", "raw")
proc_dir <- here::here("data", "processed")
USDA_FILE <- file.path(raw_dir, "usda_ers_interactive_charts.xlsx")

message("\n", strrep("=", 70))
message("PATCH: fixing usda_trend.rds")
message(strrep("=", 70))

# ---- Step 1: read the trend sheet correctly ---------------------------------
trend_raw <- readxl::read_excel(USDA_FILE,
                                sheet = "Food security all households",
                                skip = 1)

message("Raw trend sheet rows: ", nrow(trend_raw))
message("Columns: ", paste(names(trend_raw), collapse = " | "))

# ---- Step 2: filter to All households, then pick -percent columns ----------
# The "All households" rows have:
#   Category = "All households"
#   Subcategory = "Not applicable"
# These give us one row per year with the national prevalence rate.
trend_clean <- trend_raw |>
  dplyr::filter(Category == "All households",
                Subcategory == "Not applicable") |>
  dplyr::transmute(
    year              = as.integer(Year),
    food_insecure_pct = as.numeric(`Food insecure-percent`),
    very_low_pct      = suppressWarnings(as.numeric(`Very low food security-percent`))
  ) |>
  dplyr::filter(!is.na(year), !is.na(food_insecure_pct)) |>
  dplyr::arrange(year)

message("\nFiltered trend (head + tail):")
print(utils::head(trend_clean, 3))
print(utils::tail(trend_clean, 3))
message("Years covered: ", min(trend_clean$year), " to ", max(trend_clean$year))
message("Range of food insecurity: ",
        sprintf("%.1f%% to %.1f%%",
                min(trend_clean$food_insecure_pct),
                max(trend_clean$food_insecure_pct)))

# Save it.
saveRDS(trend_clean, file.path(proc_dir, "usda_trend.rds"))
message("[OK] usda_trend.rds overwritten with corrected data")


# ---- Step 3: re-run the Slide 2 analysis ------------------------------------
message("\n", strrep("=", 70))
message("Re-running Slide 2 analysis with corrected trend")
message(strrep("=", 70))

trend <- trend_clean

# Compute the trough (2020-2022 window where food insecurity hit its lowest).
trough_window <- trend |> dplyr::filter(year >= 2020, year <= 2022)
trough_year <- trough_window$year[which.min(trough_window$food_insecure_pct)]
trough_pct  <- min(trough_window$food_insecure_pct, na.rm = TRUE)

latest_year <- max(trend$year)
latest_pct  <- trend$food_insecure_pct[trend$year == latest_year]
peak_pct    <- max(trend$food_insecure_pct[trend$year >= 2020], na.rm = TRUE)
peak_year   <- trend$year[trend$food_insecure_pct == peak_pct &
                          trend$year >= 2020][1]

slide2_trend <- trend |>
  dplyr::mutate(
    annotation = dplyr::case_when(
      year == trough_year ~ "trough",
      year == peak_year   ~ "peak",
      year == latest_year ~ "latest",
      TRUE                ~ NA_character_
    )
  )

message(sprintf("Trough: %d (%.1f%%)", trough_year, trough_pct))
message(sprintf("Peak since 2020: %d (%.1f%%)", peak_year, peak_pct))
message(sprintf("Latest: %d (%.1f%%)", latest_year, latest_pct))
message(sprintf("Reversal: +%.1f percentage points from trough to latest",
                latest_pct - trough_pct))

slide2_summary <- list(
  trend_data = slide2_trend,
  trough_year = trough_year,
  trough_pct = trough_pct,
  peak_year = peak_year,
  peak_pct = peak_pct,
  latest_year = latest_year,
  latest_pct = latest_pct,
  reversal_pp = latest_pct - trough_pct,
  affected_population = 47.4e6
)

saveRDS(slide2_summary, file.path(proc_dir, "slide2_trend.rds"))
message("[OK] slide2_trend.rds overwritten with corrected analysis")

message("\n", strrep("=", 70))
message("PATCH COMPLETE")
message(strrep("=", 70))
message("\nThe trend should now show realistic numbers (10-14% range).")
message("The reversal should be a few percentage points, not thousands.")
message("\nReady to write 04_visualizations.R.\n")