# =============================================================================
# 02_clean_data.R
# -----------------------------------------------------------------------------
# Project : Food Insecurity in the United States -- A Congressional Brief
# Author  : Candace Grant (Birds and Roses LLC)

# =============================================================================


# ---- 0. SETUP ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(readxl)
  library(haven)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(tibble)
  library(survey)
})

raw_dir  <- here::here("data", "raw")
proc_dir <- here::here("data", "processed")
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

# USDA workbook sheet names (confirmed via excel_sheets() during dev).
USDA_FILE          <- file.path(raw_dir, "usda_ers_interactive_charts.xlsx")
SHEET_TREND        <- "Food security all households"
SHEET_HH_CHILDREN  <- "Food security HHs with children"
SHEET_CHILD_TRENDS <- "Child food security trends"
SHEET_DEMOGRAPHICS <- "Edu-employment-disability"
SHEET_STATE        <- "Food security by State"

# Helper for prettier console output.
section <- function(title) {
  message("\n", strrep("=", 70))
  message(title)
  message(strrep("=", 70))
}


# ---- 1. USDA NATIONAL TREND (Slide 2) ---------------------------------------

section("[1/4] USDA -- national trend (households)")

trend_raw <- readxl::read_excel(USDA_FILE, sheet = SHEET_TREND, skip = 1)
message("Columns: ", paste(names(trend_raw), collapse = " | "))
message("Rows: ", nrow(trend_raw))

trend_clean <- trend_raw |>
  janitor::clean_names()

message("After clean_names(): ", paste(names(trend_clean), collapse = " | "))

# Pick year + food insecurity columns. The first column should be year-like;
# we look for "food_insecur" and "very_low" patterns in the rest.
year_col          <- names(trend_clean)[1]
food_insecure_col <- grep("^food_insecur", names(trend_clean), value = TRUE)[1]
very_low_col      <- grep("very_low",      names(trend_clean), value = TRUE)[1]

message("Using -- year: ", year_col,
        " | food insecure: ", food_insecure_col,
        " | very low: ", very_low_col)

usda_trend <- trend_clean |>
  dplyr::transmute(
    year              = suppressWarnings(as.integer(.data[[year_col]])),
    food_insecure_pct = suppressWarnings(as.numeric(.data[[food_insecure_col]])),
    very_low_pct      = if (!is.na(very_low_col))
                          suppressWarnings(as.numeric(.data[[very_low_col]]))
                        else NA_real_
  ) |>
  dplyr::filter(!is.na(year), !is.na(food_insecure_pct)) |>
  dplyr::arrange(year)

message("\nFinal usda_trend (head + tail):")
print(utils::head(usda_trend, 3))
print(utils::tail(usda_trend, 3))

saveRDS(usda_trend, file.path(proc_dir, "usda_trend.rds"))
message("[OK] data/processed/usda_trend.rds  (", nrow(usda_trend), " rows)")


# ---- 2. STATE-LEVEL FOOD INSECURITY x POVERTY (Slide 1) --------------------
# Two pieces:
#   (a) USDA state file -- 3-yr rolling avg, multiple time windows stacked
#       Columns: Year (e.g. "2022-2024"), State (abbreviation), prevalence
#   (b) Census ACS 2023 -- state poverty rate, full state names
#
# Strategy: filter USDA to the most recent 3-year range, convert state
# abbreviations to full names, join to Census on state_name.
section("[2/4] USDA state + Census poverty -> Slide 1 scatter")

# (a) USDA state-level data
state_raw <- readxl::read_excel(USDA_FILE, sheet = SHEET_STATE, skip = 1)
message("USDA state -- columns: ", paste(names(state_raw), collapse = " | "))
message("USDA state -- rows: ", nrow(state_raw))

# Most recent 3-year window. We confirmed 2022-2024 is the latest in the
# current workbook; we still pick programmatically so this auto-updates
# if USDA reissues with newer data.
all_years <- unique(state_raw[[1]])  # first column = Year
all_years <- all_years[!is.na(all_years)]
latest_year <- tail(sort(all_years), 1)
message("Latest year window: ", latest_year)

state_clean <- state_raw |>
  janitor::clean_names()
message("After clean_names: ", paste(names(state_clean), collapse = " | "))

# Identify the columns by name pattern.
state_year_col <- names(state_clean)[1]
state_abbr_col <- names(state_clean)[2]
state_fi_col   <- grep("^food_insecurity_prevalence$|^food_insecur",
                       names(state_clean), value = TRUE)[1]

message("Using -- year: ", state_year_col,
        " | state: ", state_abbr_col,
        " | food insecure: ", state_fi_col)

state_fi <- state_clean |>
  dplyr::filter(.data[[state_year_col]] == latest_year) |>
  dplyr::transmute(
    state_abb         = .data[[state_abbr_col]],
    food_insecure_pct = suppressWarnings(as.numeric(.data[[state_fi_col]]))
  ) |>
  dplyr::filter(!is.na(food_insecure_pct))

# Convert abbreviations to full names. R has built-in constants:
#   state.abb  -> "AL" "AK" "AZ" ...   (50 states, no DC)
#   state.name -> "Alabama" "Alaska" ...
# We add DC manually because R's state constants don't include it.
abbr_to_name <- c(
  setNames(state.name, state.abb),
  "DC" = "District of Columbia"
)

state_fi <- state_fi |>
  dplyr::mutate(state_name = abbr_to_name[state_abb]) |>
  # Drop "US" (national total) and any unmatched abbreviations.
  dplyr::filter(!is.na(state_name))

message("USDA state rows for ", latest_year, ": ", nrow(state_fi))

# (b) Census ACS 2023 state poverty.
census_raw <- jsonlite::fromJSON(
  file.path(raw_dir, "census_acs_2023_state_poverty.json")
)

state_poverty <- tibble::as_tibble(
  census_raw[-1, , drop = FALSE], .name_repair = "minimal"
)
names(state_poverty) <- census_raw[1, ]
state_poverty <- state_poverty |>
  janitor::clean_names() |>
  dplyr::transmute(
    state_name  = name,
    poverty_pct = as.numeric(s1701_c03_001e),
    state_fips  = state
  ) |>
  dplyr::filter(state_fips != "72") |>   # drop Puerto Rico
  dplyr::arrange(state_name)

message("Census poverty rows: ", nrow(state_poverty), " (expect 51)")

# Join. inner_join means we keep only states present in both sources.
state_poverty_fi <- state_fi |>
  dplyr::inner_join(state_poverty, by = "state_name") |>
  dplyr::select(state_name, state_abb, poverty_pct, food_insecure_pct) |>
  dplyr::arrange(state_name)

message("\nJoined state data (head):")
print(utils::head(state_poverty_fi, 4))
message("Joined rows: ", nrow(state_poverty_fi), " (expect 51 = 50 states + DC)")

# Quick sanity check on the correlation -- this is the headline number for
# Slide 1's side panel. We expect r ~ 0.85-0.90.
r <- cor(state_poverty_fi$poverty_pct,
         state_poverty_fi$food_insecure_pct,
         use = "complete.obs")
message(sprintf("\nPearson r (poverty x food insecurity): %.3f", r))

saveRDS(state_poverty_fi, file.path(proc_dir, "state_poverty_fi.rds"))
message("[OK] data/processed/state_poverty_fi.rds  (",
        nrow(state_poverty_fi), " rows)")


# ---- 3. NHANES SURVEY DESIGN (Slide 3) -------------------------------------
# Five NHANES files joined on SEQN. The survey design object accounts for
# stratified clustered sampling and is essential for defensible prevalence.
section("[3/4] NHANES 2017-March 2020 -- merge + survey design")

# Variable definitions (from CDC variable codebooks):
#   RIAGENDR   1=Male, 2=Female
#   RIDAGEYR   age in years
#   SDMVPSU    sampling primary unit
#   SDMVSTRA   sampling stratum
#   WTMECPRP   MEC examination weight (pre-pandemic file; combined cycles)
#   FSDHH      household food security: 1=full, 2=marginal, 3=low, 4=very low
#   BMXBMI     body mass index
#   BPXOSY1    systolic BP, oscillometric, reading 1
#   BPXODI1    diastolic BP, oscillometric, reading 1
#   LBXGH      glycohemoglobin (HbA1c) %
#
# Health outcome thresholds:
#   obesity      BMXBMI    >= 30
#   hypertension BPXOSY1   >= 130 OR BPXODI1 >= 80   (2017 ACC/AHA)
#   diabetes     LBXGH     >= 6.5                     (ADA)

demo <- haven::read_xpt(file.path(raw_dir, "nhanes_p_demo.xpt"))
fsq  <- haven::read_xpt(file.path(raw_dir, "nhanes_p_fsq.xpt"))
bmx  <- haven::read_xpt(file.path(raw_dir, "nhanes_p_bmx.xpt"))
bpxo <- haven::read_xpt(file.path(raw_dir, "nhanes_p_bpxo.xpt"))
ghb  <- haven::read_xpt(file.path(raw_dir, "nhanes_p_ghb.xpt"))

message("Row counts -- DEMO: ", nrow(demo), " | FSQ: ", nrow(fsq),
        " | BMX: ", nrow(bmx), " | BPXO: ", nrow(bpxo), " | GHB: ", nrow(ghb))

# DEMO is the spine. Left-join the others -- we keep every respondent in the
# sampling frame, even if missing some health measurements, because the
# survey weights need everyone for proper standard errors.
nhanes <- demo |>
  dplyr::select(SEQN, RIAGENDR, RIDAGEYR, SDMVPSU, SDMVSTRA, WTMECPRP) |>
  dplyr::left_join(fsq  |> dplyr::select(SEQN, FSDHH), by = "SEQN") |>
  dplyr::left_join(bmx  |> dplyr::select(SEQN, BMXBMI), by = "SEQN") |>
  dplyr::left_join(bpxo |> dplyr::select(SEQN, BPXOSY1, BPXODI1), by = "SEQN") |>
  dplyr::left_join(ghb  |> dplyr::select(SEQN, LBXGH),  by = "SEQN") |>
  dplyr::mutate(
    sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
    age_group = cut(RIDAGEYR,
                    breaks = c(-Inf, 17, 39, 59, Inf),
                    labels = c("Under 18", "18-39", "40-59", "60+")),
    # Food insecurity: FSDHH 3 or 4 = food insecure (USDA convention).
    food_insecure = dplyr::case_when(
      FSDHH %in% c(3, 4) ~ "Food insecure",
      FSDHH %in% c(1, 2) ~ "Food secure",
      TRUE               ~ NA_character_
    ),
    food_insecure = factor(food_insecure,
                           levels = c("Food secure", "Food insecure")),
    # Health outcomes (NA where measurement missing).
    obesity      = as.integer(BMXBMI >= 30),
    hypertension = as.integer(BPXOSY1 >= 130 | BPXODI1 >= 80),
    diabetes     = as.integer(LBXGH >= 6.5)
  )

message("Merged NHANES rows: ", nrow(nhanes))
message("Adults (18+): ", sum(nhanes$RIDAGEYR >= 18, na.rm = TRUE))

# Build the survey design object. CRITICAL: nest = TRUE because PSUs nest
# within strata in NHANES (PSU IDs are NOT unique across strata). Forgetting
# nest = TRUE silently produces wrong standard errors.
nhanes_design <- survey::svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMECPRP,
  nest    = TRUE,
  data    = nhanes
)

message("[OK] survey design built. Class: ",
        paste(class(nhanes_design), collapse = ", "))

saveRDS(nhanes_design, file.path(proc_dir, "nhanes_design.rds"))
message("[OK] data/processed/nhanes_design.rds")


# ---- 4. USDA DEMOGRAPHIC DISPARITY DATA (Slide 1 callouts) -----------------
# Hand-encoded from USDA ERR-358 (2024) and CDC NCHS Data Brief 465 (2023).
# These are published headline rates from peer-reviewed federal reports;
# encoding them inline is standard practice for callout numbers.
section("[4/4] Demographic disparity callouts (Slide 1)")

usda_demographics <- tibble::tribble(
  ~group,                            ~food_insecure_pct, ~source,
  "All U.S. households",                          13.5, "USDA ERR-358 (2024)",
  "Households with children",                     17.9, "USDA ERR-358 (2024)",
  "Single women with children",                   34.7, "USDA ERR-358 (2024)",
  "Single men with children",                     22.8, "USDA ERR-358 (2024)",
  "Black, non-Hispanic households",               23.3, "USDA ERR-358 (2024)",
  "Hispanic households",                          21.9, "USDA ERR-358 (2024)",
  "White, non-Hispanic households",                9.0, "USDA ERR-358 (2024)",
  "Adults with disability",                       15.0, "CDC NCHS DB465 (2023)",
  "Women (18+)",                                   6.5, "CDC NCHS DB465 (2023)",
  "Men (18+)",                                     5.2, "CDC NCHS DB465 (2023)"
)

print(usda_demographics)

saveRDS(usda_demographics, file.path(proc_dir, "usda_demographics.rds"))
message("[OK] data/processed/usda_demographics.rds  (",
        nrow(usda_demographics), " rows)")


# ---- 5. SUMMARY -------------------------------------------------------------
message("\n", strrep("=", 70))
message("CLEANING COMPLETE")
message(strrep("=", 70))
message("\nFiles in data/processed/:")
processed_files <- list.files(proc_dir, full.names = FALSE)
for (f in processed_files) {
  size_kb <- file.size(file.path(proc_dir, f)) / 1024
  message(sprintf("  %-30s %8.1f KB", f, size_kb))
}
message("\nNext: source('03_analysis.R')\n")