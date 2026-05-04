# =============================================================================
# 03_analysis.R
# -----------------------------------------------------------------------------
# Project : Food Insecurity in the United States -- A Congressional Brief
# Author  : Candace Grant (Birds and Roses LLC)
#
# Take the cleaned data from data/processed/ and produce slide-ready
# analytical outputs. Each output is a small tidy data frame designed to
# feed exactly one chart in 04_visualizations.R.
#
# Outputs:
#   slide1_correlation.rds  -- Pearson r + 95% CI + regression for poverty x FI
#   slide2_trend.rds        -- annotated trend with 2021 dip + 2024 reversal
#   slide3_prevalence.rds   -- NHANES weighted prevalence ratios
#   slide4_lifetime.rds     -- Hoynes et al. (2016) effect sizes (encoded)
#   slide5_evidence.rds     -- Universal meals natural experiment (encoded)
#
# Run after 02_clean_data.R has populated data/processed/.
# =============================================================================


# ---- 0. SETUP ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(survey)
  library(srvyr)        # tidyverse wrapper around survey
})

proc_dir <- here::here("data", "processed")

section <- function(title) {
  message("\n", strrep("=", 70))
  message(title)
  message(strrep("=", 70))
}


# ---- 1. SLIDE 1 -- POVERTY x FOOD INSECURITY CORRELATION -------------------
# Input  : state_poverty_fi.rds (51 states, 2 numeric columns)
# Output : list with the correlation, CI, regression line, and full state data.
# Story  : "Poverty drives food insecurity at the state level (r = 0.XX)."
section("[1/5] Slide 1 -- state-level poverty x food insecurity")

state_data <- readRDS(file.path(proc_dir, "state_poverty_fi.rds"))

# Pearson correlation with 95% CI. cor.test() does this in one call.
ct <- cor.test(state_data$poverty_pct,
               state_data$food_insecure_pct,
               method = "pearson")

# Linear regression so we can draw a fitted line on the scatter. We treat
# poverty as the predictor (X) and food insecurity as the outcome (Y) --
# this matches the causal narrative we're telling.
fit <- lm(food_insecure_pct ~ poverty_pct, data = state_data)
fit_summary <- summary(fit)

# Generate predictions across the observed poverty range for plotting.
poverty_grid <- seq(min(state_data$poverty_pct),
                    max(state_data$poverty_pct),
                    length.out = 100)
fit_line <- tibble::tibble(
  poverty_pct       = poverty_grid,
  food_insecure_pct = predict(fit, newdata = tibble::tibble(poverty_pct = poverty_grid))
)

slide1_correlation <- list(
  state_data  = state_data,
  fit_line    = fit_line,
  pearson_r   = unname(ct$estimate),
  conf_low    = ct$conf.int[1],
  conf_high   = ct$conf.int[2],
  p_value     = ct$p.value,
  r_squared   = fit_summary$r.squared,
  intercept   = unname(coef(fit)[1]),
  slope       = unname(coef(fit)[2])
)

message(sprintf("Pearson r:        %.3f  (95%% CI: %.3f to %.3f)",
                slide1_correlation$pearson_r,
                slide1_correlation$conf_low,
                slide1_correlation$conf_high))
message(sprintf("R-squared:        %.3f  (poverty explains %.1f%% of variance)",
                slide1_correlation$r_squared,
                100 * slide1_correlation$r_squared))
message(sprintf("Regression line:  food_insecure = %.2f + %.2f * poverty",
                slide1_correlation$intercept,
                slide1_correlation$slope))
message(sprintf("p-value:          %.2e",
                slide1_correlation$p_value))

saveRDS(slide1_correlation, file.path(proc_dir, "slide1_correlation.rds"))
message("[OK] slide1_correlation.rds")


# ---- 2. SLIDE 2 -- TREND WITH ANNOTATIONS ----------------------------------
# Input  : usda_trend.rds (year, food_insecure_pct, very_low_pct)
# Output : trend with annotation flags for the visual story.
# Story  : "We fixed it in 2021. We unfixed it by 2024."
section("[2/5] Slide 2 -- trend reversal")

trend <- readRDS(file.path(proc_dir, "usda_trend.rds"))

# Compute the change from the 2021 trough to the most recent year.
# We're flexible about the trough year -- use whichever year had the
# minimum food_insecure_pct in 2020-2022.
trough_window <- trend |> dplyr::filter(year >= 2020, year <= 2022)
trough_year <- trough_window$year[which.min(trough_window$food_insecure_pct)]
trough_pct  <- min(trough_window$food_insecure_pct, na.rm = TRUE)

latest_year <- max(trend$year)
latest_pct  <- trend$food_insecure_pct[trend$year == latest_year]
peak_pct    <- max(trend$food_insecure_pct[trend$year >= 2020], na.rm = TRUE)
peak_year   <- trend$year[trend$food_insecure_pct == peak_pct &
                          trend$year >= 2020][1]

# Annotation flags for the visualization layer.
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

# Estimated affected population. USDA reports ~334M U.S. residents (2024).
# At 13.5% food insecurity, that's ~45M individuals -- but the 47.4M figure
# uses USDA's own population denominator from ERR-358. We use 47.4M as a
# stored constant.
affected_population_2024 <- 47.4e6
message(sprintf("Affected population (2024): %.1fM", affected_population_2024 / 1e6))

slide2_summary <- list(
  trend_data = slide2_trend,
  trough_year = trough_year,
  trough_pct = trough_pct,
  peak_year = peak_year,
  peak_pct = peak_pct,
  latest_year = latest_year,
  latest_pct = latest_pct,
  reversal_pp = latest_pct - trough_pct,
  affected_population = affected_population_2024
)

saveRDS(slide2_summary, file.path(proc_dir, "slide2_trend.rds"))
message("[OK] slide2_trend.rds")


# ---- 3. SLIDE 3 -- NHANES PREVALENCE RATIOS --------------------------------
# Input  : nhanes_design.rds (svydesign object)
# Output : prevalence + ratio table for obesity, hypertension, diabetes.
# Story  : "Food-insecure adults have substantially higher disease prevalence."
#
# Method note: svyby() runs svymean() within each level of the by= variable.
# We get population-weighted prevalence with proper standard errors that
# account for stratified clustering.
section("[3/5] Slide 3 -- NHANES disease prevalence by food security status")

nhanes_design <- readRDS(file.path(proc_dir, "nhanes_design.rds"))

# Filter to adults (>=18). MUST use subset() on the design, not filter()
# on the underlying data, to preserve the survey structure.
adults_design <- subset(nhanes_design, RIDAGEYR >= 18)
message("Adults in design: ",
        sum(weights(adults_design$variables) > 0 & adults_design$variables$RIDAGEYR >= 18,
            na.rm = TRUE))

# Compute weighted prevalence for each outcome by food security status.
# na.rm = TRUE here means missing measurements are dropped from the
# numerator AND denominator -- i.e., we estimate prevalence among adults
# with a valid measurement, which is the standard approach.
compute_prevalence <- function(design, outcome_var, outcome_label) {
  fmla <- as.formula(paste0("~", outcome_var))
  result <- survey::svyby(
    fmla,
    by      = ~food_insecure,
    design  = design,
    FUN     = survey::svymean,
    na.rm   = TRUE
  )
  # Returns a data frame with food_insecure, the outcome (mean = prevalence),
  # and se. Tidy it up.
  tibble::tibble(
    outcome        = outcome_label,
    food_insecure  = as.character(result$food_insecure),
    prevalence     = result[[outcome_var]],
    se             = result$se,
    ci_low         = result[[outcome_var]] - 1.96 * result$se,
    ci_high        = result[[outcome_var]] + 1.96 * result$se
  )
}

prevalence_obesity   <- compute_prevalence(adults_design, "obesity",      "Obesity")
prevalence_hyperten  <- compute_prevalence(adults_design, "hypertension", "Hypertension")
prevalence_diabetes  <- compute_prevalence(adults_design, "diabetes",     "Diabetes")

prevalence_all <- dplyr::bind_rows(prevalence_obesity,
                                   prevalence_hyperten,
                                   prevalence_diabetes) |>
  dplyr::filter(!is.na(food_insecure))

message("\nWeighted prevalence by food security status:")
prevalence_print <- prevalence_all |>
  dplyr::mutate(
    prevalence_pct = sprintf("%.1f%% (\u00b1%.1f)",
                             100 * prevalence,
                             100 * 1.96 * se)
  ) |>
  dplyr::select(outcome, food_insecure, prevalence_pct)
print(prevalence_print)

# Compute prevalence ratios: food-insecure / food-secure.
prevalence_ratios <- prevalence_all |>
  dplyr::select(outcome, food_insecure, prevalence) |>
  tidyr::pivot_wider(names_from = food_insecure, values_from = prevalence) |>
  dplyr::mutate(
    ratio = `Food insecure` / `Food secure`,
    excess_pp = (`Food insecure` - `Food secure`) * 100
  )

message("\nPrevalence ratios (food-insecure / food-secure):")
ratios_print <- prevalence_ratios |>
  dplyr::mutate(
    summary = sprintf("%.2fx (%.1f pp higher)", ratio, excess_pp)
  ) |>
  dplyr::select(outcome, summary)
print(ratios_print)

slide3_prevalence <- list(
  prevalence = prevalence_all,
  ratios     = prevalence_ratios,
  # Cost callout from Berkowitz et al. (2018), Health Affairs.
  # https://doi.org/10.1377/hlthaff.2017.0096
  cost_billions_2014_dollars = 52.9,
  cost_per_adult_2014_dollars = 1834,
  cost_source = "Berkowitz et al., Health Affairs (2018)"
)

saveRDS(slide3_prevalence, file.path(proc_dir, "slide3_prevalence.rds"))
message("[OK] slide3_prevalence.rds")


# ---- 4. SLIDE 4 -- LIFETIME COST (HOYNES ET AL. 2016) ----------------------
# Hand-encoded effect sizes from Hoynes, Schanzenbach & Almond (2016),
# American Economic Review 106(4): 903-934.
# https://doi.org/10.1257/aer.20130375
#
# These are the headline causal estimates from a quasi-experimental study
# using county-level rollout of the Food Stamp Program in the 1960s-70s as
# a natural experiment. We encode them inline because:
#   (a) AER access is paywalled, replication data restricted;
#   (b) These are widely cited published estimates;
#   (c) The story requires effect sizes, not raw data.
#
# The signs are normalized so positive = improvement vs. counterfactual
# (control group of children who did not receive food assistance).
section("[4/5] Slide 4 -- lifetime trajectory (Hoynes et al. 2016)")

slide4_lifetime <- tibble::tribble(
  ~outcome,                                       ~effect_pct, ~direction,    ~category,
  "Adult metabolic syndrome",                            -27,  "decrease",   "Health",
  "High blood pressure",                                 -16,  "decrease",   "Health",
  "Adult obesity",                                       -21,  "decrease",   "Health",
  "Heart disease",                                       -22,  "decrease",   "Health",
  "Diabetes",                                            -10,  "decrease",   "Health",
  "Economic self-sufficiency (women)",                   +29,  "increase",   "Economic",
  "Adult earnings",                                       +6,  "increase",   "Economic",
  "Reliance on transfer programs (women)",               -13,  "decrease",   "Economic"
)

slide4_lifetime <- slide4_lifetime |>
  dplyr::mutate(
    abs_effect = abs(effect_pct),
    is_positive = direction == "increase",
    # For consistent visualization: "good outcomes" (improvements vs. control)
    # whether they're decreases (disease) or increases (earnings).
    is_improvement = (direction == "decrease" & category == "Health") |
                     (direction == "increase" & category == "Economic") |
                     (direction == "decrease" & outcome == "Reliance on transfer programs (women)")
  )

print(slide4_lifetime)

slide4_payload <- list(
  data = slide4_lifetime,
  citation = "Hoynes, Schanzenbach & Almond (2016), American Economic Review",
  doi = "10.1257/aer.20130375"
)

saveRDS(slide4_payload, file.path(proc_dir, "slide4_lifetime.rds"))
message("[OK] slide4_lifetime.rds")


# ---- 5. SLIDE 5 -- UNIVERSAL SCHOOL MEALS EVIDENCE -------------------------
# Hand-encoded outcomes from the universal school meals natural experiment.
# Eight states implemented universal free meals as of 2024-25 school year.
# Data sources:
#   - FRAC, "State of Healthy School Meals for All" (2024)
#   - California Department of Education, Universal Meals Program eval data
#   - Oregon Department of Education, school discipline data 2022-2023
#
# We're showing four headline outcomes that map directly to political
# decision criteria: participation, hunger reduction, health, and behavior.
section("[5/5] Slide 5 -- universal school meals evidence")

slide5_evidence <- tibble::tribble(
  ~metric,                                ~change_pct, ~unit,           ~source,
  "School lunch participation",                  +6.0, "percent",       "FRAC 2024",
  "Daily additional meals served",          +233656.0, "meals/day",     "FRAC 2024",
  "Childhood obesity (CEP schools, CA)",         -0.6, "percentage_pt", "CDE follow-up",
  "School suspensions (Oregon)",                 -0.7, "percentage_pt", "ODE 2022-2023",
  "Household food insecurity (HHs w/ kids)",     -5.1, "percent",       "FRAC 2024",
  "Annual program cost per child",               +700, "dollars",       "FRAC 2024"
)

# Cost-benefit framing for the closing slide.
program_cost_per_child <- 700
healthcare_savings_per_child <- 1834   # Berkowitz per-adult; conservative for kids
benefit_cost_ratio <- healthcare_savings_per_child / program_cost_per_child

slide5_payload <- list(
  metrics = slide5_evidence,
  states_implemented = 8,
  states_list = c("California", "Maine", "Colorado", "Minnesota", "New Mexico",
                  "Vermont", "Massachusetts", "Michigan"),
  benefit_cost_ratio = benefit_cost_ratio,
  closing_message = "8 states acted. 42 are waiting."
)

print(slide5_evidence)
message(sprintf("\nBenefit-cost ratio (illustrative): %.1fx", benefit_cost_ratio))
message(sprintf("States that have implemented: %d / 50",
                slide5_payload$states_implemented))

saveRDS(slide5_payload, file.path(proc_dir, "slide5_evidence.rds"))
message("[OK] slide5_evidence.rds")


# ---- 6. SUMMARY -------------------------------------------------------------
message("\n", strrep("=", 70))
message("ANALYSIS COMPLETE")
message(strrep("=", 70))
message("\nFiles in data/processed/:")
processed_files <- list.files(proc_dir, full.names = FALSE)
for (f in processed_files) {
  size_kb <- file.size(file.path(proc_dir, f)) / 1024
  message(sprintf("  %-30s %8.1f KB", f, size_kb))
}
message("\nNext: source('04_visualizations.R')\n")