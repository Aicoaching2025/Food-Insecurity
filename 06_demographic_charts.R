# =============================================================================
# 06_demographic_charts.R
# -----------------------------------------------------------------------------
# Project : Food Insecurity in the United States -- A Congressional Brief
# Author  : Candace Grant (Birds and Roses LLC)
#
#Three charts from the USDA ERS demographic prevalence table
# (insecurity.xlsx). All numbers are official USDA ERR-358 (2024) values.
#
#
# =============================================================================


# ---- 0. SETUP ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(ggplot2)
  library(ggtext)
  library(scales)
})

source(here::here("R", "theme_congressional.R"))

raw_dir  <- here::here("data", "raw")
proc_dir <- here::here("data", "processed")
fig_dir  <- here::here("figures")
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
if (!dir.exists(fig_dir))  dir.create(fig_dir,  recursive = TRUE)

xlsx_file <- file.path(raw_dir, "insecurity.xlsx")
if (!file.exists(xlsx_file)) {
  stop("File not found: ", xlsx_file,
       "\nPlease move insecurity.xlsx into data/raw/ before running.")
}

SLIDE_WIDTH_IN  <- 12
SLIDE_HEIGHT_IN <- 6.75
SLIDE_DPI       <- 300

save_slide <- function(plot, filename, width = SLIDE_WIDTH_IN,
                       height = SLIDE_HEIGHT_IN) {
  ggsave(filename = file.path(fig_dir, filename),
         plot = plot, width = width, height = height,
         dpi = SLIDE_DPI, bg = "white")
  message("  [OK] figures/", filename)
}

section <- function(title) {
  message("\n", strrep("=", 70))
  message(title)
  message(strrep("=", 70))
}


# ---- 1. LOAD AND CLEAN ------------------------------------------------------
# The Excel file has a one-row title before the actual headers, so skip = 1.
# It also has trailing rows with notes/source — we filter those out by
# keeping only rows where the percent columns are numeric.
section("[1/4] Load demographic data from insecurity.xlsx")

raw <- readxl::read_excel(xlsx_file, sheet = "Food insecurity", skip = 1)

# The original column names contain commas and spaces; rename for clarity.
demog <- raw |>
  dplyr::rename(
    category_type = `Type of household characteristic`,
    group         = `Household characteristic`,
    pct_2023      = `Percent of U.S. households in 2023`,
    pct_2024      = `Percent of U.S. households in 2024`,
    change_pp     = `Percentage point change from 2023 to 2024`,
    significant   = `Statistical significance indicator`
  ) |>
  # Drop the trailing notes/source rows (they have NA in the percent cols).
  dplyr::filter(!is.na(pct_2023), !is.na(group)) |>
  # Coerce to numeric (the Excel file sometimes brings them in as character).
  dplyr::mutate(
    pct_2023  = as.numeric(pct_2023),
    pct_2024  = as.numeric(pct_2024),
    change_pp = as.numeric(change_pp),
    # Fix typo in source data: "referece" appears twice instead of "reference"
    category_type = stringr::str_replace(category_type, "referece", "reference")
  )

message("Rows after cleaning: ", nrow(demog))
message("\nCategory types found:")
print(unique(demog$category_type))


# ---- 2. CHART 08: POVERTY GRADIENT ----------------------------------------
# Filter to the four poverty-ratio rows. Order them from poorest to richest
# so the bar chart reads left-to-right as a poverty gradient.
section("[2/4] Chart 08: Poverty gradient")

poverty_data <- demog |>
  dplyr::filter(category_type == "Household income-to-poverty ratio") |>
  dplyr::mutate(
    # Friendlier labels for the chart axis.
    group_label = dplyr::case_when(
      group == "Under 1.00"     ~ "Below\npoverty line",
      group == "Under 1.30"     ~ "Below 130%\nof poverty",
      group == "Under 1.85"     ~ "Below 185%\nof poverty",
      group == "1.85 and over"  ~ "At or above 185%\nof poverty",
      TRUE                      ~ group
    ),
    # Order from poorest to richest. We use a factor with explicit levels.
    group_label = factor(group_label,
                         levels = c("Below\npoverty line",
                                    "Below 130%\nof poverty",
                                    "Below 185%\nof poverty",
                                    "At or above 185%\nof poverty"))
  )

print(poverty_data |> dplyr::select(group, pct_2024))

# Compute the 5x gap for the headline subtitle.
below_poverty <- poverty_data$pct_2024[poverty_data$group == "Under 1.00"]
above_185 <- poverty_data$pct_2024[poverty_data$group == "1.85 and over"]
gap_ratio <- below_poverty / above_185
message(sprintf("\nBelow poverty: %.1f%% | Above 1.85x poverty: %.1f%% | Ratio: %.1fx",
                below_poverty, above_185, gap_ratio))

# Highlight the extremes (below poverty in coral, above 1.85x in muted).
poverty_data <- poverty_data |>
  dplyr::mutate(
    is_anchor = group %in% c("Under 1.00", "1.85 and over")
  )

p08 <- ggplot(poverty_data, aes(x = group_label, y = pct_2024,
                                fill = is_anchor)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct_2024)),
            vjust = -0.5,
            family = "Palatino", fontface = "bold",
            size = 6,
            color = congressional_palette$ink) +
  scale_fill_manual(
    values = c("TRUE" = congressional_palette$ghost,
               "FALSE" = "#A8AED9"),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1, suffix = "%"),
    limits = c(0, 47),
    breaks = seq(0, 40, by = 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "When poverty goes away, hunger nearly disappears",
    subtitle = highlight_number(
      sprintf("%.0fx", gap_ratio),
      sprintf("&nbsp;&nbsp;higher food insecurity below the poverty line (%.1f%%) vs above 185%% of poverty (%.1f%%)",
              below_poverty, above_185)
    ),
    x = "Household income relative to federal poverty line",
    y = "Food insecurity rate (2024)",
    caption = src("USDA ERS, Household Food Security in the United States in 2024 (ERR-358)")
  ) +
  theme_congressional() +
  theme(
    axis.text.x = element_text(size = ggplot2::rel(0.95), lineheight = 1.0)
  )

save_slide(p08, "08_poverty_gradient.png")


# ---- 3. CHART 09: WHO IS AFFECTED ------------------------------------------
# Horizontal bar chart of the most-affected demographic groups, sorted by
# 2024 prevalence. Compare against the U.S. average (13.7%) shown as a
# vertical reference line.
section("[3/4] Chart 09: Who is affected")

# Curate the groups for this chart: pick the most politically resonant
# disparities and skip the ones that don't tell a story (e.g., Other,
# Northeast, etc.).
who_groups <- c(
  "Female head, no spouse",
  "Male head, no spouse",
  "Black, non-Hispanic",
  "Hispanic",
  "With children < 18 years",
  "All households",
  "White, non-Hispanic"
)

who_data <- demog |>
  dplyr::filter(group %in% who_groups) |>
  dplyr::mutate(
    # Friendlier labels.
    group_label = dplyr::case_when(
      group == "Female head, no spouse"     ~ "Female-headed households",
      group == "Male head, no spouse"       ~ "Male-headed households",
      group == "Black, non-Hispanic"        ~ "Black households",
      group == "Hispanic"                   ~ "Hispanic households",
      group == "White, non-Hispanic"        ~ "White households",
      group == "With children < 18 years"   ~ "Households with children",
      group == "All households"             ~ "U.S. average (all households)",
      TRUE ~ group
    ),
    # Highlight everything except the average.
    is_average = group == "All households"
  ) |>
  # Sort by 2024 rate, descending.
  dplyr::arrange(pct_2024) |>
  dplyr::mutate(group_label = factor(group_label, levels = group_label))

us_avg <- demog$pct_2024[demog$group == "All households"]

p09 <- ggplot(who_data, aes(x = pct_2024, y = group_label,
                            fill = is_average)) +
  geom_col(width = 0.7) +
  # Reference line at the U.S. average.
  geom_vline(xintercept = us_avg,
             color = congressional_palette$muted,
             linetype = "dashed",
             linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", pct_2024)),
            hjust = -0.2,
            family = "Palatino", fontface = "bold",
            size = 5,
            color = congressional_palette$ink) +
  scale_fill_manual(
    values = c("TRUE" = congressional_palette$ghost,
               "FALSE" = "#9BC9C0"),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1, suffix = "%"),
    limits = c(0, 42),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Food insecurity is a story of who, not just how many",
    subtitle = highlight_number(
      "1 in 3",
      sprintf("&nbsp;&nbsp;female-headed households in America are food insecure &nbsp;\u2014&nbsp; vs %.1f%% nationally",
              us_avg)
    ),
    x = "Food insecurity rate (2024)",
    y = NULL,
    caption = src(paste(
      "USDA ERS, Household Food Security in the United States in 2024 (ERR-358).",
      "\nDashed line shows U.S. average rate."
    ))
  ) +
  theme_congressional() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = congressional_palette$grid,
                                      linewidth = 0.3),
    axis.text.y = element_text(size = ggplot2::rel(1.0))
  )

save_slide(p09, "09_who_is_affected.png")


# ---- 4. CHART 10: YEAR-OVER-YEAR CHANGES (2023 -> 2024) -------------------
# Lollipop chart showing which groups got worse from 2023 to 2024. Sort by
# magnitude. The deck's Slide 2 shows the trend reversal; this shows
# *which groups* absorbed it.
section("[4/4] Chart 10: Year-over-year change")

# Curate the groups with non-trivial changes.
yoy_groups <- c(
  "Female head, no spouse",
  "Male head, no spouse",
  "Black, non-Hispanic",
  "Hispanic",
  "With children < 18 years",
  "Men living alone",
  "Women living alone",
  "Adults > 65 living alone",
  "All households"
)

yoy_data <- demog |>
  dplyr::filter(group %in% yoy_groups) |>
  dplyr::mutate(
    group_label = dplyr::case_when(
      group == "Female head, no spouse"     ~ "Female-headed households",
      group == "Male head, no spouse"       ~ "Male-headed households",
      group == "Black, non-Hispanic"        ~ "Black households",
      group == "Hispanic"                   ~ "Hispanic households",
      group == "White, non-Hispanic"        ~ "White households",
      group == "With children < 18 years"   ~ "Households with children",
      group == "Men living alone"           ~ "Men living alone",
      group == "Women living alone"         ~ "Women living alone",
      group == "Adults > 65 living alone"   ~ "Adults 65+ living alone",
      group == "All households"             ~ "U.S. average",
      TRUE ~ group
    ),
    direction = ifelse(change_pp >= 0, "Worse", "Improved"),
    is_average = group == "All households"
  ) |>
  dplyr::arrange(change_pp) |>
  dplyr::mutate(group_label = factor(group_label, levels = group_label))

p10 <- ggplot(yoy_data, aes(x = change_pp, y = group_label, color = direction)) +
  # Vertical reference line at zero.
  geom_vline(xintercept = 0, color = congressional_palette$muted,
             linewidth = 0.5) +
  # Lollipop "stem" from zero to the value.
  geom_segment(aes(x = 0, xend = change_pp, yend = group_label),
               linewidth = 1.2) +
  # Lollipop "head".
  geom_point(size = 5) +
  # Direct label of the change.
  geom_text(aes(label = sprintf("%+.1f pp", change_pp),
                x = change_pp + ifelse(change_pp >= 0, 0.15, -0.15),
                hjust = ifelse(change_pp >= 0, 0, 1)),
            family = "Palatino", fontface = "bold",
            size = 4.5,
            color = congressional_palette$ink) +
  scale_color_manual(
    values = c("Worse"    = congressional_palette$accent,
               "Improved" = congressional_palette$primary),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = function(x) sprintf("%+.1f pp", x),
    limits = c(-2.5, 3.5)
  ) +
  labs(
    title = "Female-headed households absorbed the largest one-year increase",
    subtitle = highlight_number(
      "+2.1 pp",
      "&nbsp;&nbsp;rise in food insecurity among female-headed households in a single year (2023 to 2024)"
    ),
    x = "Change in food insecurity rate, 2023 to 2024",
    y = NULL,
    caption = src(paste(
      "USDA ERS, Household Food Security in the United States in 2024 (ERR-358).",
      "\nNo changes were statistically significant at p < 0.10; substantive magnitudes shown."
    ))
  ) +
  theme_congressional() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = congressional_palette$grid,
                                      linewidth = 0.3)
  )

save_slide(p10, "10_year_over_year.png")


# ---- 5. SAVE PROCESSED DATA ------------------------------------------------
# Save the cleaned tibble for any downstream use or audit.
saveRDS(demog, file.path(proc_dir, "usda_demographics_clean.rds"))


# ---- 6. SUMMARY -------------------------------------------------------------
message("\n", strrep("=", 70))
message("DEMOGRAPHIC CHARTS COMPLETE")
message(strrep("=", 70))
message("\nThree new figures in figures/:")
message("  08_poverty_gradient.png")
message("  09_who_is_affected.png")
message("  10_year_over_year.png")
message("\nUse the strongest 1-2 in the deck; reference the rest in the README.\n")