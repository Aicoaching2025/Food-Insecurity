# =============================================================================
# 04_visualizations.R
# -----------------------------------------------------------------------------
# Project : Food Insecurity in the United States -- A Congressional Brief
# Author  : Candace Grant (Birds and Roses LLC)
#
# Build the 5 PNG charts that anchor the 5-slide Quarto reveal.js deck.
# Every chart pulls from one source of design truth: theme_congressional.R.
#
# Outputs in figures/:
#   01_problem_scale.png        -- Slide 1: 47.4M anchor + r=0.747 inset
#   02_trend_reversal.png       -- Slide 2: 2021 dip + 2024 peak
#   03_disease_burden.png       -- Slide 3: NHANES prevalence + cost
#   04_lifetime_cost.png        -- Slide 4: Hoynes lollipop chart
#   05_universal_meals_evidence.png  -- Slide 5: stat tiles + call to action
#
# Run after 03_analysis.R has produced data/processed/slide*.rds files.
#
# Image specs: 16:9 aspect ratio (12 x 6.75 inches at 300 DPI) so charts
# fill a reveal.js slide cleanly without scaling artifacts.
# =============================================================================


# ---- 0. SETUP ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)        # rich-text titles/subtitles with inline color
  library(ggrepel)       # smart label placement to avoid overlap
  library(scales)        # axis label formatters (percent, comma)
  library(patchwork)     # combining multiple ggplots into one figure
})

# Load the design system. This is the one source of truth for colors,
# typography, and chart styling. Every chart uses theme_congressional()
# and pulls colors from congressional_palette.
source(here::here("R", "theme_congressional.R"))

# Output directory.
fig_dir <- here::here("figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Standard slide dimensions: 16:9 aspect for reveal.js, 300 DPI.
SLIDE_WIDTH_IN  <- 12
SLIDE_HEIGHT_IN <- 6.75
SLIDE_DPI       <- 300

# Helper: save a ggplot at slide dimensions with consistent settings.
save_slide <- function(plot, filename, width = SLIDE_WIDTH_IN,
                       height = SLIDE_HEIGHT_IN) {
  ggsave(
    filename = file.path(fig_dir, filename),
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = SLIDE_DPI,
    bg       = "white"
  )
  message("  [OK] figures/", filename)
}

section <- function(title) {
  message("\n", strrep("=", 70))
  message(title)
  message(strrep("=", 70))
}


# ============================================================================
# SLIDE 1 -- THE PROBLEM: 47.4M + r=0.747
# ============================================================================
# 
# ============================================================================
# SLIDE 1 -- THE PROBLEM: 47.4M + r=0.747
# ============================================================================
section("[1/5] Slide 1 -- The Problem (47.4M + state correlation)")

slide1_data <- readRDS(here::here("data", "processed", "slide1_correlation.rds"))

# ---- LEFT PANEL: the headline number ---------------------------------------
# Split the big number across two lines so it fits comfortably in the panel.
left_panel <- ggplot() +
  xlim(0, 10) + ylim(0, 10) +
  annotate("text", x = 5, y = 8.0, label = "47.4",
           size = 28, family = "Palatino", fontface = "bold",
           color = congressional_palette$accent) +
  annotate("text", x = 5, y = 6.4, label = "million",
           size = 18, family = "Palatino", fontface = "bold",
           color = congressional_palette$accent) +
  annotate("text", x = 5, y = 5.0, label = "Americans live in",
           size = 7, family = "Palatino",
           color = congressional_palette$ink) +
  annotate("text", x = 5, y = 4.3, label = "food-insecure households",
           size = 7, family = "Palatino",
           color = congressional_palette$ink) +
  annotate("text", x = 5, y = 2.6, label = "1 in 5",
           size = 13, family = "Palatino", fontface = "bold",
           color = congressional_palette$accent) +
  annotate("text", x = 5, y = 1.6, label = "American children",
           size = 6, family = "Palatino",
           color = congressional_palette$muted) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20))

# ---- RIGHT PANEL: state-level scatter --------------------------------------
state_data <- slide1_data$state_data
fit_line   <- slide1_data$fit_line

# Use full state names so "LA" vs "CA" can't be misread.
states_to_label <- state_data |>
  dplyr::filter(state_abb %in% c("MS", "LA", "AR",
                                 "NH", "MN", "MA",
                                 "TX", "CA", "NY")) |>
  # Use full state name for label clarity
  dplyr::mutate(label = state_name)

right_panel <- ggplot(state_data,
                      aes(x = poverty_pct, y = food_insecure_pct)) +
  geom_line(data = fit_line,
            color = congressional_palette$accent,
            linewidth = 1.2, alpha = 0.7) +
  # All states as ghost points first.
  geom_point(color = congressional_palette$primary,
             size = 3, alpha = 0.65) +
  # Highlighted states get a larger coral dot so you can see the labeled point.
  geom_point(data = states_to_label,
             color = congressional_palette$accent,
             size = 4, alpha = 1) +
  # Labels with stronger connecting line so we always see the link.
  ggrepel::geom_text_repel(
    data = states_to_label,
    aes(label = label),
    family = "Palatino",
    fontface = "bold",
    size = 4.2,
    color = congressional_palette$ink,
    box.padding = 0.7,
    point.padding = 0.4,
    min.segment.length = 0,         # always draw the segment
    segment.color = congressional_palette$muted,
    segment.alpha = 0.8,
    segment.size = 0.4,
    seed = 42
  ) +
  scale_x_continuous(labels = scales::label_percent(scale = 1, suffix = "%"),
                     limits = c(7, 22), breaks = seq(8, 22, by = 4)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, suffix = "%"),
                     limits = c(6, 18), breaks = seq(6, 18, by = 3)) +
  labs(
    title = "Poverty drives food insecurity",
    subtitle = highlight_number(
      sprintf("r = %.2f", slide1_data$pearson_r),
      sprintf("&nbsp;&nbsp;State poverty explains %.0f%% of variance",
              100 * slide1_data$r_squared)
    ),
    x = "State poverty rate (ACS 2023)",
    y = "State food insecurity prevalence (USDA 2022-2024)",
    caption = src("USDA ERS; U.S. Census Bureau ACS 1-year (2023)")
  ) +
  theme_congressional()

slide1 <- left_panel + right_panel +
  patchwork::plot_layout(widths = c(1, 1.3)) +
  patchwork::plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )

save_slide(slide1, "01_problem_scale.png")


# ============================================================================
# SLIDE 2 -- IT'S GETTING WORSE: 2021 dip + 2024 peak
# ============================================================================
# Design rationale:
#   A single line chart from 2001 to 2024. Most of the line is ghosted in
#   gray (history) so the eye lands on the 2020-2024 segment in coral.
#   Three labeled points: 2021 trough ("we fixed it"), 2024 peak ("we
#   unfixed it"), and the +3.5 pp arrow between them.
# ============================================================================
# SLIDE 2 -- IT'S GETTING WORSE: 2021 dip + 2024 peak
# ============================================================================
section("[2/5] Slide 2 -- The Trend Reversal")

slide2 <- readRDS(here::here("data", "processed", "slide2_trend.rds"))
trend  <- slide2$trend_data

# Split into history vs recent. CRITICAL: include 2019 in BOTH subsets
# so the gray line and the coral line share a point, sealing any visual gap.
trend <- trend |>
  dplyr::mutate(period = ifelse(year >= 2019, "recent", "history"))

# Pre-2020 segment also needs 2019 for visual continuity.
history_segment <- trend |>
  dplyr::filter(year <= 2019)

# Recent segment is 2019 onward (so the lines share 2019).
recent_segment <- trend |>
  dplyr::filter(year >= 2019)

annotation_points <- trend |>
  dplyr::filter(!is.na(annotation))

p2 <- ggplot(trend, aes(x = year, y = food_insecure_pct)) +
  # History segment in ghost gray, ending at 2019.
  geom_line(data = history_segment,
            color = congressional_palette$ghost,
            linewidth = 1.2) +
  geom_point(data = history_segment,
             color = congressional_palette$ghost,
             size = 1.8) +
  # Recent segment in coral, starting at 2019 (overlap closes the visual gap).
  geom_line(data = recent_segment,
            color = congressional_palette$accent,
            linewidth = 1.8) +
  geom_point(data = recent_segment,
             color = congressional_palette$accent,
             size = 3.2) +
  # Annotated points (trough, peak, latest) -- white-fill rings for emphasis.
  geom_point(data = annotation_points,
             color = congressional_palette$accent,
             size = 5, shape = 21, fill = "white", stroke = 2.2) +
  # Trough label -- shifted up slightly so it doesn't get cut off at chart bottom.
  annotate("text",
           x = slide2$trough_year - 0.3,
           y = slide2$trough_pct - 0.55,
           label = sprintf("2021: %.1f%%\n\"we fixed it\"", slide2$trough_pct),
           family = "Palatino", fontface = "bold",
           color = congressional_palette$primary,
           size = 4.5, hjust = 1, vjust = 1, lineheight = 0.95) +
  # Latest label -- placed to the LEFT of the 2024 point, not the right.
  annotate("text",
           x = slide2$latest_year - 0.3,
           y = slide2$latest_pct + 0.6,
           label = sprintf("2024: %.1f%%\n\"we unfixed it\"", slide2$latest_pct),
           family = "Palatino", fontface = "bold",
           color = congressional_palette$accent,
           size = 4.5, hjust = 1, vjust = 0, lineheight = 0.95) +
  # Reversal arrow -- shorter and more vertical, leaving room for label on left.
  annotate("segment",
           x = slide2$trough_year + 0.5,
           xend = slide2$latest_year - 0.5,
           y = slide2$trough_pct + 0.4,
           yend = slide2$latest_pct - 0.4,
           arrow = arrow(length = unit(0.18, "inches"), type = "closed"),
           color = congressional_palette$accent, linewidth = 0.8) +
  # "+3.5 pp" label moved to the LEFT side of the arrow.
 
  scale_x_continuous(breaks = seq(2001, 2024, by = 4),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, suffix = "%"),
                     limits = c(8.5, 16),    # widened from 9 to 8.5 for label room
                     breaks = seq(9, 16, by = 1)) +
  labs(
    title = "The pandemic-era safety net worked. Then it expired.",
    subtitle = highlight_number(
      sprintf("+%.1f pp", slide2$reversal_pp),
      "rise in food insecurity from 2021 trough to 2024"
    ),
    x = NULL,
    y = "U.S. household food insecurity rate",
    caption = src("USDA ERS, Household Food Security in the United States in 2024 (ERR-358)")
  ) +
  theme_congressional()

save_slide(p2, "02_trend_reversal.png")


# ============================================================================
# SLIDE 3 -- THE DISEASE BURDEN: NHANES prevalence + Berkowitz cost
# ============================================================================
# Design rationale:
#   Grouped bar chart. Three outcomes (obesity, hypertension, diabetes),
#   each with two bars (food secure in teal, food insecure in coral).
#   Real NHANES numbers labeled on bars. A footnote acknowledges the
#   published literature gap (Option 4 from analysis decision).
#   Below the chart: the $52.9B/year cost callout.
section("[3/5] Slide 3 -- Disease Burden (NHANES + Berkowitz)")

slide3 <- readRDS(here::here("data", "processed", "slide3_prevalence.rds"))
prev   <- slide3$prevalence

# Order outcomes by ratio (largest disparity first).
outcome_order <- slide3$ratios |>
  dplyr::arrange(desc(ratio)) |>
  dplyr::pull(outcome)

prev <- prev |>
  dplyr::mutate(
    outcome = factor(outcome, levels = outcome_order),
    food_insecure = factor(food_insecure,
                           levels = c("Food secure", "Food insecure"))
  )

p3 <- ggplot(prev,
             aes(x = outcome, y = prevalence,
                 fill = food_insecure)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = scales::percent(prevalence, accuracy = 0.1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, family = "Palatino", fontface = "bold",
            size = 5,
            color = congressional_palette$ink) +
  scale_fill_manual(values = c("Food secure"  = "#5A9590",
                               "Food insecure" = "#EC8B6D")) +
  scale_y_continuous(labels = scales::label_percent(),
                     limits = c(0, 0.55),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Food-insecure adults face higher disease prevalence",
    subtitle = highlight_number(
      "$52.9 billion",
      "in excess U.S. healthcare costs every year (Berkowitz et al., 2018)"
    ),
    x = NULL,
    y = "Weighted prevalence among adults",
    ccaption = src(paste(
      "NHANES 2017-March 2020 pre-pandemic file (CDC NCHS).",
      "\nAnalysis applies MEC examination weights and NHANES sample design.",
      "\nPublished literature on older cycles reports larger disparities;",
      "the 2017-2020 cycle shows narrower differences."
    ))
  ) +
  theme_congressional() +
  theme(legend.position = "top",
        legend.justification = "left")

save_slide(p3, "03_disease_burden.png")


# ============================================================================
# SLIDE 4 -- THE LIFETIME COST: Hoynes effect sizes
# ============================================================================
# Design rationale:
#   Horizontal lollipop chart. All eight outcomes oriented the same direction
#   (always "this is an improvement" -- positive bars). Color-coded by
#   category (Health vs Economic). Each effect labeled with its magnitude.
section("[4/5] Slide 4 -- Lifetime Cost (Hoynes 2016)")

slide4_payload <- readRDS(here::here("data", "processed", "slide4_lifetime.rds"))
slide4 <- slide4_payload$data

# Orient effects so all bars point in the "good" direction.
# We display the absolute magnitude with a label showing the original signed
# direction (e.g., "-27% adult metabolic syndrome").
slide4 <- slide4 |>
  dplyr::mutate(
    label_with_sign = sprintf("%s%d%%",
                              ifelse(effect_pct >= 0, "+", "-"),
                              abs_effect),
    # Order by effect magnitude within category.
    outcome = factor(outcome, levels = outcome[order(category, abs_effect)])
  )

p4 <- ggplot(slide4,
             aes(x = abs_effect, y = outcome, color = category)) +
  geom_segment(aes(x = 0, xend = abs_effect, yend = outcome),
               linewidth = 1.2, alpha = 0.5) +
  geom_point(size = 5) +
  geom_text(aes(label = label_with_sign, x = abs_effect + 1.5),
            family = "Palatino", fontface = "bold",
            hjust = 0, size = 4.5,
            color = congressional_palette$ink) +
  scale_color_manual(values = c("Health"   = congressional_palette$primary,
                                "Economic" = congressional_palette$accent)) +
  scale_x_continuous(limits = c(0, 38),
                     breaks = seq(0, 35, by = 10),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Children with food assistance grow into healthier, more self-sufficient adults",
    subtitle = "Effect sizes vs. children without assistance, all statistically significant",
    x = "Magnitude of effect on adult outcomes",
    y = NULL,
    color = NULL,
    caption = src(paste(
      "Hoynes, Schanzenbach & Almond (2016). 'Long-Run Impacts of Childhood",
      "Access to the Safety Net.' American Economic Review 106(4): 903-934."
    ))
  ) +
  theme_congressional() +
  theme(legend.position = "top",
        legend.justification = "left",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = congressional_palette$grid,
                                          linewidth = 0.3))

save_slide(p4, "04_lifetime_cost.png")


# ============================================================================
# SLIDE 5 -- THE FIX: universal school meals natural experiment
# ============================================================================
# Design rationale:
#   Closing call to action. Four "stat tiles" arranged as a 2x2 grid showing
#   measured outcomes from the 8-state natural experiment. Below: closing
#   line "8 states acted. 42 are waiting." in large coral, the implicit ask.
section("[5/5] Slide 5 -- The Fix (universal meals)")

slide5_payload <- readRDS(here::here("data", "processed", "slide5_evidence.rds"))

# We curate four headline metrics for the tiles -- the most politically
# resonant subset of the full data.
tile_data <- tibble::tribble(
  ~metric,                       ~big_number,    ~direction, ~unit,         ~caption,
  "School lunch participation",  "+6%",          "increase", NA_character_, "more students fed daily",
  "Household food insecurity",   "-5.1%",        "decrease", NA_character_, "in families with children",
  "Childhood obesity",           "-0.6 pp",      "decrease", NA_character_, "in CEP schools (California)",
  "School suspensions",          "-0.7 pp",      "decrease", NA_character_, "in Oregon (2022-2023)"
) |>
  dplyr::mutate(
    x = c(0.25, 0.75, 0.25, 0.75),
    y = c(0.75, 0.75, 0.30, 0.30),
    color = ifelse(direction == "increase",
                   congressional_palette$primary,
                   congressional_palette$accent)
  )

# Build the tile chart as ggplot annotations on a 0-1 x 0-1 canvas.
p5 <- ggplot() +
  xlim(0, 1) + ylim(0, 1) +
  # Big numbers
  geom_text(data = tile_data,
            aes(x = x, y = y, label = big_number, color = I(color)),
            size = 18, family = "Palatino", fontface = "bold") +
  # Metric labels
  geom_text(data = tile_data,
            aes(x = x, y = y - 0.10, label = metric),
            size = 5, family = "Palatino", fontface = "bold",
            color = congressional_palette$ink) +
  # Captions
  geom_text(data = tile_data,
            aes(x = x, y = y - 0.14, label = caption),
            size = 4, family = "Palatino", fontface = "italic",
            color = congressional_palette$muted) +
  # Closing line at the bottom
  annotate("text", x = 0.5, y = 0.05,
           label = "8 states acted. 42 are waiting.",
           size = 11, family = "Palatino", fontface = "bold",
           color = congressional_palette$accent) +
  labs(
    title = "Universal school meals: measured results from 8 states",
    subtitle = "California, Maine, Colorado, Minnesota, New Mexico, Vermont, Massachusetts, Michigan",
    caption = src(paste(
      "FRAC, State of Healthy School Meals for All (2024); California Department of Education;",
      "Oregon Department of Education school discipline data (2022-2023)."
    ))
  ) +
  theme_congressional() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title.position = "plot"
  )

save_slide(p5, "05_universal_meals_evidence.png")


# ---- 6. SUMMARY -------------------------------------------------------------
message("\n", strrep("=", 70))
message("VISUALIZATIONS COMPLETE")
message(strrep("=", 70))
message("\nFiles in figures/:")
fig_files <- list.files(fig_dir, full.names = FALSE)
for (f in fig_files) {
  size_kb <- file.size(file.path(fig_dir, f)) / 1024
  message(sprintf("  %-40s %8.1f KB", f, size_kb))
}
message("\nNext: build presentation.qmd with these PNGs.\n")