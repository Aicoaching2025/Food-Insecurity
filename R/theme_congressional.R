# =============================================================================
# R/theme_congressional.R
# -----------------------------------------------------------------------------
# A custom ggplot2 theme + color palette for the 5-slide Congressional brief.
#
# Design rationale:
#   - Audience is political (staffers, members, lobbyists) -- the visual
#     vocabulary is reports and one-pagers, not journal figures.
#   - One argument per chart. Ghost the supporting data, highlight the
#     focal data. The reader's eye should land on exactly one thing first.
#   - Palatino for titles (gravitas without preciousness; reads as
#     "considered" without reading as "academic").
#   - Source citation in every footer. Non-negotiable for political audiences
#     -- they need to know the number didn't come from us, it came from
#     USDA / Census / CDC.
#
# Usage:
#   source("R/theme_congressional.R")
#   ggplot(...) + theme_congressional() + scale_color_congressional()
# =============================================================================


# ---- 1. PALETTE -------------------------------------------------------------
# A small, deliberate palette. Three roles for color:
#   PRIMARY    deep teal     -- "this is the data; trust it"
#   ACCENT     warm coral    -- "look here; this is the point"
#   GHOST      pale gray     -- "this is context, not the argument"
#
# Plus four utility shades:
#   INK        charcoal      -- body text
#   PAPER      off-white     -- background for chart-on-card layouts
#   MUTED      slate         -- footer / source citation text
#   GRID       very pale     -- axis grid lines that don't compete

congressional_palette <- list(
  primary  = "#0F766E",   # deep teal -- defensible, calm, federal-document feel
  accent   = "#E76F51",   # warm coral -- pops without screaming; not red
  ghost    = "#D4D4D4",   # neutral gray for de-emphasized data
  ink      = "#1F2937",   # near-black charcoal for body text
  paper    = "#FAFAF9",   # off-white background
  muted    = "#6B7280",   # slate for source/footer
  grid     = "#E5E7EB"    # pale gray for grid lines
)


# ---- 2. THE THEME -----------------------------------------------------------
# A theme function that takes one argument (base_size) and returns a ggplot2
# theme object. Standard pattern from the ggplot2 ecosystem.
theme_congressional <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Typography. Palatino isn't always present; fall back to serif.
      text             = ggplot2::element_text(family = "Palatino",
                                               color  = congressional_palette$ink),
      plot.title       = ggplot2::element_text(family = "Palatino",
                                               face   = "bold",
                                               size   = ggplot2::rel(1.4),
                                               margin = ggplot2::margin(b = 4)),
      plot.subtitle    = ggtext::element_markdown(family = "Palatino",
                                                  size   = ggplot2::rel(1.05),
                                                  color  = congressional_palette$muted,
                                                  margin = ggplot2::margin(b = 16)),
      plot.caption     = ggplot2::element_text(family = "Palatino",
                                               size   = ggplot2::rel(0.75),
                                               color  = congressional_palette$muted,
                                               hjust  = 0,
                                               margin = ggplot2::margin(t = 12)),
      plot.caption.position = "plot",   # caption aligns to plot, not panel
      plot.title.position   = "plot",   # title aligns to plot, not panel

      # Axes. Minimalist -- no chartjunk.
      axis.title.x     = ggplot2::element_text(size   = ggplot2::rel(0.9),
                                               color  = congressional_palette$muted,
                                               margin = ggplot2::margin(t = 8)),
      axis.title.y     = ggplot2::element_text(size   = ggplot2::rel(0.9),
                                               color  = congressional_palette$muted,
                                               margin = ggplot2::margin(r = 8)),
      axis.text        = ggplot2::element_text(color = congressional_palette$ink),
      axis.ticks       = ggplot2::element_blank(),

      # Grid. Horizontal only; minor grid off; very subtle.
      panel.grid.major.y = ggplot2::element_line(color    = congressional_palette$grid,
                                                 linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),

      # Background. Transparent so charts can sit on slide backgrounds cleanly.
      panel.background = ggplot2::element_rect(fill  = NA, color = NA),
      plot.background  = ggplot2::element_rect(fill  = NA, color = NA),

      # Legend. Top-left, no title (the colors are explained in the subtitle).
      legend.position  = "top",
      legend.justification = "left",
      legend.title     = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = ggplot2::rel(0.85)),

      # Strip text (for facets) -- bold but not loud.
      strip.text       = ggplot2::element_text(face = "bold",
                                               size = ggplot2::rel(0.95),
                                               hjust = 0),

      # Margins. Generous so titles breathe.
      plot.margin      = ggplot2::margin(20, 20, 20, 20)
    )
}


# ---- 3. SCALE HELPERS -------------------------------------------------------
# Convenience wrappers so we never hand-type hex codes in the chart code.
# This is what keeps the design system coherent: every chart pulls from these.

# For categorical fills/colors when we have 1 highlighted + everything-else.
# Use with: scale_fill_congressional(highlight = TRUE)
scale_fill_congressional <- function(highlight = FALSE) {
  if (highlight) {
    ggplot2::scale_fill_manual(values = c("TRUE"  = congressional_palette$accent,
                                          "FALSE" = congressional_palette$ghost),
                               guide  = "none")
  } else {
    ggplot2::scale_fill_manual(values = c(congressional_palette$primary,
                                          congressional_palette$accent,
                                          congressional_palette$muted))
  }
}

scale_color_congressional <- function(highlight = FALSE) {
  if (highlight) {
    ggplot2::scale_color_manual(values = c("TRUE"  = congressional_palette$accent,
                                           "FALSE" = congressional_palette$ghost),
                                guide  = "none")
  } else {
    ggplot2::scale_color_manual(values = c(congressional_palette$primary,
                                           congressional_palette$accent,
                                           congressional_palette$muted))
  }
}

# A standard footer text for every chart. Takes a source string and returns
# a properly-formatted caption. Use as: labs(caption = src("USDA ERS, 2024"))
src <- function(source_text) {
  paste0("Source: ", source_text)
         
}

# Helper for headline annotations (large coral numbers in the subtitle).
# Use ggtext markdown so we get inline color spans.
# Example:
#   subtitle = highlight_number("47.4M", "Americans live in food-insecure households")
highlight_number <- function(number, label) {
  paste0("<span style='color:", congressional_palette$accent, ";font-weight:bold;'>",
         number, "</span> ", label)
}