# app.R  ---- Single-file Shiny app (no external data) ----
# Polished underwriting tool: why premium index < 100% even with flat ROL

# ==============================================================================
# LLOYD'S RATE CHANGE REQUIREMENTS SUMMARY
# ==============================================================================
#
# Source: "Instructions for what is needed wit.txt"
#
# KEY CONCEPTS:
# -------------
# 1. RARC (Risk Adjusted Rate Change):
#    - Measures price difference between current year charge vs. what would have
#      been charged LAST YEAR for the EXACT SAME RISK
#    - Net of claims inflation to show true impact on expected loss ratios
#    - The standard metric Lloyd's uses to track "more or less money for same risk"
#
# 2. Benchmark Price:
#    - Theoretical gross premium required to achieve the loss ratio set in the
#      approved Syndicate Business Forecast (SBF)
#
# 3. Price Adequacy:
#    - Comparison of achieved price against benchmark price
#    - Ensures business written will meet profitability targets
#
# PMDR (Performance Management Data Return) - 4 COMPONENTS:
# ---------------------------------------------------------
# Syndicates must submit monthly PMDR breaking premium movements into:
#
#   Component 1: Change in Deductible / Attachment Point
#                - Premium impact from shifts in risk retention level
#
#   Component 2: Change in Breadth of Cover
#                - Premium movement from adding/removing perils
#                  (e.g., adding cyber or piracy coverage)
#
#   Component 3: Other Factors
#                - Changes in limits, exposure units (e.g., more ships, higher turnover),
#                  or geographic shifts
#
#   Component 4: Pure Rate Change (THE RARC)
#                - The "residual" movement - this IS the RARC
#                - Price change NOT explained by exposure adjustments above
#
# ==============================================================================

# Print summary to console on app load
message("
================================================================================
LLOYD'S RATE CHANGE APP - Requirements Summary
================================================================================
RARC = Risk Adjusted Rate Change (price change for SAME risk, net of inflation)

PMDR Decomposition:
  1. Deductible/Attachment change

  2. Breadth of Cover change (perils)
  3. Other Factors (limits, exposure units, geography)
  4. Pure Rate Change = RARC (the residual)

This app will implement Lloyd's-style waterfall decomposition for satellite
layer pricing, separating exposure-driven changes from pure rate movement.
================================================================================
")

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

# --------------------------
# Helpers
# --------------------------

# --------------------------
# Exposure curve (MBBEFD / Swiss Re)
# --------------------------
has_mbbefd <- function() requireNamespace("mbbefd", quietly = TRUE)

G_mbbefd <- function(x, c) {
  # x in [0,1]
  x <- pmax(0, pmin(1, x))

  if (has_mbbefd()) {
    pars <- mbbefd::swissRe(as.numeric(c))  # returns (b,g)
    # ecMBBEFD is the exposure curve function G(x)
    return(mbbefd::ecMBBEFD(x, b = pars[1], g = pars[2]))
  }

  # Fallback (NOT MBBEFD): uniform exposure curve from vignette: G(d)=d(2-d)  :contentReference[oaicite:1]{index=1}
  x * (2 - x)
}

layer_share_from_curve <- function(A, L, MPL, c) {
  if (!is.finite(MPL) || MPL <= 0 || !is.finite(L) || L <= 0) {
    return(list(d = NA_real_, u = NA_real_, Gd = NA_real_, Gu = NA_real_, share = 0))
  }
  d <- A / MPL
  u <- (A + L) / MPL
  # clamp into [0,1]
  d <- pmax(0, pmin(1, d))
  u <- pmax(0, pmin(1, u))
  G_d <- G_mbbefd(d, c)
  G_u <- G_mbbefd(u, c)
  list(d = d, u = u, Gd = G_d, Gu = G_u, share = pmax(G_u - G_d, 0))
}

curve_allocation_and_q <- function(values, A, L, c, q_gu) {
  shares <- vapply(values, function(MPL_i) layer_share_from_curve(A, L, MPL_i, c)$share, numeric(1))
  # Allocation weight proportional to share × value (severity base)
  raw <- shares * pmax(values, 0)
  w_curve <- if (sum(raw) > 0) raw / sum(raw) else rep(0, length(values))
  q_layer <- q_gu * shares
  list(shares = shares, w_curve = w_curve, q_layer = q_layer)
}

alloc_from_curve <- function(values, A, L, c) {
  # curve-implied allocation across satellites based on each satellite’s MPL = value
  # allocation_i ∝ (G(u_i) - G(d_i)) * value_i  (value scales the severity base)
  parts <- vapply(values, function(MPL_i) {
    out <- layer_share_from_curve(A, L, MPL_i, c)
    if (is.na(out$share)) return(0)
    pmax(out$share, 0)
  }, numeric(1))

  raw <- parts * pmax(values, 0)
  if (sum(raw) <= 0) return(rep(0, length(values)))
  raw / sum(raw)
}

alloc_shift_pct <- function(w0, w1) 0.5 * sum(abs(w1 - w0))

fmt_money <- function(x) dollar(x, accuracy = 1, big.mark = ",", prefix = "£")
fmt_bn <- function(x) paste0("£", comma(x / 1e9, accuracy = 0.001), "bn")
fmt_mn <- function(x) paste0("£", comma(x / 1e6, accuracy = 0.01), "m")
fmt_pct <- function(x, digits = 1) percent(x, accuracy = 10^(-digits))

clamp01 <- function(x) pmax(0, pmin(1, x))

interp_vec <- function(v_prior, v_curr, t01) {
    (1 - t01) * v_prior + t01 * v_curr
}

# "Tower top" assumption: max satellite value (post-dep)
limit_from_values <- function(values, attachment) {
    pmax(max(values) - attachment, 0)
}

value_in_layer <- function(values, attachment, limit) {
    # Coherent simplification:
    # For each satellite i: exposure that *could* sit in the layer, capped by contract limit.
    pmax(pmin(values - attachment, limit), 0)
}

premium_gross <- function(rol, limit) rol * limit
premium_net <- function(gross, brokerage) gross * (1 - brokerage)

expected_loss <- function(q_layer, vil) sum(q_layer * vil)

implied_lr <- function(el, net_prem) ifelse(net_prem <= 0, NA_real_, el / net_prem)

tip <- function(label, text) {
    shiny::tags$span(
        label,
        title = text,
        `data-bs-toggle` = "tooltip",
        `data-bs-placement` = "bottom",
        style = "text-decoration: underline dotted; text-underline-offset: 3px; cursor: help;"
    )
}

# --------------------------
# Defaults provided
# --------------------------
sat_names <- paste0("V", 1:4)
base_values <- c(6509542466, 6709868493, 6218354896, 8458413699)

q_gu <- c(
  0.0010601787456726477,
  0.0009041025302225488,
  0.0009040826278695704,
  0.006066660583587538
)

# ==============================================================================
# C-PARAMETER LOOKUP FROM CSV
# ==============================================================================
# Load the c-parameters CSV at startup (once, not reactive)
# CSV columns: c (Swiss Re curve parameter), expectation (expected loss ratio)
#
# MAPPING LOGIC:
# - Each satellite has a failure rate q_gu_i
# - We interpret failure rate as an "expectation" measure
# - Find the c value that corresponds to this expectation via interpolation
# - Use linear interpolation between the two nearest points in the CSV
# ==============================================================================

# Read CSV at startup
c_params_csv <- tryCatch(
    read.csv("c parameters.csv", stringsAsFactors = FALSE),
    error = function(e) {
        message("Warning: Could not read 'c parameters.csv'. Using fallback.")
        data.frame(c = c(0, 5, 10), expectation = c(1, 0.2, 0.001))
    }
)

# Clean column names (handle BOM if present)
names(c_params_csv) <- gsub("^\\W+", "", names(c_params_csv))

# Ensure sorted by expectation (descending) for interpolation
c_params_csv <- c_params_csv[order(c_params_csv$expectation, decreasing = TRUE), ]

#' Map a failure rate (expectation) to a c parameter using linear interpolation
#' @param failure_rate Numeric: the ground-up failure rate (q_gu) for a satellite
#' @param lookup_df Data frame with columns 'c' and 'expectation'
#' @return Numeric: the interpolated c value
#' @details Uses linear interpolation. If failure_rate is outside the CSV range,
#'          returns the boundary c value (clamped).
map_failure_rate_to_c <- function(failure_rate, lookup_df = c_params_csv) {
    # Handle edge cases

if (is.na(failure_rate) || !is.finite(failure_rate)) return(NA_real_)

    # Get expectation and c columns
    exp_col <- lookup_df$expectation
    c_col <- lookup_df$c

    # Clamp to CSV range
    min_exp <- min(exp_col)
    max_exp <- max(exp_col)

    if (failure_rate >= max_exp) {
        # Higher than max expectation -> lowest c
        return(c_col[which.max(exp_col)])
    }
    if (failure_rate <= min_exp) {
        # Lower than min expectation -> highest c
        return(c_col[which.min(exp_col)])
    }

    # Linear interpolation using approx()
    # Note: CSV is sorted descending by expectation, but approx needs increasing x
    # So we'll use the c values sorted by expectation ascending
    sorted_idx <- order(exp_col)
    approx(
        x = exp_col[sorted_idx],
        y = c_col[sorted_idx],
        xout = failure_rate,
        method = "linear",
        rule = 2  # Clamp to boundary values
    )$y
}

#' Compute per-satellite c values from failure rates
#' @param q_gu_vec Vector of failure rates
#' @return Vector of c values (same length as input)
compute_per_satellite_c <- function(q_gu_vec) {
    vapply(q_gu_vec, map_failure_rate_to_c, numeric(1))
}

#' Compute portfolio-level c as value-weighted average of per-satellite c
#' @param q_gu_vec Vector of failure rates
#' @param values_vec Vector of satellite values (for weighting)
#' @return Numeric: single portfolio-level c
#' @details ASSUMPTION: Portfolio c = sum(c_i * value_i) / sum(value_i)
#'          This is a simplification; per-satellite c would be more accurate
#'          but requires more invasive changes to allocation functions.
compute_portfolio_c <- function(q_gu_vec, values_vec) {
    c_per_sat <- compute_per_satellite_c(q_gu_vec)
    weights <- values_vec / sum(values_vec)
    sum(c_per_sat * weights, na.rm = TRUE)
}

# Log the CSV loading
message(sprintf(
    "C-parameters CSV loaded: %d rows, c range [%.2f, %.2f], expectation range [%.6f, %.6f]",
    nrow(c_params_csv),
    min(c_params_csv$c), max(c_params_csv$c),
    min(c_params_csv$expectation), max(c_params_csv$expectation)
))

# --------------------------
# UI
# --------------------------
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly"
)

ui <- page_sidebar(
    title = "Satellite Layer Pricing Story",
    theme = theme,
    sidebar = sidebar(
        width = 420,
        card(
            card_header(tagList(icon("sliders"), "Deal Inputs")),
            layout_column_wrap(
                width = 1 / 2,
                switchInput("mode", label = "Sensitivity mode", value = TRUE, onLabel = "On", offLabel = "Off"),
                switchInput("unlock_rol", label = "Unlock ROL", value = FALSE, onLabel = "Unlocked", offLabel = "Fixed")
            ),
            tags$hr(),
            h6("Portfolio / Schedule"),
            DTOutput("schedule_tbl"),
            tags$hr(),
            h6("Pricing terms"),
            sliderInput("rol", "Rate on Line (ROL)", min = 0.0025, max = 0.03, value = 0.0105, step = 0.0001),
            sliderInput("brokerage", "Brokerage", min = 0, max = 0.30, value = 0.14, step = 0.005),
            sliderInput("target_lr", "Target loss ratio", min = 0.20, max = 0.80, value = 0.45, step = 0.01),
            tags$hr(),
            h6("Layer terms"),
            sliderInput("excess_prior", "Prior excess (attachment)", min = 0, max = 8e9, value = 5e9, step = 0.1e9),
            sliderInput("excess_curr", "Current excess (attachment)", min = 0, max = 8e9, value = 5.5e9, step = 0.1e9),
            tags$hr(),
            h6("Value depreciation (Current)"),
            sliderInput("dep_curr", "Depreciation", min = -0.40, max = 0, value = -0.15, step = 0.01),
            tags$hr(),
            h6("Exposure curve / layer allocation"),
            tags$hr(),
            h6("Exposure curve (Swiss Re / MBBEFD)"),
            selectInput(
                "c_mode",
                tip("C-parameter mode", "Auto: derive c from failure rates via CSV lookup. Manual: use dropdown."),
                choices = c("Auto from CSV" = "auto", "Manual" = "manual"),
                selected = "manual"
            ),
            conditionalPanel(
                condition = "input.c_mode == 'manual'",
                selectInput(
                    "sr_c",
                    "Swiss Re curve (c)",
                    choices = c("Y1 (c=1.5)" = 1.5, "Y2 (c=2)" = 2, "Y3 (c=3)" = 3, "Y4 (c=4)" = 4, "Lloyd's-style (c=5)" = 5),
                    selected = 4
                )
            ),
            conditionalPanel(
                condition = "input.c_mode == 'auto'",
                tags$div(
                    style = "font-size: 0.85rem; color: #6c757d; padding: 0.5rem; background: rgba(0,0,0,0.03); border-radius: 0.5rem;",
                    tags$p(style = "margin-bottom: 0.25rem;", tags$b("Auto c-parameter:")),
                    tags$p(style = "margin-bottom: 0;",
                        "Portfolio c is computed as value-weighted average of per-satellite c values, ",
                        "where each c_i is derived from q_gu_i via linear interpolation in the CSV."
                    ),
                    uiOutput("auto_c_display")
                )
            ),
            layout_column_wrap(
                width = 1 / 2,
                actionButton("reset_prior", "Reset to prior", class = "btn btn-outline-primary w-100"),
                actionButton("reset_current", "Reset to current", class = "btn btn-primary w-100")
            )
        ),
        accordion(
            accordion_panel(
                "Actuarial detail (formulas & assumptions)",
                tags$div(
                    style = "font-size: 0.95rem;",
                    tags$h6("Value update"),
                    tags$pre("V_new = V_old × (1 + dep)\n(dep = 0% for Prior; slider for Current)"),
                    tags$h6("Tower / limit assumption (transparent)"),
                    tags$pre("Tower top = max(V_i post-dep)\nLimit = max(V_i) − Attachment, floored at 0"),
                    tags$h6("Value in layer"),
                    tags$pre("ValueInLayer_i = max( min(V_i − Attachment, Limit), 0 )"),
                    tags$h6("Premium"),
                    tags$pre("Gross Premium = ROL × Limit\nNet Premium = Gross × (1 − Brokerage)"),
                    tags$h6("Expected loss"),
                    tags$pre("EL_i = q_i × w_i × ValueInLayer_i\nTotal EL = Σ EL_i"),
                    tags$h6("Implied LR"),
                    tags$pre("LR = EL / NetPremium\nDistance to target = LR − TargetLR")
                )
            ),
            open = FALSE
        ),
        # ==================================================================
        # LLOYD'S RATE CHANGE: Prior Year Inputs (optional)
        # ==================================================================
        accordion(
            id = "lloyds_accordion",
            accordion_panel(
                title = tagList(icon("clock-rotate-left"), " Prior Year (optional)"),
                tags$div(
                    style = "font-size: 0.92rem;",
                    tags$p(
                        style = "color: #6c757d; margin-bottom: 0.75rem;",
                        "For Lloyd's RARC calculation: capture last year's pricing inputs."
                    ),
                    # Prior ROL
                    sliderInput(
                        "prior_rol",
                        tip("Prior Rate on Line", "Last year's ROL for RARC comparison"),
                        min = 0.0025, max = 0.03, value = 0.0105, step = 0.0001
                    ),
                    tags$hr(),
                    # Prior Failure Rates toggle + table
                    switchInput(
                        "use_prior_failure_rates",
                        label = "Use prior failure rates",
                        value = FALSE,
                        onLabel = "Yes",
                        offLabel = "No",
                        size = "small"
                    ),
                    conditionalPanel(
                        condition = "input.use_prior_failure_rates == true",
                        tags$div(
                            style = "margin-top: 0.5rem;",
                            tags$small(
                                style = "color: #6c757d;",
                                "Edit prior year ground-up failure rates (q_gu):"
                            ),
                            DTOutput("prior_q_gu_tbl", height = "auto")
                        )
                    ),
                    tags$hr(),
                    # Prior Allocation toggle + table
                    switchInput(
                        "use_prior_allocation",
                        label = "Use manual prior allocation",
                        value = FALSE,
                        onLabel = "Yes",
                        offLabel = "No",
                        size = "small"
                    ),
                    conditionalPanel(
                        condition = "input.use_prior_allocation == true",
                        tags$div(
                            style = "margin-top: 0.5rem;",
                            tags$small(
                                style = "color: #6c757d;",
                                "Edit prior year layer allocation weights (must sum to 1):"
                            ),
                            DTOutput("prior_alloc_tbl", height = "auto"),
                            uiOutput("prior_alloc_validation")
                        )
                    )
                )
            ),
            accordion_panel(
                title = tagList(icon("calculator"), " Rerated View (current method)"),
                tags$div(
                    style = "font-size: 0.92rem;",
                    tags$p(
                        style = "color: #6c757d; margin-bottom: 0.75rem;",
                        "Prior exposure re-rated using current methodology."
                    ),
                    # Rerated failure rates toggle + table
                    switchInput(
                        "use_rerated_manual_rates",
                        label = "Override rerated failure rates",
                        value = FALSE,
                        onLabel = "Yes",
                        offLabel = "No",
                        size = "small"
                    ),
                    conditionalPanel(
                        condition = "input.use_rerated_manual_rates == true",
                        tags$div(
                            style = "margin-top: 0.5rem;",
                            tags$small(
                                style = "color: #6c757d;",
                                "Edit failure rates for rerated view (default = current q_gu):"
                            ),
                            DTOutput("rerated_q_gu_tbl", height = "auto")
                        )
                    ),
                    tags$hr(),
                    # Rerated allocation - MBBEFD by default
                    switchInput(
                        "use_rerated_manual_alloc",
                        label = "Override rerated allocation",
                        value = FALSE,
                        onLabel = "Manual",
                        offLabel = "MBBEFD",
                        size = "small"
                    ),
                    tags$small(
                        style = "color: #6c757d;",
                        "Layer allocation: Uses MBBEFD curve by default."
                    ),
                    conditionalPanel(
                        condition = "input.use_rerated_manual_alloc == true",
                        tags$div(
                            style = "margin-top: 0.5rem;",
                            tags$small(
                                style = "color: #6c757d;",
                                "Manual rerated allocation weights (must sum to 1):"
                            ),
                            DTOutput("rerated_alloc_tbl", height = "auto"),
                            uiOutput("rerated_alloc_validation")
                        )
                    )
                )
            ),
            accordion_panel(
                title = tagList(icon("chart-waterfall"), " PMDR Adjustments"),
                tags$div(
                    style = "font-size: 0.92rem;",
                    tags$p(
                        style = "color: #6c757d; margin-bottom: 0.75rem;",
                        "Lloyd's PMDR manual adjustments for breadth of cover and other factors."
                    ),
                    # Breadth of cover change (user input)
                    sliderInput(
                        "breadth_of_cover_change",
                        tip("Breadth of Cover Change", "Premium impact from adding/removing perils (e.g., cyber, piracy). Enter as £ delta."),
                        min = -5e6, max = 5e6, value = 0, step = 1e4,
                        pre = "£"
                    ),
                    tags$hr(),
                    # Other exposure change (user input)
                    sliderInput(
                        "other_exposure_change",
                        tip("Other Exposure Change", "Premium impact from limits, exposure units, geography shifts. Enter as £ delta."),
                        min = -5e6, max = 5e6, value = 0, step = 1e4,
                        pre = "£"
                    ),
                    tags$hr(),
                    tags$p(
                        style = "color: #6c757d; font-size: 0.85rem;",
                        "Note: Deductible/attachment change is computed automatically. ",
                        "Pure Rate Change (RARC) is the residual after all other components."
                    )
                )
            ),
            accordion_panel(
                title = tagList(icon("table"), " Lloyd's Decomposition (verification)"),
                tags$div(
                    style = "font-size: 0.92rem;",
                    tags$p(
                        style = "color: #6c757d; margin-bottom: 0.75rem;",
                        "Numerical verification of PMDR decomposition and RARC calculation."
                    ),
                    uiOutput("lloyds_decomp_table")
                )
            ),
            open = FALSE
        )
    ),
    useShinyjs(),
    tags$head(
        tags$script(HTML("
        document.addEventListener('shown.bs.modal', () => {
        [...document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')].forEach(el => new bootstrap.Tooltip(el));
        });
        document.addEventListener('DOMContentLoaded', () => {
        [...document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')].forEach(el => new bootstrap.Tooltip(el));
        });
    "))
    ),
    # Main content
    layout_column_wrap(
        width = 1,
        gap = "1rem",

        # KPI row
        layout_column_wrap(
            width = 1 / 4,
            gap = "1rem",
            value_box(
                title = tip("Total TIV", "Sum of satellite schedule values (post-dep in each scenario)."),
                value = uiOutput("kpi_tiv"),
                showcase = icon("satellite-dish"),
                theme_color = "primary"
            ),
            value_box(
                title = tip("Layer Limit (actual)", "Limit derived from max value minus attachment (assumption shown in drawer)."),
                value = uiOutput("kpi_limit"),
                showcase = icon("layer-group"),
                theme_color = "info"
            ),
            value_box(
                title = tip("Premium (gross / net)", "Gross = ROL×Limit; Net = Gross×(1−brokerage)."),
                value = uiOutput("kpi_prem"),
                showcase = icon("coins"),
                theme_color = "success"
            ),
            value_box(
                title = tip("Expected Loss", "Σ(q_i × w_i × ValueInLayer_i)."),
                value = uiOutput("kpi_el"),
                showcase = icon("triangle-exclamation"),
                theme_color = "warning"
            )
        ),

        # Center visuals
        layout_column_wrap(
            width = 1 / 2,
            gap = "1rem",
            card(
                full_screen = TRUE,
                card_header(tagList(icon("chart-bar"), "Pricing & allocation bridge (percent)")),

                navset_tab(
                    nav_panel(
                    "Percent bridge",
                    plotlyOutput("pct_bridge", height = 360),
                    tags$div(
                        style = "font-size:0.95rem; color:#5a6b7b;",
                        "Compares booked premium base (limit) change vs allocation shift and EL change."
                    )
                    ),
                    nav_panel(
                    "MBBEFD exposure curve",
                    plotlyOutput("ec_curve", height = 360),
                    tags$div(
                        style = "font-size:0.95rem; color:#5a6b7b;",
                        "Two points per scenario: (Attachment/MPL, G(d)) and ((Attachment+Limit)/MPL, G(u)). ",
                        "Layer share is G(u)−G(d)."
                    )
                    )
                )
                ),
            card(
                full_screen = TRUE,
                card_header(tagList(icon("gauge-high"), "Rate change index")),
                plotlyOutput("gauge", height = 270)
            )
        ),

        # Mechanics + risk view
        layout_column_wrap(
            width = 1 / 2,
            gap = "1rem",
            card(
                full_screen = TRUE,
                card_header(tagList(icon("wand-magic-sparkles"), "Mechanics (auto-updating story)")),
                uiOutput("mechanics"),
                tags$hr(),
                uiOutput("dominant_driver")
            ),
            card(
                full_screen = TRUE,
                card_header(tagList(icon("heart-pulse"), "Risk view (EL, LR, distance to target)")),
                uiOutput("risk_cards"),
                plotlyOutput("risk_line", height = 240)
            )
        )
    )
)

# ==============================================================================
# IMPLEMENTATION PLAN: Lloyd's-Style Rate Change Reactives
# ==============================================================================
#
# NEW REACTIVE OBJECTS TO ADD:
# ----------------------------
#
# 1. prior_inputs()
#    - PURPOSE: Capture last year's pricing inputs for re-rating comparison
#    - RETURNS: List containing:
#      * last_year_rol: Rate on Line from prior period
#      * last_year_q_gu: Ground-up failure rates (optional, for advanced decomp)
#      * last_year_layer_alloc: Layer allocation weights (optional)
#    - USAGE: Forms baseline for "what would we have charged last year?"
#
# 2. current_inputs()
#    - PURPOSE: Formalize current year pricing inputs (extends existing logic)
#    - RETURNS: List containing:
#      * current_rol: Current Rate on Line
#      * current_q_gu: Current ground-up failure rates
#      * current_layer_alloc: Current layer allocation weights
#    - USAGE: The "actual price charged this year"
#
# 3. c_param_lookup()
#    - PURPOSE: Load and interpolate Swiss Re c-parameters from CSV
#    - SOURCE: /home/user/rate-change/c parameters.csv
#    - RETURNS: Function or lookup table mapping c -> expectation
#    - USAGE: Exposure curve parameterization for different risk profiles
#
# 4. prior_exposure_rerated_current_method()
#    - PURPOSE: Re-rate last year's exposure using THIS year's methodology
#    - RETURNS: List containing:
#      * rerated_premium: What we'd charge for prior exposure at current rates
#      * methodology_delta: Difference from actual prior premium
#    - USAGE: Core of RARC calculation - isolates methodology changes
#
# 5. lloyds_decomp()
#    - PURPOSE: Full Lloyd's PMDR-style waterfall decomposition
#    - RETURNS: List containing 4 PMDR components:
#      * delta_attachment: Premium impact from deductible/attachment change
#      * delta_breadth: Premium impact from coverage breadth changes
#      * delta_other: Premium impact from limits/exposure/geography
#      * delta_pure_rate: THE RARC - residual pure rate change
#    - USAGE: Official Lloyd's reporting breakdown
#
# 6. rarc_index()
#    - PURPOSE: Calculate the pure Rate Adjusted Rate Change index
#    - RETURNS: Numeric index where:
#      * 100% = flat (same rate for same risk)
#      * >100% = rate increase
#      * <100% = rate decrease
#    - FORMULA: current_premium / prior_exposure_rerated_current_method
#    - USAGE: The single number Lloyd's cares about most
#
# WIRING PLAN:
# ------------
# prior_inputs() + current_inputs()
#        |
#        v
# c_param_lookup() --> prior_exposure_rerated_current_method()
#        |                            |
#        v                            v
# lloyds_decomp() <------------------ rarc_index()
#        |
#        v
#   [Future UI: PMDR waterfall chart, RARC gauge]
#
# ==============================================================================

# --------------------------
# Server
# --------------------------
server <- function(input, output, session) {
    # Schedule table state
    schedule <- reactiveVal(
        data.frame(Satellite = sat_names, BaseValue = base_values, stringsAsFactors = FALSE)
    )

    output$schedule_tbl <- renderDT({
        datatable(
            schedule(),
            rownames = FALSE,
            options = list(
                dom = "t",
                pageLength = 4,
                ordering = FALSE
            ),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatCurrency("BaseValue", currency = "£", digits = 0)
    })

    observeEvent(input$schedule_tbl_cell_edit, {
        info <- input$schedule_tbl_cell_edit
        df <- schedule()
        i <- info$row
        j <- info$col
        v <- info$value
        # Only BaseValue editable (DT uses 0-based col indexing)
        if (j == 1) {
            newv <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", v)))
            if (is.finite(newv) && newv >= 0) df[i, j + 1] <- newv
            schedule(df)
        }
    })

    # ==========================================================================
    # LLOYD'S RATE CHANGE: Prior Year & Rerated View Tables
    # ==========================================================================

    # --- Prior Failure Rates (q_gu) ---
    prior_q_gu_state <- reactiveVal(
        data.frame(
            Satellite = sat_names,
            q_gu_prior = q_gu,
            stringsAsFactors = FALSE
        )
    )

    output$prior_q_gu_tbl <- renderDT({
        datatable(
            prior_q_gu_state(),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 4, ordering = FALSE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatPercentage("q_gu_prior", digits = 4)
    })

    observeEvent(input$prior_q_gu_tbl_cell_edit, {
        info <- input$prior_q_gu_tbl_cell_edit
        df <- prior_q_gu_state()
        i <- info$row
        j <- info$col
        v <- info$value
        if (j == 1) {
            # Parse as percentage or decimal
            newv <- suppressWarnings(as.numeric(gsub("[^0-9.eE-]", "", v)))
            # If > 1, assume percentage input (e.g., 0.1 entered as 0.1%)
            if (is.finite(newv) && newv >= 0) {
                df[i, j + 1] <- newv
                prior_q_gu_state(df)
            }
        }
    })

    # --- Prior Allocation Weights ---
    prior_alloc_state <- reactiveVal(
        data.frame(
            Satellite = sat_names,
            Allocation = rep(1 / length(sat_names), length(sat_names)),
            stringsAsFactors = FALSE
        )
    )

    output$prior_alloc_tbl <- renderDT({
        datatable(
            prior_alloc_state(),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 4, ordering = FALSE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatPercentage("Allocation", digits = 2)
    })

    observeEvent(input$prior_alloc_tbl_cell_edit, {
        info <- input$prior_alloc_tbl_cell_edit
        df <- prior_alloc_state()
        i <- info$row
        j <- info$col
        v <- info$value
        if (j == 1) {
            newv <- suppressWarnings(as.numeric(gsub("[^0-9.eE-]", "", v)))
            if (is.finite(newv) && newv >= 0 && newv <= 1) {
                df[i, j + 1] <- newv
                prior_alloc_state(df)
            }
        }
    })

    # Validation: prior allocation must sum to 1
    output$prior_alloc_validation <- renderUI({
        alloc_sum <- sum(prior_alloc_state()$Allocation)
        tol <- 0.0001
        if (abs(alloc_sum - 1) > tol) {
            tags$div(
                style = "color: #dc3545; font-size: 0.85rem; margin-top: 0.5rem;",
                icon("exclamation-triangle"),
                sprintf(" Weights sum to %.4f (must equal 1.0)", alloc_sum)
            )
        } else {
            tags$div(
                style = "color: #28a745; font-size: 0.85rem; margin-top: 0.5rem;",
                icon("check-circle"),
                " Weights sum to 1.0"
            )
        }
    })

    # --- Rerated Failure Rates (for rerated view) ---
    rerated_q_gu_state <- reactiveVal(
        data.frame(
            Satellite = sat_names,
            q_gu_rerated = q_gu,
            stringsAsFactors = FALSE
        )
    )

    output$rerated_q_gu_tbl <- renderDT({
        datatable(
            rerated_q_gu_state(),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 4, ordering = FALSE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatPercentage("q_gu_rerated", digits = 4)
    })

    observeEvent(input$rerated_q_gu_tbl_cell_edit, {
        info <- input$rerated_q_gu_tbl_cell_edit
        df <- rerated_q_gu_state()
        i <- info$row
        j <- info$col
        v <- info$value
        if (j == 1) {
            newv <- suppressWarnings(as.numeric(gsub("[^0-9.eE-]", "", v)))
            if (is.finite(newv) && newv >= 0) {
                df[i, j + 1] <- newv
                rerated_q_gu_state(df)
            }
        }
    })

    # --- Rerated Allocation Weights (manual override) ---
    rerated_alloc_state <- reactiveVal(
        data.frame(
            Satellite = sat_names,
            Allocation = rep(1 / length(sat_names), length(sat_names)),
            stringsAsFactors = FALSE
        )
    )

    output$rerated_alloc_tbl <- renderDT({
        datatable(
            rerated_alloc_state(),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 4, ordering = FALSE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatPercentage("Allocation", digits = 2)
    })

    observeEvent(input$rerated_alloc_tbl_cell_edit, {
        info <- input$rerated_alloc_tbl_cell_edit
        df <- rerated_alloc_state()
        i <- info$row
        j <- info$col
        v <- info$value
        if (j == 1) {
            newv <- suppressWarnings(as.numeric(gsub("[^0-9.eE-]", "", v)))
            if (is.finite(newv) && newv >= 0 && newv <= 1) {
                df[i, j + 1] <- newv
                rerated_alloc_state(df)
            }
        }
    })

    # Validation: rerated allocation must sum to 1
    output$rerated_alloc_validation <- renderUI({
        alloc_sum <- sum(rerated_alloc_state()$Allocation)
        tol <- 0.0001
        if (abs(alloc_sum - 1) > tol) {
            tags$div(
                style = "color: #dc3545; font-size: 0.85rem; margin-top: 0.5rem;",
                icon("exclamation-triangle"),
                sprintf(" Weights sum to %.4f (must equal 1.0)", alloc_sum)
            )
        } else {
            tags$div(
                style = "color: #28a745; font-size: 0.85rem; margin-top: 0.5rem;",
                icon("check-circle"),
                " Weights sum to 1.0"
            )
        }
    })

    # ==========================================================================
    # END LLOYD'S RATE CHANGE: Prior Year & Rerated View Tables
    # ==========================================================================

    # Lock ROL unless unlocked
    observe({
        if (!isTRUE(input$unlock_rol)) {
            updateSliderInput(session, "rol", value = 0.0105)
            shinyjs::disable("rol")
        } else {
            shinyjs::enable("rol")
        }
    })

    # Reset buttons
    observeEvent(input$reset_prior, {
        updateSliderInput(session, "excess_prior", value = 5e9)
        updateSliderInput(session, "excess_curr",  value = 5e9)
        updateSliderInput(session, "dep_curr",     value = 0)
        updateSliderInput(session, "brokerage",    value = 0.14)
        updateSliderInput(session, "target_lr",    value = 0.45)
        updateSelectInput(session, "sr_c",         selected = 4)
        if (!isTRUE(input$unlock_rol)) updateSliderInput(session, "rol", value = 0.0105)
    })

    observeEvent(input$reset_current, {
        updateSliderInput(session, "excess_prior", value = 5e9)
        updateSliderInput(session, "excess_curr", value = 5.5e9)
        updateSliderInput(session, "dep_curr", value = -0.15)
        updateRadioButtons(session, "alloc_set", selected = "interp")
        updateSliderInput(session, "alloc_interp", value = 1)
        updateRadioButtons(session, "q_set", selected = "interp")
        updateSliderInput(session, "q_interp", value = 1)
        updateSliderInput(session, "brokerage", value = 0.14)
        updateSliderInput(session, "target_lr", value = 0.45)
        if (!isTRUE(input$unlock_rol)) updateSliderInput(session, "rol", value = 0.0105)
    })
    # Scenario calculators
    calc_scenario <- function(values_base, dep, attachment, rol, brokerage, c_curve, q_gu) {
        values <- values_base * (1 + dep)
        tiv <- sum(values)
        lim <- limit_from_values(values, attachment)
        vil <- value_in_layer(values, attachment, lim)

        # Curve-driven allocation + layer-adjusted failure rates
        cq <- curve_allocation_and_q(values = values, A = attachment, L = lim, c = c_curve, q_gu = q_gu)
        q_layer <- cq$q_layer

        gp <- premium_gross(rol, lim)
        np <- premium_net(gp, brokerage)

        el <- expected_loss(q_layer, vil)
        lr <- implied_lr(el, np)

        indicated_gross <- ifelse(
            is.na(lr) || isTRUE(all.equal(input$target_lr, 0)),
            NA_real_,
            (el / input$target_lr) / (1 - brokerage)
        )

        list(
            values = values, tiv = tiv, limit = lim, vil = vil,
            gp = gp, np = np, el = el, lr = lr,
            indicated_gross = indicated_gross,
            shares = cq$shares, w_curve = cq$w_curve, q_layer = q_layer
        )
    }

    values_base <- reactive(schedule()$BaseValue)

    # ==========================================================================
    # LLOYD'S RATE CHANGE REACTIVE STUBS
    # ==========================================================================
    # These are placeholder reactives that return NULL for now.
    # They will be implemented in subsequent iterations to provide
    # full Lloyd's PMDR-compliant rate change decomposition.
    # ==========================================================================

    # --------------------------------------------------------------------------
    # 1. prior_inputs()
    # --------------------------------------------------------------------------
    # Captures last year's pricing inputs for RARC comparison
    # Includes: prior ROL, prior q_gu (optional), prior allocation (optional)
    # Uses existing input$excess_prior for attachment
    # --------------------------------------------------------------------------
    prior_inputs <- reactive({
        # Prior ROL from UI
        prior_rol <- input$prior_rol

        # Prior attachment (reuse existing slider)
        prior_attachment <- input$excess_prior

        # Prior failure rates: use custom if toggled, else default q_gu
        use_prior_q <- isTRUE(input$use_prior_failure_rates)
        prior_q_gu_vec <- if (use_prior_q) {
            prior_q_gu_state()$q_gu_prior
        } else {
            q_gu  # Default to current q_gu
        }

        # Prior allocation: use manual if toggled, else NULL (will use MBBEFD)
        use_prior_alloc <- isTRUE(input$use_prior_allocation)
        prior_alloc_vec <- if (use_prior_alloc) {
            prior_alloc_state()$Allocation
        } else {
            NULL  # Signal to use MBBEFD curve
        }

        # Validation flags
        prior_alloc_valid <- if (use_prior_alloc) {
            abs(sum(prior_alloc_state()$Allocation) - 1) < 0.0001
        } else {
            TRUE
        }

        list(
            prior_rol = prior_rol,
            prior_attachment = prior_attachment,
            use_prior_failure_rates = use_prior_q,
            prior_q_gu = prior_q_gu_vec,
            use_prior_allocation = use_prior_alloc,
            prior_allocation = prior_alloc_vec,
            prior_allocation_valid = prior_alloc_valid
        )
    })

    # --------------------------------------------------------------------------
    # 2. current_inputs()
    # --------------------------------------------------------------------------
    # Formalizes current year pricing inputs
    # Includes: current ROL, current q_gu, current allocation (from MBBEFD)
    # --------------------------------------------------------------------------
    current_inputs <- reactive({
        # Current ROL from UI
        current_rol <- input$rol

        # Current attachment
        current_attachment <- input$excess_curr

        # Current failure rates (always use default q_gu for now)
        current_q_gu_vec <- q_gu

        # Current allocation: computed from MBBEFD curve in current() reactive
        # Will be populated when we integrate with current() scenario
        current_alloc_vec <- current()$w_curve

        list(
            current_rol = current_rol,
            current_attachment = current_attachment,
            current_q_gu = current_q_gu_vec,
            current_allocation = current_alloc_vec
        )
    })

    # --------------------------------------------------------------------------
    # Helper: Rerated view inputs
    # --------------------------------------------------------------------------
    # Inputs for re-rating prior exposure using current methodology
    # --------------------------------------------------------------------------
    rerated_inputs <- reactive({
        # Rerated failure rates: use manual if toggled, else use current q_gu
        use_manual_rates <- isTRUE(input$use_rerated_manual_rates)
        rerated_q_gu_vec <- if (use_manual_rates) {
            rerated_q_gu_state()$q_gu_rerated
        } else {
            q_gu  # Default to current q_gu
        }

        # Rerated allocation: use manual if toggled, else use MBBEFD (NULL signals MBBEFD)
        use_manual_alloc <- isTRUE(input$use_rerated_manual_alloc)
        rerated_alloc_vec <- if (use_manual_alloc) {
            rerated_alloc_state()$Allocation
        } else {
            NULL  # Signal to use MBBEFD curve
        }

        # Validation flags
        rerated_alloc_valid <- if (use_manual_alloc) {
            abs(sum(rerated_alloc_state()$Allocation) - 1) < 0.0001
        } else {
            TRUE
        }

        list(
            use_manual_rates = use_manual_rates,
            rerated_q_gu = rerated_q_gu_vec,
            use_manual_allocation = use_manual_alloc,
            rerated_allocation = rerated_alloc_vec,
            rerated_allocation_valid = rerated_alloc_valid
        )
    })

    # --------------------------------------------------------------------------
    # 3. c_param_lookup()
    # --------------------------------------------------------------------------
    # Provides the CSV lookup table and per-satellite c values
    # Uses the c_params_csv loaded at startup
    # --------------------------------------------------------------------------
    c_param_lookup <- reactive({
        # Return the lookup table and helper functions
        list(
            lookup_table = c_params_csv,
            map_fn = map_failure_rate_to_c,
            per_satellite_fn = compute_per_satellite_c,
            portfolio_fn = compute_portfolio_c
        )
    })

    # --------------------------------------------------------------------------
    # Helper: per_satellite_c()
    # --------------------------------------------------------------------------
    # Computes c values for each satellite based on their failure rates
    # --------------------------------------------------------------------------
    per_satellite_c <- reactive({
        compute_per_satellite_c(q_gu)
    })

    # --------------------------------------------------------------------------
    # Helper: portfolio_c()
    # --------------------------------------------------------------------------
    # Computes portfolio-level c as value-weighted average
    # ASSUMPTION: We use base_values (not depreciated) for weighting
    # --------------------------------------------------------------------------
    portfolio_c <- reactive({
        compute_portfolio_c(q_gu, values_base())
    })

    # --------------------------------------------------------------------------
    # Helper: effective_c()
    # --------------------------------------------------------------------------
    # Returns the c parameter to use based on c_mode:
    # - "manual": use input$sr_c dropdown
    # - "auto": use portfolio_c() (value-weighted average from CSV)
    # --------------------------------------------------------------------------
    effective_c <- reactive({
        mode <- input$c_mode
        if (mode == "manual") {
            as.numeric(input$sr_c)
        } else {
            # Auto mode: use portfolio-level c
            portfolio_c()
        }
    })

    # --------------------------------------------------------------------------
    # Output: auto_c_display
    # --------------------------------------------------------------------------
    # Shows the computed c values when in auto mode
    # --------------------------------------------------------------------------
    output$auto_c_display <- renderUI({
        c_per_sat <- per_satellite_c()
        port_c <- portfolio_c()

        # Create a mini-table showing per-satellite c
        sat_df <- data.frame(
            Satellite = sat_names,
            q_gu = q_gu,
            c_i = round(c_per_sat, 3)
        )

        tags$div(
            style = "margin-top: 0.5rem;",
            tags$table(
                class = "table table-sm",
                style = "font-size: 0.8rem; margin-bottom: 0.5rem;",
                tags$thead(
                    tags$tr(
                        tags$th("Sat"),
                        tags$th("q_gu"),
                        tags$th("c_i")
                    )
                ),
                tags$tbody(
                    lapply(seq_len(nrow(sat_df)), function(i) {
                        tags$tr(
                            tags$td(sat_df$Satellite[i]),
                            tags$td(sprintf("%.4f%%", sat_df$q_gu[i] * 100)),
                            tags$td(sprintf("%.3f", sat_df$c_i[i]))
                        )
                    })
                )
            ),
            tags$div(
                style = "font-weight: bold; color: #2c3e50;",
                sprintf("Portfolio c (value-weighted): %.3f", port_c)
            )
        )
    })

    # --------------------------------------------------------------------------
    # 4. prior_exposure_rerated_current_method()
    # --------------------------------------------------------------------------
    # Re-rates last year's exposure using THIS year's methodology
    # This is the core of RARC: "what would we charge for prior risk today?"
    #
    # DEFINITION:
    # - Take PRIOR exposure (values, attachment from prior year)
    # - Apply CURRENT ROL (this year's rate)
    # - This gives: "same risk, current pricing"
    #
    # Returns: List with:
    #   - rerated_gross: Gross premium for prior exposure at current ROL
    #   - rerated_net: Net premium for prior exposure at current ROL
    #   - prior_gross: Original prior gross premium (for reference)
    #   - methodology_delta: rerated_gross - prior_gross (ROL change impact)
    # --------------------------------------------------------------------------
    prior_exposure_rerated_current_method <- reactive({
        # Get prior exposure parameters
        v0 <- values_base()
        A_prior <- input$excess_prior
        bro <- input$brokerage
        cpar <- effective_c()

        # Prior ROL (from Lloyd's inputs) vs Current ROL
        rol_prior <- input$prior_rol
        rol_current <- input$rol

        # Calculate prior scenario (prior exposure at prior ROL)
        prior_scenario <- calc_scenario(
            values_base = v0,
            dep = 0,
            attachment = A_prior,
            rol = rol_prior,
            brokerage = bro,
            c_curve = cpar,
            q_gu = q_gu
        )

        # Calculate rerated scenario (prior exposure at CURRENT ROL)
        rerated_scenario <- calc_scenario(
            values_base = v0,
            dep = 0,
            attachment = A_prior,
            rol = rol_current,
            brokerage = bro,
            c_curve = cpar,
            q_gu = q_gu
        )

        list(
            rerated_gross = rerated_scenario$gp,
            rerated_net = rerated_scenario$np,
            prior_gross = prior_scenario$gp,
            prior_net = prior_scenario$np,
            methodology_delta = rerated_scenario$gp - prior_scenario$gp
        )
    })

    # --------------------------------------------------------------------------
    # 5. lloyds_decomp()
    # --------------------------------------------------------------------------
    # Full Lloyd's PMDR-style waterfall decomposition
    #
    # COMPONENTS:
    # 1. delta_attachment: Premium impact from deductible/attachment change
    #    = (prior values at current attachment) - (prior values at prior attachment)
    # 2. delta_breadth: Premium impact from coverage scope changes (user input)
    # 3. delta_other: Premium impact from limits/exposure/geography (user input)
    # 4. delta_pure_rate: RARC residual = total_change - (1+2+3)
    #
    # CONSTRAINT: prior_premium + sum(deltas) = current_premium
    #
    # Also computes:
    # - benchmark_premium: EL_current / target_lr / (1 - brokerage)
    # - price_adequacy: current_premium / benchmark_premium
    # --------------------------------------------------------------------------
    lloyds_decomp <- reactive({
        # Get scenario data
        p <- prior()
        c <- current()
        bro <- input$brokerage
        tgt_lr <- input$target_lr

        # Premium values
        prior_gross <- p$gp
        prior_net <- p$np
        current_gross <- c$gp
        current_net <- c$np
        total_change <- current_gross - prior_gross

        # ----- Component 1: Deductible/Attachment Change -----
        # Calculate: prior values/ROL but with CURRENT attachment
        v0 <- values_base()
        rol <- input$rol
        cpar <- effective_c()
        A_prior <- input$excess_prior
        A_current <- input$excess_curr

        # Intermediate scenario: prior values at current attachment
        intermediate <- calc_scenario(
            values_base = v0,
            dep = 0,
            attachment = A_current,  # <-- CURRENT attachment
            rol = rol,
            brokerage = bro,
            c_curve = cpar,
            q_gu = q_gu
        )
        # delta_attachment = change from moving attachment (holding everything else at prior)
        # We compute: (prior values at current attachment) - (prior values at prior attachment)
        delta_attachment <- intermediate$gp - prior_gross

        # ----- Component 2: Breadth of Cover Change -----
        # User input (manual adjustment)
        delta_breadth <- input$breadth_of_cover_change

        # ----- Component 3: Other Exposure Change -----
        # User input (manual adjustment)
        delta_other <- input$other_exposure_change

        # ----- Component 4: Pure Rate Change (RARC) -----
        # Residual: whatever is left after accounting for 1, 2, 3
        delta_pure_rate <- total_change - delta_attachment - delta_breadth - delta_other

        # ----- Benchmark Premium -----
        # Theoretical gross premium to achieve target LR
        # benchmark_gross = EL_current / target_lr / (1 - brokerage)
        el_current <- c$el
        benchmark_gross <- if (tgt_lr > 0 && bro < 1) {
            el_current / tgt_lr / (1 - bro)
        } else {
            NA_real_
        }
        benchmark_net <- if (!is.na(benchmark_gross)) benchmark_gross * (1 - bro) else NA_real_

        # ----- Price Adequacy -----
        # current_premium / benchmark_premium
        price_adequacy <- if (!is.na(benchmark_gross) && benchmark_gross > 0) {
            current_gross / benchmark_gross
        } else {
            NA_real_
        }

        # ----- Verification -----
        # Check that decomposition sums correctly
        reconstructed <- prior_gross + delta_attachment + delta_breadth + delta_other + delta_pure_rate
        decomp_error <- abs(reconstructed - current_gross)

        list(
            # Starting point
            prior_gross = prior_gross,
            prior_net = prior_net,

            # PMDR Components (£ deltas)
            delta_attachment = delta_attachment,
            delta_breadth = delta_breadth,
            delta_other = delta_other,
            delta_pure_rate = delta_pure_rate,

            # Ending point
            current_gross = current_gross,
            current_net = current_net,
            total_change = total_change,

            # Benchmark & adequacy
            benchmark_gross = benchmark_gross,
            benchmark_net = benchmark_net,
            price_adequacy = price_adequacy,

            # Expected loss (for reference)
            el_prior = p$el,
            el_current = el_current,

            # Verification
            reconstructed = reconstructed,
            decomp_error = decomp_error
        )
    })

    # --------------------------------------------------------------------------
    # 6. rarc_index()
    # --------------------------------------------------------------------------
    # The pure Risk Adjusted Rate Change index
    #
    # DEFINITION:
    # RARC = (prior exposure priced at CURRENT ROL) / (prior exposure priced at PRIOR ROL)
    #
    # This isolates the pure rate movement by comparing:
    # - Numerator: what the SAME risk would cost at current rates
    # - Denominator: what the SAME risk actually cost at prior rates
    #
    # Returns:
    # - 100% = flat (same rate for same risk)
    # - >100% = rate increase (charging more for same risk)
    # - <100% = rate decrease (charging less for same risk)
    # --------------------------------------------------------------------------
    rarc_index <- reactive({
        rerated <- prior_exposure_rerated_current_method()

        # RARC = rerated_gross / prior_gross
        # (same exposure, current ROL) / (same exposure, prior ROL)
        if (rerated$prior_gross > 0) {
            rerated$rerated_gross / rerated$prior_gross
        } else {
            NA_real_
        }
    })

    # --------------------------------------------------------------------------
    # Output: lloyds_decomp_table
    # --------------------------------------------------------------------------
    # Verification table showing all decomposition components
    # --------------------------------------------------------------------------
    output$lloyds_decomp_table <- renderUI({
        decomp <- lloyds_decomp()
        rarc <- rarc_index()
        rerated <- prior_exposure_rerated_current_method()

        # Format helper
        fmt <- function(x, prefix = "£") {
            if (is.na(x)) return("—")
            paste0(prefix, format(round(x), big.mark = ",", scientific = FALSE))
        }
        fmt_pct <- function(x) {
            if (is.na(x)) return("—")
            sprintf("%.2f%%", x * 100)
        }
        fmt_delta <- function(x) {
            if (is.na(x)) return("—")
            sign <- if (x >= 0) "+" else ""
            paste0(sign, fmt(x))
        }

        tags$div(
            style = "font-size: 0.85rem;",

            # PMDR Decomposition Table
            tags$h6("PMDR Decomposition", style = "margin-top: 0;"),
            tags$table(
                class = "table table-sm table-striped",
                style = "margin-bottom: 1rem;",
                tags$thead(
                    tags$tr(
                        tags$th("Component"),
                        tags$th(style = "text-align: right;", "Value")
                    )
                ),
                tags$tbody(
                    tags$tr(
                        tags$td("Prior Gross Premium"),
                        tags$td(style = "text-align: right;", fmt(decomp$prior_gross))
                    ),
                    tags$tr(style = "background: #e3f2fd;",
                        tags$td("1. Deductible/Attachment Change"),
                        tags$td(style = "text-align: right;", fmt_delta(decomp$delta_attachment))
                    ),
                    tags$tr(style = "background: #e3f2fd;",
                        tags$td("2. Breadth of Cover Change"),
                        tags$td(style = "text-align: right;", fmt_delta(decomp$delta_breadth))
                    ),
                    tags$tr(style = "background: #e3f2fd;",
                        tags$td("3. Other Exposure Change"),
                        tags$td(style = "text-align: right;", fmt_delta(decomp$delta_other))
                    ),
                    tags$tr(style = "background: #fff3cd; font-weight: bold;",
                        tags$td("4. Pure Rate Change (RARC)"),
                        tags$td(style = "text-align: right;", fmt_delta(decomp$delta_pure_rate))
                    ),
                    tags$tr(style = "font-weight: bold; border-top: 2px solid #333;",
                        tags$td("Current Gross Premium"),
                        tags$td(style = "text-align: right;", fmt(decomp$current_gross))
                    ),
                    tags$tr(style = "color: #6c757d;",
                        tags$td("Total Change"),
                        tags$td(style = "text-align: right;", fmt_delta(decomp$total_change))
                    )
                )
            ),

            # Benchmark & Adequacy
            tags$h6("Benchmark & Price Adequacy"),
            tags$table(
                class = "table table-sm",
                style = "margin-bottom: 1rem;",
                tags$tbody(
                    tags$tr(
                        tags$td("Expected Loss (current)"),
                        tags$td(style = "text-align: right;", fmt(decomp$el_current))
                    ),
                    tags$tr(
                        tags$td("Benchmark Gross Premium"),
                        tags$td(style = "text-align: right;", fmt(decomp$benchmark_gross))
                    ),
                    tags$tr(style = "font-weight: bold;",
                        tags$td("Price Adequacy"),
                        tags$td(style = "text-align: right;", fmt_pct(decomp$price_adequacy))
                    )
                )
            ),

            # RARC Index
            tags$h6("Risk Adjusted Rate Change (RARC)"),
            tags$table(
                class = "table table-sm",
                style = "margin-bottom: 1rem;",
                tags$tbody(
                    tags$tr(
                        tags$td("Prior exposure @ Prior ROL"),
                        tags$td(style = "text-align: right;", fmt(rerated$prior_gross))
                    ),
                    tags$tr(
                        tags$td("Prior exposure @ Current ROL"),
                        tags$td(style = "text-align: right;", fmt(rerated$rerated_gross))
                    ),
                    tags$tr(style = "font-weight: bold; background: #d4edda;",
                        tags$td("RARC Index"),
                        tags$td(style = "text-align: right;", fmt_pct(rarc))
                    )
                )
            ),

            # Verification
            if (decomp$decomp_error > 0.01) {
                tags$div(
                    style = "color: #dc3545; font-size: 0.8rem;",
                    icon("exclamation-triangle"),
                    sprintf(" Decomposition error: %s (should be 0)", fmt(decomp$decomp_error))
                )
            } else {
                tags$div(
                    style = "color: #28a745; font-size: 0.8rem;",
                    icon("check-circle"),
                    " Decomposition verified: sum of components equals total change"
                )
            }
        )
    })

    # ==========================================================================
    # END LLOYD'S RATE CHANGE REACTIVE STUBS
    # ==========================================================================

    prior <- reactive({
        calc_scenario(
            values_base = values_base(),
            dep = 0,
            attachment = input$excess_prior,
            rol = input$rol,
            brokerage = input$brokerage,
            c_curve = effective_c(),
            q_gu = q_gu
        )
    })

    current <- reactive({
        calc_scenario(
            values_base = values_base(),
            dep = input$dep_curr,
            attachment = input$excess_curr,
            rol = input$rol,
            brokerage = input$brokerage,
            c_curve = effective_c(),
            q_gu = q_gu
        )
    })

    # KPI cards: show Prior vs Current and % change
    output$kpi_tiv <- renderUI({
        p <- prior()$tiv
        c <- current()$tiv
        chg <- ifelse(p == 0, NA_real_, (c / p) - 1)
        HTML(paste0(
            "<div style='line-height:1.2'>",
            "<div><b>Prior:</b> ", fmt_bn(p), "</div>",
            "<div><b>Current:</b> ", fmt_bn(c), "</div>",
            "<div style='opacity:0.8'>Change: ", ifelse(is.na(chg), "—", fmt_pct(chg, 1)), "</div>",
            "</div>"
        ))
    })

    output$kpi_limit <- renderUI({
        p <- prior()$limit
        c <- current()$limit
        chg <- ifelse(p == 0, NA_real_, (c / p) - 1)
        HTML(paste0(
            "<div style='line-height:1.2'>",
            "<div><b>Prior:</b> ", fmt_bn(p), "</div>",
            "<div><b>Current:</b> ", fmt_bn(c), "</div>",
            "<div style='opacity:0.8'>Change: ", ifelse(is.na(chg), "—", fmt_pct(chg, 1)), "</div>",
            "</div>"
        ))
    })

    output$kpi_prem <- renderUI({
        p_g <- prior()$gp
        c_g <- current()$gp
        p_n <- prior()$np
        c_n <- current()$np
        idx <- ifelse(p_g == 0, NA_real_, c_g / p_g)
        HTML(paste0(
            "<div style='line-height:1.25'>",
            "<div><b>Prior gross:</b> ", fmt_mn(p_g), " &nbsp;&nbsp; <b>net:</b> ", fmt_mn(p_n), "</div>",
            "<div><b>Current gross:</b> ", fmt_mn(c_g), " &nbsp;&nbsp; <b>net:</b> ", fmt_mn(c_n), "</div>",
            "<div style='opacity:0.8'>Premium index: ", ifelse(is.na(idx), "—", fmt_pct(idx - 1, 1)), " (",
            ifelse(is.na(idx), "—", paste0(round(100 * idx, 1), "%")),
            ")</div>",
            "</div>"
        ))
    })

    output$kpi_el <- renderUI({
        p <- prior()$el
        c <- current()$el
        chg <- ifelse(p == 0, NA_real_, (c / p) - 1)
        HTML(paste0(
            "<div style='line-height:1.2'>",
            "<div><b>Prior:</b> ", fmt_mn(p), "</div>",
            "<div><b>Current:</b> ", fmt_mn(c), "</div>",
            "<div style='opacity:0.8'>Change: ", ifelse(is.na(chg), "—", fmt_pct(chg, 1)), "</div>",
            "</div>"
        ))
    })

    # Waterfall decomposition (booked premium) + risk-view (indicated)
    waterfall_data <- reactive({
        rol <- input$rol
        bro <- input$brokerage

        v0 <- values_base()
        A0 <- input$excess_prior
        A1 <- input$excess_curr
        dep1 <- input$dep_curr

        # Step states
        cpar <- effective_c()
        s0 <- calc_scenario(v0, 0, A0, rol, bro, cpar, q_gu)
        s1 <- calc_scenario(v0, dep1, A0, rol, bro, cpar, q_gu)
        s2 <- calc_scenario(v0, dep1, A1, rol, bro, cpar, q_gu)
        s3 <- s2
        s4 <- s2

        indicated <- c(s0$indicated_gross, s1$indicated_gross, s2$indicated_gross, s3$indicated_gross, s4$indicated_gross)

        steps <- tibble::tibble(
            step = c(
                "Prior premium",
                "1) Value depreciation",
                "2) Excess change",
                "3) Exposure curve shift (risk view)",
                "4) Failure rate shift (risk view)",
                "Current premium"
            ),
            booked = c(s0$gp, s1$gp, s2$gp, s2$gp, s2$gp, s4$gp), # keep booked flat for steps 3-4 (story clarity)
            indicated = c(indicated[1], indicated[2], indicated[3], indicated[4], indicated[5], indicated[5]),
            delta_booked = c(NA, s1$gp - s0$gp, s2$gp - s1$gp, 0, 0, NA),
            delta_ind = c(NA, indicated[2] - indicated[1], indicated[3] - indicated[2], indicated[4] - indicated[3], indicated[5] - indicated[4], NA)
        )

        list(steps = steps, s0 = s0, s4 = s4, s1 = s1, s2 = s2, s3 = s3)
    })

    output$waterfall <- renderPlotly({
        df <- waterfall_data()$steps

        measure <- c("absolute", rep("relative", 4), "total")
        y <- c(df$booked[1], df$delta_booked[2], df$delta_booked[3], 0, 0, df$booked[6])

        txt <- c(
            paste0("Prior gross premium: ", fmt_mn(df$booked[1])),
            paste0("Δ from depreciation: ", fmt_mn(df$delta_booked[2])),
            paste0("Δ from excess change: ", fmt_mn(df$delta_booked[3])),
            "Booked premium unchanged (risk-view step)",
            "Booked premium unchanged (risk-view step)",
            paste0("Current gross premium: ", fmt_mn(df$booked[6]))
        )

        p <- plot_ly(
            type = "waterfall",
            measure = measure,
            x = df$step,
            y = y,
            text = txt,
            hoverinfo = "text",
            showlegend = FALSE
        ) %>%
            layout(
                yaxis = list(title = "Gross premium (booked)"),
                xaxis = list(title = ""),
                margin = list(l = 50, r = 20, t = 10, b = 90)
            )

        # Add indicated premium line (risk view) overlay
        if (all(is.finite(df$indicated))) {
            p <- p %>%
                add_lines(
                    x = df$step,
                    y = df$indicated,
                    name = "Indicated gross (EL / target LR)",
                    hovertext = paste0("Indicated gross: ", fmt_mn(df$indicated)),
                    hoverinfo = "text",
                    line = list(width = 3, dash = "dot"),
                    inherit = FALSE
                )
        }

        p
    })

    # Gauge
    output$gauge <- renderPlotly({

        lr_prior   <- prior()$lr
        lr_current <- current()$lr

        idx <- ifelse(is.na(lr_prior) || lr_prior == 0, NA_real_, lr_current / lr_prior)

        label <- ifelse(
            is.na(idx), "—",
            ifelse(idx < 1, "Improvement", ifelse(abs(idx - 1) < 0.0005, "Flat", "Deterioration"))
        )

        plot_ly(
            type = "indicator",
            mode = "gauge+number+delta",
            value = ifelse(is.na(idx), 0, 100 * idx),
            number = list(suffix = "%", valueformat = ".1f"),
            delta = list(reference = 100, valueformat = ".1f", suffix = " pts"),
            title = list(text = paste0("Loss Ratio Index (Current / Prior) — ", label)),
            gauge = list(
            axis = list(range = list(60, 140)),
            threshold = list(line = list(width = 4), thickness = 0.85, value = 100),
            steps = list(
                list(range = c(60, 100), color = "#d4edda"),  # green zone
                list(range = c(100, 120), color = "#fff3cd"), # amber
                list(range = c(120, 140), color = "#f8d7da")  # red
            )
            )
        ) %>%
            layout(margin = list(l = 30, r = 30, t = 40, b = 10))
        })

    # Mechanics panel (dynamic bullets) + dominant driver
    output$mechanics <- renderUI({
        rol <- input$rol
        dep <- input$dep_curr
        A0 <- input$excess_prior
        A1 <- input$excess_curr

        p <- prior()
        c <- current()

        wf <- waterfall_data()$steps
        d1 <- wf$delta_booked[2]
        d2 <- wf$delta_booked[3]
        dom <- ifelse(abs(d1) >= abs(d2), "Value depreciation", "Excess (attachment) change")

        bullets <- list(
            HTML(paste0("We held <b>ROL</b> at ", fmt_pct(rol, 2), " (rate applied to the <b>limit</b>, not directly to TIV).")),
            HTML(paste0("Attachment moved from <b>", fmt_bn(A0), "</b> to <b>", fmt_bn(A1), "</b>, changing how much value sits in the layer.")),
            HTML(paste0("Current values are depreciated by <b>", fmt_pct(dep, 0), "</b>, shrinking the value base used to form the layer limit.")),
            HTML(paste0("That changed the <b>effective limit / premium base</b> from ", fmt_bn(p$limit), " to ", fmt_bn(c$limit), ", so gross premium moved even with flat ROL.")),
            HTML(paste0("Expected loss moved from ", fmt_mn(p$el), " to ", fmt_mn(c$el), " as the allocation and failure-rate view shifted.")),
            HTML(paste0("Dominant booked-premium driver today: <b>", dom, "</b>."))
        )

        tags$ul(
            style = "margin-bottom:0;",
            lapply(bullets, tags$li)
        )
    })

    output$dominant_driver <- renderUI({
        wf <- waterfall_data()$steps
        d_dep <- wf$delta_booked[2]
        d_exc <- wf$delta_booked[3]

        pick <- ifelse(abs(d_dep) >= abs(d_exc), "Depreciation", "Attachment")

        # Simple mini-table (no DT) for clarity
        tags$div(
            style = "font-size: 0.98rem;",
            tags$div(
                style = "padding:0.4rem 0.6rem; background:rgba(0,0,0,0.03); border-radius:0.75rem; margin-bottom:0.6rem;",
                HTML(paste0("<b>Booked premium delta attribution:</b> ", pick, " dominates (by absolute £ change)."))
            ),
            tags$table(
                class = "table table-sm",
                tags$thead(
                    tags$tr(
                        tags$th("Booked step"),
                        tags$th(style = "text-align:right;", "Δ Gross premium")
                    )
                ),
                tags$tbody(
                    tags$tr(
                        tags$td("1) Value depreciation"),
                        tags$td(style = "text-align:right;", fmt_mn(d_dep))
                    ),
                    tags$tr(
                        tags$td("2) Excess change"),
                        tags$td(style = "text-align:right;", fmt_mn(d_exc))
                    )
                )
            )
        )
    })

    # Risk view cards + trend line
    output$risk_cards <- renderUI({
        p <- prior()
        c <- current()
        tgt <- input$target_lr

        dist_p <- ifelse(is.na(p$lr), NA_real_, p$lr - tgt)
        dist_c <- ifelse(is.na(c$lr), NA_real_, c$lr - tgt)

        # Format helpers for LR display
        f_lr <- function(x) ifelse(is.na(x), "—", fmt_pct(x, 1))
        f_dist <- function(x) ifelse(is.na(x), "—", paste0(ifelse(x > 0, "+", ""), fmt_pct(x, 1)))

        # Indicated premium movement
        wf <- waterfall_data()$steps
        ind0 <- wf$indicated[1]
        ind1 <- wf$indicated[6]
        ind_chg <- ifelse(!is.finite(ind0) || ind0 == 0, NA_real_, (ind1 / ind0) - 1)

        layout_column_wrap(
            width = 1 / 3,
            gap = "0.75rem",
            value_box(
                title = "Implied LR (prior)",
                value = HTML(paste0(
                    "<div style='line-height:1.15'>",
                    "<div>", f_lr(p$lr), "</div>",
                    "<div style='opacity:0.8'>vs target ", fmt_pct(tgt, 1), ": ", f_dist(dist_p), "</div>",
                    "</div>"
                )),
                showcase = icon("bullseye"),
                theme_color = "secondary"
            ),
            value_box(
                title = "Implied LR (current)",
                value = HTML(paste0(
                    "<div style='line-height:1.15'>",
                    "<div>", f_lr(c$lr), "</div>",
                    "<div style='opacity:0.8'>vs target ", fmt_pct(tgt, 1), ": ", f_dist(dist_c), "</div>",
                    "</div>"
                )),
                showcase = icon("chart-line"),
                theme_color = "primary"
            ),
            value_box(
                title = "Indicated gross premium",
                value = HTML(paste0(
                    "<div style='line-height:1.15'>",
                    "<div><b>Prior:</b> ", ifelse(is.finite(ind0), fmt_mn(ind0), "—"), "</div>",
                    "<div><b>Current:</b> ", ifelse(is.finite(ind1), fmt_mn(ind1), "—"), "</div>",
                    "<div style='opacity:0.8'>Change: ", ifelse(is.na(ind_chg), "—", fmt_pct(ind_chg, 1)), "</div>",
                    "</div>"
                )),
                showcase = icon("scale-balanced"),
                theme_color = "info"
            )
        )
    })

    output$risk_line <- renderPlotly({
        wf <- waterfall_data()$steps
        tgt <- input$target_lr
        bro <- input$brokerage

        # Compute implied LR at each step using indicated gross (if present) OR directly from scenario objects.
        # We'll use scenario objects for robustness.
        s <- waterfall_data()
        s0 <- s$s0
        s1 <- s$s1
        s2 <- s$s2
        s3 <- s$s3
        s4 <- s$s4

        # Net prem at each step (booked view uses limit; but LR is EL / net premium under same booked net prem)
        # For steps 3-4 booked premium remains s2; LR still changes because EL changes.
        net_s0 <- s0$np
        net_s1 <- s1$np
        net_s2 <- s2$np
        net_s3 <- s2$np
        net_s4 <- s2$np

        lr_path <- c(
            implied_lr(s0$el, net_s0),
            implied_lr(s1$el, net_s1),
            implied_lr(s2$el, net_s2),
            implied_lr(s3$el, net_s3),
            implied_lr(s4$el, net_s4)
        )

        df <- tibble::tibble(
            step = c("Prior", "Depreciation", "Excess", "Exposure curve", "Failure rate"),
            lr = lr_path
        )

        plot_ly(df,
            x = ~step, y = ~lr, type = "scatter", mode = "lines+markers",
            hovertext = ~ paste0("Implied LR: ", ifelse(is.na(lr), "—", fmt_pct(lr, 1))),
            hoverinfo = "text",
            name = "Implied LR"
        ) %>%
            add_lines(
                x = df$step,
                y = rep(tgt, nrow(df)),
                name = "Target LR",
                hovertext = paste0("Target LR: ", fmt_pct(tgt, 1)),
                hoverinfo = "text",
                line = list(dash = "dot")
            ) %>%
            layout(
                yaxis = list(title = "Loss ratio", tickformat = ".0%"),
                xaxis = list(title = ""),
                margin = list(l = 60, r = 20, t = 10, b = 50),
                legend = list(orientation = "h", x = 0, y = -0.2)
            )
    })
    output$ec_curve <- renderPlotly({
        vals0 <- values_base()
        # Scenario MPL (tower top) consistent with your “tower top = max(V_i)” assumption
        MPL0 <- max(vals0)               # prior MPL (no dep)
        vals1 <- vals0 * (1 + input$dep_curr)
        MPL1 <- max(vals1)               # current MPL (post-dep)

        # Use your scenario limits
        L0 <- prior()$limit
        L1 <- current()$limit

        A0 <- input$excess_prior
        A1 <- input$excess_curr

        cpar <- effective_c()

        s0 <- layer_share_from_curve(A0, L0, MPL0, cpar)
        s1 <- layer_share_from_curve(A1, L1, MPL1, cpar)

        xs <- seq(0, 1, length.out = 200)
        Gs <- G_mbbefd(xs, cpar)

        df_curve <- data.frame(x = xs, G = Gs)

        pts <- data.frame(
            scen = c("Prior", "Prior", "Current", "Current"),
            x = c(s0$d, s0$u, s1$d, s1$u),
            y = c(s0$Gd, s0$Gu, s1$Gd, s1$Gu),
            lab = c("d (att/MPL)", "u ((att+lim)/MPL)", "d (att/MPL)", "u ((att+lim)/MPL)")
        )

        # Annotation text for layer shares
        ann <- paste0(
            "Prior layer share G(u)-G(d): ", sprintf("%.1f%%", 100*s0$share),
            "<br>Current layer share G(u)-G(d): ", sprintf("%.1f%%", 100*s1$share),
            if (!has_mbbefd()) "<br><i>Note: using fallback curve (install 'mbbefd' for true MBBEFD)</i>" else ""
        )

        plot_ly() %>%
            add_lines(data = df_curve, x = ~x, y = ~G, name = "G(x)", hoverinfo = "skip") %>%
            add_markers(
            data = pts,
            x = ~x, y = ~y, color = ~scen, symbol = ~lab,
            hovertext = ~paste0(scen, "<br>", lab, "<br>x=", sprintf("%.3f", x), "<br>G(x)=", sprintf("%.3f", y)),
            hoverinfo = "text",
            marker = list(size = 9)
            ) %>%
            layout(
            xaxis = list(title = "Deductible as % of MPL (x)"),
            yaxis = list(title = "Exposure curve G(x)"),
            margin = list(l = 60, r = 20, t = 10, b = 50),
            annotations = list(
                list(
                x = 0.02, y = 0.98, xref = "paper", yref = "paper",
                text = ann, showarrow = FALSE, align = "left"
                )
            ),
            legend = list(orientation = "h", x = 0, y = -0.2)
            )
        })

        output$pct_bridge <- renderPlotly({
            # Booked premium base change (limit) %
            p_lim <- prior()$limit
            c_lim <- current()$limit
            prem_base_chg <- ifelse(p_lim <= 0, NA_real_, (c_lim / p_lim) - 1)

            # Allocation change % (chosen allocation view)
            w0 <- prior()$w_curve
            w1 <- current()$w_curve
            alloc_chg <- alloc_shift_pct(w0, w1)

            share0 <- sum(prior()$shares * prior()$values) / sum(prior()$values)   # value-weighted avg share
            share1 <- sum(current()$shares * current()$values) / sum(current()$values)
            alloc_chg <- ifelse(share0 <= 0, NA_real_, (share1 / share0) - 1)

            # Curve-implied allocation change % (based on MBBEFD curve)
            vals0 <- values_base()
            vals1 <- vals0 * (1 + input$dep_curr)
            cpar <- effective_c()
            w0_curve <- alloc_from_curve(vals0, input$excess_prior, prior()$limit, cpar)
            w1_curve <- alloc_from_curve(vals1, input$excess_curr, current()$limit, cpar)
            alloc_curve_chg <- alloc_shift_pct(w0_curve, w1_curve)

            # Expected loss change %
            p_el <- prior()$el
            c_el <- current()$el
            el_chg <- ifelse(p_el <= 0, NA_real_, (c_el / p_el) - 1)

            df <- data.frame(
                metric = c("Booked premium base (Limit)", "Chosen allocation shift", "Curve-implied allocation shift", "Expected loss (EL)"),
                pct = c(prem_base_chg, alloc_chg, alloc_curve_chg, el_chg)
        )

        plot_ly(
            df,
            x = ~metric,
            y = ~pct,
            type = "bar",
            hovertext = ~paste0(metric, "<br>", sprintf("%+.1f%%", 100*pct)),
            hoverinfo = "text"
        ) %>%
            layout(
            yaxis = list(title = "Change vs prior", tickformat = ".0%"),
            xaxis = list(title = ""),
            margin = list(l = 60, r = 20, t = 10, b = 90)
            )
        })
}

shiny::runApp(shiny::shinyApp(ui, server), launch.browser = TRUE)
