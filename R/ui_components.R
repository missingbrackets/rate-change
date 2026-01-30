# ==============================================================================
# R/ui_components.R - Reusable UI Building Blocks
# ==============================================================================
#
# This file contains reusable UI helper functions and components for the
# Lloyd's rate change application.
#
# ==============================================================================

#' Create a tooltip-enabled label
#'
#' Creates a span element with Bootstrap tooltip functionality.
#'
#' @param label The visible label text
#' @param text The tooltip text shown on hover
#' @return A shiny.tag object with tooltip attributes
tip <- function(label, text) {
  shiny::tags$span(
    label,
    title = text,
    `data-bs-toggle` = "tooltip",
    `data-bs-placement` = "bottom",
    style = "text-decoration: underline dotted; text-underline-offset: 3px; cursor: help;"
  )
}

#' Create a section header with description
#'
#' @param title Section title
#' @param description Optional description text (muted)
#' @return A tagList with header and optional description
section_header <- function(title, description = NULL) {
  tags <- list(shiny::h6(title))
  if (!is.null(description)) {
    tags <- c(tags, list(
      shiny::tags$small(style = "color: #6c757d;", description)
    ))
  }
  shiny::tagList(tags)
}

#' Build the Deal Inputs card UI
#'
#' @param ns Namespace function for module IDs (or identity for non-module use)
#' @return A bslib::card with all deal input components
build_deal_inputs_card <- function(ns = identity) {
  bslib::card(
    bslib::card_header(shiny::tagList(shiny::icon("sliders"), "Deal Inputs")),
    bslib::layout_column_wrap(
      width = 1 / 2,
      shinyWidgets::switchInput(ns("mode"), label = "Sensitivity mode", value = TRUE, onLabel = "On", offLabel = "Off"),
      shinyWidgets::switchInput(ns("unlock_rol"), label = "Unlock ROL", value = FALSE, onLabel = "Unlocked", offLabel = "Fixed")
    ),
    shiny::tags$hr(),

    # Schedule Exposures
    section_header("Schedule Exposures (Direct Prior/Current)", "Edit exposure values directly. No depreciation adjustment."),
    DT::DTOutput(ns("schedule_tbl")),
    shiny::tags$hr(),

    # ROL Table with Apply to All
    section_header("Rate on Line (ROL) - Satellite Level", "Per-satellite ROL for prior and current year."),
    DT::DTOutput(ns("rol_tbl")),
    bslib::layout_column_wrap(
      width = 1 / 2,
      style = "margin-top: 0.5rem;",
      shiny::tags$div(
        style = "display: flex; gap: 0.25rem; align-items: center;",
        shiny::numericInput(ns("apply_rol_value"), NULL, value = 0.0105, min = 0, max = 1, step = 0.0001, width = "100px"),
        shiny::actionButton(ns("apply_rol_prior"), "Prior", class = "btn btn-sm btn-outline-secondary"),
        shiny::actionButton(ns("apply_rol_current"), "Curr", class = "btn btn-sm btn-outline-secondary"),
        shiny::actionButton(ns("apply_rol_both"), "Both", class = "btn btn-sm btn-secondary")
      ),
      shiny::tags$small(style = "color: #6c757d; align-self: center;", "Apply ROL to all satellites")
    ),
    shiny::tags$hr(),

    # QGU Table with Apply to All
    section_header("Ground-Up Failure Rates (QGU) - Satellite Level", "Per-satellite ground-up failure rates."),
    DT::DTOutput(ns("qgu_tbl")),
    bslib::layout_column_wrap(
      width = 1 / 2,
      style = "margin-top: 0.5rem;",
      shiny::tags$div(
        style = "display: flex; gap: 0.25rem; align-items: center;",
        shiny::numericInput(ns("apply_qgu_value"), NULL, value = 0.001, min = 0, max = 1, step = 0.0001, width = "100px"),
        shiny::actionButton(ns("apply_qgu_prior"), "Prior", class = "btn btn-sm btn-outline-secondary"),
        shiny::actionButton(ns("apply_qgu_current"), "Curr", class = "btn btn-sm btn-outline-secondary"),
        shiny::actionButton(ns("apply_qgu_both"), "Both", class = "btn btn-sm btn-secondary")
      ),
      shiny::tags$small(style = "color: #6c757d; align-self: center;", "Apply QGU to all satellites")
    ),
    shiny::tags$hr(),

    # Layer Terms Table
    section_header("Layer Terms (Attachment/Limit) - Satellite Level", "Per-satellite attachment and limit for layer allocation."),
    DT::DTOutput(ns("layer_terms_tbl")),
    shiny::tags$hr(),

    # Brokerage
    shiny::h6("Brokerage / Deductions"),
    bslib::layout_column_wrap(
      width = 1 / 2,
      shiny::numericInput(ns("prior_brokerage"), "Prior Brokerage", value = DEFAULT_BROKERAGE, min = 0, max = 0.50, step = 0.005),
      shiny::numericInput(ns("current_brokerage"), "Current Brokerage", value = DEFAULT_BROKERAGE, min = 0, max = 0.50, step = 0.005)
    ),
    shiny::tags$hr(),

    # Target Loss Ratio
    shiny::h6("Target Loss Ratio"),
    shiny::sliderInput(ns("target_lr"), "Target LR", min = 0.20, max = 0.80, value = DEFAULT_TARGET_LR, step = 0.01),
    shiny::tags$hr(),

    # Exposure Curve Settings
    shiny::h6("Exposure Curve (Swiss Re / MBBEFD)"),
    shiny::selectInput(
      ns("c_mode"),
      tip("C-parameter mode", "Auto: derive c from failure rates. Manual: use dropdown."),
      choices = c("Auto from CSV" = "auto", "Manual" = "manual"),
      selected = "manual"
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'manual'", ns("c_mode")),
      ns = ns,
      shiny::selectInput(
        ns("sr_c"),
        "Swiss Re curve (c)",
        choices = CURVE_OPTIONS,
        selected = DEFAULT_C
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'auto'", ns("c_mode")),
      ns = ns,
      shiny::tags$div(
        style = "font-size: 0.85rem; color: #6c757d; padding: 0.5rem; background: rgba(0,0,0,0.03); border-radius: 0.5rem;",
        shiny::tags$p(style = "margin-bottom: 0.25rem;", shiny::tags$b("Auto c-parameter:")),
        shiny::tags$p(style = "margin-bottom: 0;",
          "Portfolio c is computed as value-weighted average of per-satellite c values."
        ),
        shiny::uiOutput(ns("auto_c_display"))
      )
    ),
    shiny::tags$hr(),

    # Bulk paste button
    shiny::actionButton(ns("open_bulk_paste"), "Bulk Paste Data...", class = "btn btn-outline-info w-100 mb-2", icon = shiny::icon("paste")),

    # Reset buttons
    bslib::layout_column_wrap(
      width = 1 / 2,
      shiny::actionButton(ns("reset_defaults"), "Reset to Defaults", class = "btn btn-outline-primary w-100"),
      shiny::actionButton(ns("reset_current"), "Copy Prior to Current", class = "btn btn-primary w-100")
    )
  )
}

#' Build the PMDR Adjustments accordion UI
#'
#' @param ns Namespace function for module IDs (or identity for non-module use)
#' @return A bslib::accordion with PMDR adjustment panels
build_pmdr_accordion <- function(ns = identity) {
  bslib::accordion(
    id = ns("pmdr_accordion"),
    bslib::accordion_panel(
      title = shiny::tagList(shiny::icon("chart-waterfall"), " PMDR Adjustments"),
      value = "pmdr_adjustments",
      shiny::tags$div(
        style = "font-size: 0.92rem;",
        shiny::tags$p(
          style = "color: #6c757d; margin-bottom: 0.75rem;",
          "Lloyd's PMDR manual adjustments for breadth of cover and other factors."
        ),
        shiny::sliderInput(
          ns("breadth_of_cover_change"),
          tip("Breadth of Cover Change", "Premium impact from adding/removing perils."),
          min = PMDR_ADJUSTMENT_MIN, max = PMDR_ADJUSTMENT_MAX, value = 0, step = PMDR_ADJUSTMENT_STEP, pre = "\u00a3"
        ),
        shiny::sliderInput(
          ns("other_exposure_change"),
          tip("Other Exposure Change (manual)", "Manual premium adjustment for other factors."),
          min = PMDR_ADJUSTMENT_MIN, max = PMDR_ADJUSTMENT_MAX, value = 0, step = PMDR_ADJUSTMENT_STEP, pre = "\u00a3"
        ),
        shiny::uiOutput(ns("auto_exposure_change_display")),
        shiny::tags$hr(),
        shiny::tags$p(
          style = "color: #6c757d; font-size: 0.85rem;",
          "Deductions Change is computed automatically from brokerage difference."
        )
      )
    ),
    bslib::accordion_panel(
      title = shiny::tagList(shiny::icon("heart-pulse"), " Risk View (EL, LR, Benchmark)"),
      value = "risk_view",
      shiny::tags$div(
        style = "font-size: 0.92rem;",
        shiny::uiOutput(ns("risk_cards")),
        plotly::plotlyOutput(ns("risk_line"), height = 200)
      )
    ),
    bslib::accordion_panel(
      title = shiny::tagList(shiny::icon("calculator"), " Actuarial Formulas"),
      value = "actuarial_detail",
      shiny::tags$div(
        style = "font-size: 0.95rem;",
        shiny::tags$h6("Expected Loss Formula (Consistent Everywhere)"),
        shiny::tags$pre("EL = \u03a3(q_gu_i \u00d7 Exposure_i \u00d7 LayerAllocation_i)"),
        shiny::tags$h6("Premium (Satellite-level ROL)"),
        shiny::tags$pre("Gross = \u03a3(ROL_i \u00d7 ValueInLayer_i)\nNet = Gross \u00d7 (1 \u2212 Brokerage)"),
        shiny::tags$h6("Layer Allocation"),
        shiny::tags$pre("Allocation_i = (LayerShare_i \u00d7 Value_i) / \u03a3(...)"),
        shiny::tags$h6("Implied LR"),
        shiny::tags$pre("LR = EL / NetPremium"),
        shiny::tags$h6("Rate Change (from Loss Ratios)"),
        shiny::tags$pre("RateChange = LR_prior / LR_current \u2212 1"),
        shiny::tags$p(
          style = "font-size: 0.8rem; color: #6c757d;",
          "Positive rate change = prices increased = LR decreased"
        )
      )
    ),
    open = FALSE
  )
}

#' Build the main content KPI row
#'
#' @param ns Namespace function for module IDs
#' @return A layout_column_wrap with KPI value boxes
build_kpi_row <- function(ns = identity) {
  bslib::layout_column_wrap(
    width = 1 / 4,
    gap = "1rem",
    bslib::value_box(
      title = tip("Total TIV", "Sum of satellite schedule values."),
      value = shiny::uiOutput(ns("kpi_tiv")),
      showcase = shiny::icon("satellite-dish"),
      theme_color = "primary"
    ),
    bslib::value_box(
      title = tip("Layer Limit", "Limit derived from max value minus attachment."),
      value = shiny::uiOutput(ns("kpi_limit")),
      showcase = shiny::icon("layer-group"),
      theme_color = "info"
    ),
    bslib::value_box(
      title = tip("Premium (gross / net)", "Gross = ROL\u00d7Limit; Net = Gross\u00d7(1\u2212brokerage)."),
      value = shiny::uiOutput(ns("kpi_prem")),
      showcase = shiny::icon("coins"),
      theme_color = "success"
    ),
    bslib::value_box(
      title = tip("Expected Loss", "EL = \u03a3(q_gu \u00d7 exposure \u00d7 allocation)."),
      value = shiny::uiOutput(ns("kpi_el")),
      showcase = shiny::icon("triangle-exclamation"),
      theme_color = "warning"
    )
  )
}

#' Build the charts row
#'
#' @param ns Namespace function for module IDs
#' @return A layout_column_wrap with chart cards
build_charts_row <- function(ns = identity) {
  bslib::layout_column_wrap(
    width = 1 / 2,
    gap = "1rem",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::tagList(shiny::icon("chart-bar"), "Lloyd's Rate Change Analysis")),
      bslib::navset_tab(
        bslib::nav_panel(
          "PMDR Waterfall",
          plotly::plotlyOutput(ns("lloyds_waterfall"), height = 360),
          shiny::tags$div(
            style = "font-size:0.95rem; color:#5a6b7b;",
            "Lloyd's PMDR decomposition with Deductions Change component"
          )
        ),
        bslib::nav_panel(
          "Loss vs Premium",
          plotly::plotlyOutput(ns("loss_premium_comparison"), height = 360)
        ),
        bslib::nav_panel(
          "MBBEFD Curve",
          plotly::plotlyOutput(ns("ec_curve"), height = 360)
        ),
        bslib::nav_panel(
          "Per-Satellite Curves",
          plotly::plotlyOutput(ns("per_satellite_curves"), height = 360),
          shiny::tags$div(
            style = "font-size:0.85rem; color:#5a6b7b;",
            "Each satellite has its own c parameter derived from its failure rate"
          )
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::tagList(shiny::icon("gauge-high"), "RARC (Pure Rate Change) Index")),
      plotly::plotlyOutput(ns("rarc_gauge"), height = 270)
    )
  )
}

#' Build the allocation and decomposition row
#'
#' @param ns Namespace function for module IDs
#' @return A layout_column_wrap with allocation table and Lloyd's decomposition
build_allocation_risk_row <- function(ns = identity) {
  bslib::layout_column_wrap(
    width = 1 / 2,
    gap = "1rem",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::tagList(shiny::icon("table-cells"), "Layer Allocation by Satellite")),
      DT::DTOutput(ns("layer_allocation_tbl")),
      shiny::tags$div(
        style = "font-size:0.85rem; color:#5a6b7b; margin-top:0.5rem;",
        "Shows layer allocation weights and expected loss contribution per satellite."
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::tagList(shiny::icon("table"), "Lloyd's Decomposition")),
      shiny::uiOutput(ns("lloyds_decomp_table")),
      shiny::tags$div(
        style = "font-size:0.85rem; color:#5a6b7b; margin-top:0.5rem;",
        "PMDR breakdown showing prior-to-current premium change components."
      )
    )
  )
}

#' Build the per-satellite curve diagnostics row
#'
#' @param ns Namespace function for module IDs
#' @return A layout_column_wrap with curve diagnostics table and share comparison
build_curve_diagnostics_row <- function(ns = identity) {
  bslib::layout_column_wrap(
    width = 1 / 2,
    gap = "1rem",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::tagList(shiny::icon("chart-area"), "Per-Satellite Curve Diagnostics")),
      DT::DTOutput(ns("curve_diagnostics_tbl")),
      shiny::tags$div(
        style = "font-size:0.85rem; color:#5a6b7b; margin-top:0.5rem;",
        "Curve parameters (c_i), bounds (d, u), and layer shares per satellite"
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::tagList(shiny::icon("chart-column"), "Layer Share Comparison")),
      plotly::plotlyOutput(ns("layer_share_comparison"), height = 280),
      shiny::tags$div(
        style = "font-size:0.85rem; color:#5a6b7b; margin-top:0.5rem;",
        "Prior vs Current layer shares using per-satellite c parameters"
      )
    )
  )
}

#' Get Bootstrap tooltip initialization script
#'
#' @return HTML script tag for tooltip initialization
tooltip_init_script <- function() {
  shiny::tags$script(shiny::HTML("
    document.addEventListener('shown.bs.modal', () => {
      [...document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')].forEach(el => new bootstrap.Tooltip(el));
    });
    document.addEventListener('DOMContentLoaded', () => {
      [...document.querySelectorAll('[data-bs-toggle=\"tooltip\"]')].forEach(el => new bootstrap.Tooltip(el));
    });
  "))
}

#' Build the bulk paste modal
#'
#' @param ns Namespace function for module IDs
#' @return A modalDialog for bulk data entry
build_bulk_paste_modal <- function(ns = identity) {
  shiny::modalDialog(
    title = shiny::tagList(shiny::icon("paste"), " Bulk Paste Data"),
    size = "l",
    easyClose = TRUE,
    shiny::tags$p(
      style = "color: #6c757d;",
      "Paste tab-separated or comma-separated data. Format: Satellite, Prior Value, Current Value"
    ),
    shiny::selectInput(
      ns("bulk_paste_target"),
      "Target Table",
      choices = c(
        "Schedule Exposures" = "schedule",
        "Rate on Line (ROL)" = "rol",
        "Failure Rates (QGU)" = "qgu"
      ),
      selected = "schedule"
    ),
    shiny::tags$div(
      style = "margin-bottom: 0.5rem;",
      shiny::tags$b("Example format:"),
      shiny::tags$pre(
        style = "font-size: 0.8rem; background: #f8f9fa; padding: 0.5rem; border-radius: 4px;",
        "V1, 6500000000, 6600000000\nV2, 6700000000, 6800000000\nV3, 6200000000, 6300000000\nV4, 8400000000, 8500000000"
      )
    ),
    shiny::textAreaInput(
      ns("bulk_paste_data"),
      "Paste Data Here",
      value = "",
      rows = 8,
      width = "100%",
      placeholder = "Paste your data here (CSV or tab-separated)..."
    ),
    shiny::uiOutput(ns("bulk_paste_preview")),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("apply_bulk_paste"), "Apply Data", class = "btn btn-primary", icon = shiny::icon("check"))
    )
  )
}

# ==============================================================================
# END R/ui_components.R
# ==============================================================================
