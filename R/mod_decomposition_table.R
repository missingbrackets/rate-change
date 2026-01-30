# ==============================================================================
# R/mod_decomposition_table.R - PMDR Decomposition Table Rendering
# ==============================================================================
#
# This module contains functions to render the Lloyd's PMDR decomposition
# verification tables in the sidebar accordion.
#
# ==============================================================================

#' Format value for decomposition table
#'
#' @param x Numeric value
#' @param prefix Currency prefix
#' @return Formatted string
fmt_decomp_value <- function(x, prefix = "\u00a3") {
  if (is.na(x)) return("\u2014")
  paste0(prefix, format(round(x), big.mark = ",", scientific = FALSE))
}

#' Format percentage for decomposition table
#'
#' @param x Numeric value (0-1 scale)
#' @return Formatted percentage string
fmt_decomp_pct <- function(x) {
  if (is.na(x)) return("\u2014")
  sprintf("%.2f%%", x * 100)
}

#' Format delta value with sign
#'
#' @param x Numeric value
#' @return Formatted string with +/- prefix
fmt_decomp_delta <- function(x) {
  if (is.na(x)) return("\u2014")
  sign <- if (x >= 0) "+" else ""
  paste0(sign, fmt_decomp_value(x))
}

#' Render the full Lloyd's decomposition verification UI
#'
#' @param decomp Lloyd's decomposition results list
#' @param rarc RARC index value
#' @return A shiny tagList with decomposition tables
render_lloyds_decomp_table <- function(decomp, rarc) {
  shiny::tags$div(
    style = "font-size: 0.85rem;",

    # PMDR Decomposition Table
    shiny::tags$h6("PMDR Decomposition", style = "margin-top: 0;"),
    shiny::tags$table(
      class = "table table-sm table-striped",
      style = "margin-bottom: 1rem;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Component"),
          shiny::tags$th(style = "text-align: right;", "Value")
        )
      ),
      shiny::tags$tbody(
        shiny::tags$tr(
          shiny::tags$td("Prior Gross Premium"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_value(decomp$prior_gross))
        ),
        shiny::tags$tr(
          style = "background: #e3f2fd;",
          shiny::tags$td("1. Deductible/Attachment Change"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_delta(decomp$delta_attachment))
        ),
        shiny::tags$tr(
          style = "background: #e3f2fd;",
          shiny::tags$td("2. Breadth of Cover Change"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_delta(decomp$delta_breadth))
        ),
        shiny::tags$tr(
          style = "background: #e3f2fd;",
          shiny::tags$td("3. Other Exposure Change"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_delta(decomp$delta_other))
        ),
        shiny::tags$tr(
          style = "background: #fce4ec;",
          shiny::tags$td("4. Deductions Change"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_delta(decomp$delta_deductions))
        ),
        shiny::tags$tr(
          style = "background: #fff3cd; font-weight: bold;",
          shiny::tags$td("5. Pure Rate Change (RARC)"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_delta(decomp$delta_pure_rate))
        ),
        shiny::tags$tr(
          style = "font-weight: bold; border-top: 2px solid #333;",
          shiny::tags$td("Current Gross Premium"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_value(decomp$current_gross))
        ),
        shiny::tags$tr(
          style = "color: #6c757d;",
          shiny::tags$td("Total Change"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_delta(decomp$total_change))
        )
      )
    ),

    # Benchmark & Price Adequacy Table
    shiny::tags$h6("Benchmark & Price Adequacy"),
    shiny::tags$table(
      class = "table table-sm",
      style = "margin-bottom: 1rem;",
      shiny::tags$tbody(
        shiny::tags$tr(
          shiny::tags$td("Expected Loss (current)"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_value(decomp$el_current))
        ),
        shiny::tags$tr(
          shiny::tags$td("Benchmark Gross Premium"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_value(decomp$benchmark_gross))
        ),
        shiny::tags$tr(
          style = "font-weight: bold;",
          shiny::tags$td("Price Adequacy"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_pct(decomp$price_adequacy))
        )
      )
    ),

    # RARC Index Table
    shiny::tags$h6("RARC Index"),
    shiny::tags$table(
      class = "table table-sm",
      shiny::tags$tbody(
        shiny::tags$tr(
          style = "font-weight: bold; background: #d4edda;",
          shiny::tags$td("RARC Index"),
          shiny::tags$td(style = "text-align: right;", fmt_decomp_pct(rarc))
        )
      )
    ),

    # Verification status
    if (decomp$decomp_error > 0.01) {
      shiny::tags$div(
        style = "color: #dc3545; font-size: 0.8rem;",
        shiny::icon("exclamation-triangle"),
        sprintf(" Decomposition error: %s", fmt_decomp_value(decomp$decomp_error))
      )
    } else {
      shiny::tags$div(
        style = "color: #28a745; font-size: 0.8rem;",
        shiny::icon("check-circle"),
        " Decomposition verified"
      )
    }
  )
}

#' Render auto c-parameter display
#'
#' @param qgu_current Current failure rates vector
#' @param values_current Current exposure values vector
#' @param sat_names Satellite names vector
#' @param c_params_csv C-parameters lookup data frame
#' @return A shiny tagList with c-parameter table
render_auto_c_display <- function(qgu_current, values_current, sat_names, c_params_csv) {
  c_per_sat <- compute_per_satellite_c(qgu_current, c_params_csv)
  port_c <- compute_portfolio_c(qgu_current, values_current, c_params_csv)

  sat_df <- data.frame(
    Satellite = sat_names,
    q_gu = qgu_current,
    c_i = round(c_per_sat, 3)
  )

  shiny::tags$div(
    style = "margin-top: 0.5rem;",
    shiny::tags$table(
      class = "table table-sm",
      style = "font-size: 0.8rem; margin-bottom: 0.5rem;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Sat"),
          shiny::tags$th("QGU"),
          shiny::tags$th("c_i")
        )
      ),
      shiny::tags$tbody(
        lapply(seq_len(nrow(sat_df)), function(i) {
          shiny::tags$tr(
            shiny::tags$td(sat_df$Satellite[i]),
            shiny::tags$td(sprintf("%.4f%%", sat_df$q_gu[i] * 100)),
            shiny::tags$td(sprintf("%.3f", sat_df$c_i[i]))
          )
        })
      )
    ),
    shiny::tags$div(
      style = "font-weight: bold; color: #2c3e50;",
      sprintf("Portfolio c (value-weighted): %.3f", port_c)
    )
  )
}

#' Render auto exposure change display
#'
#' @param auto_delta Auto-computed exposure change delta
#' @return A shiny tags$div with the formatted value
render_auto_exposure_change_display <- function(auto_delta) {
  shiny::tags$div(
    style = "font-size: 0.85rem; color: #6c757d; margin-top: 0.25rem;",
    paste0("Auto exposure impact from schedule change: ", fmt_money(auto_delta))
  )
}

# ==============================================================================
# END R/mod_decomposition_table.R
# ==============================================================================
