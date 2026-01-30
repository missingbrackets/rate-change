# ==============================================================================
# R/mod_visualizations.R - Chart Rendering Functions
# ==============================================================================
#
# This module contains all chart rendering functions for plotly visualizations:
# - RARC Gauge
# - PMDR Waterfall
# - Loss vs Premium Comparison
# - MBBEFD Exposure Curve
# - Risk Line Chart
#
# ==============================================================================

#' Render RARC gauge chart
#'
#' @param rarc RARC index value (1.0 = flat, >1 = increase)
#' @return A plotly gauge chart
render_rarc_gauge <- function(rarc) {
  rarc_pct <- if (is.na(rarc)) NA_real_ else rarc * 100

  label <- if (is.na(rarc)) {
    "\u2014"
  } else if (rarc > 1.005) {
    "Rate Increase"
  } else if (rarc < 0.995) {
    "Rate Decrease"
  } else {
    "Flat Rate"
  }

  bar_color <- if (is.na(rarc)) {
    "#6c757d"
  } else if (rarc >= 1) {
    "#28a745"
  } else {
    "#dc3545"
  }

  plotly::plot_ly(
    type = "indicator",
    mode = "gauge+number+delta",
    value = if (is.na(rarc_pct)) 100 else rarc_pct,
    number = list(suffix = "%", valueformat = ".1f"),
    delta = list(reference = 100, valueformat = ".1f", suffix = " pts"),
    title = list(
      text = paste0("<b>RARC Index</b><br><span style='font-size:0.8em;color:#666'>", label, "</span>"),
      font = list(size = 14)
    ),
    gauge = list(
      axis = list(range = list(80, 120), ticksuffix = "%"),
      bar = list(color = bar_color),
      threshold = list(line = list(color = "#333", width = 4), thickness = 0.85, value = 100),
      steps = list(
        list(range = c(80, 95), color = "#f8d7da"),
        list(range = c(95, 100), color = "#fff3cd"),
        list(range = c(100, 105), color = "#d4edda"),
        list(range = c(105, 120), color = "#28a745")
      )
    )
  ) %>%
    plotly::layout(
      margin = list(l = 30, r = 30, t = 60, b = 10),
      annotations = list(
        list(
          x = 0.5, y = -0.15, xref = "paper", yref = "paper",
          text = "RARC = (Prior exposure @ Current ROL) / (Prior exposure @ Prior ROL)",
          showarrow = FALSE, font = list(size = 10, color = "#666")
        )
      )
    )
}

#' Format value for waterfall label
#'
#' @param x Numeric value
#' @return Formatted string with sign prefix
fmt_waterfall_label <- function(x) {
  if (is.na(x)) return("\u2014")
  prefix <- if (x >= 0) "+" else ""
  paste0(prefix, "\u00a3", format(round(x / 1e6, 2), nsmall = 2), "m")
}

#' Render PMDR waterfall chart
#'
#' @param decomp Lloyd's decomposition results list
#' @return A plotly waterfall chart
render_pmdr_waterfall <- function(decomp) {
  steps <- c(
    "Prior<br>Premium",
    "1. Attachment<br>Change",
    "2. Breadth<br>of Cover",
    "3. Other<br>Factors",
    "4. Deductions<br>Change",
    "5. Pure Rate<br>(RARC)",
    "Current<br>Premium"
  )

  values <- c(
    decomp$prior_gross,
    decomp$delta_attachment,
    decomp$delta_breadth,
    decomp$delta_other,
    decomp$delta_deductions,
    decomp$delta_pure_rate,
    decomp$current_gross
  )

  measure <- c("absolute", "relative", "relative", "relative", "relative", "relative", "total")

  text_labels <- c(
    paste0("\u00a3", format(round(decomp$prior_gross / 1e6, 2), nsmall = 2), "m"),
    fmt_waterfall_label(decomp$delta_attachment),
    fmt_waterfall_label(decomp$delta_breadth),
    fmt_waterfall_label(decomp$delta_other),
    fmt_waterfall_label(decomp$delta_deductions),
    fmt_waterfall_label(decomp$delta_pure_rate),
    paste0("\u00a3", format(round(decomp$current_gross / 1e6, 2), nsmall = 2), "m")
  )

  plotly::plot_ly(
    type = "waterfall",
    x = steps,
    y = values,
    measure = measure,
    text = text_labels,
    textposition = "inside",
    textfont = list(color = "white", size = 9),
    hoverinfo = "text",
    hovertext = c(
      paste0("Prior Gross: \u00a3", format(round(decomp$prior_gross), big.mark = ",")),
      paste0("Attachment Change: ", fmt_waterfall_label(decomp$delta_attachment)),
      paste0("Breadth Change: ", fmt_waterfall_label(decomp$delta_breadth)),
      paste0("Other Factors: ", fmt_waterfall_label(decomp$delta_other)),
      paste0("Deductions Change: ", fmt_waterfall_label(decomp$delta_deductions)),
      paste0("Pure Rate (RARC): ", fmt_waterfall_label(decomp$delta_pure_rate)),
      paste0("Current Gross: \u00a3", format(round(decomp$current_gross), big.mark = ","))
    ),
    connector = list(line = list(color = "#aaa", width = 1)),
    increasing = list(marker = list(color = "#27ae60")),
    decreasing = list(marker = list(color = "#e74c3c")),
    totals = list(marker = list(color = "#2c3e50"))
  ) %>%
    plotly::layout(
      title = list(text = "<b>Lloyd's PMDR Premium Decomposition</b>", font = list(size = 14), x = 0.5),
      xaxis = list(title = "", tickfont = list(size = 9)),
      yaxis = list(title = "Gross Premium (\u00a3)", tickformat = ",.0f", tickprefix = "\u00a3"),
      margin = list(l = 80, r = 20, t = 50, b = 100),
      showlegend = FALSE
    )
}

#' Render loss vs premium comparison chart
#'
#' @param decomp Lloyd's decomposition results list
#' @param prior Prior scenario results list
#' @param current Current scenario results list
#' @return A plotly grouped bar chart
render_loss_premium_comparison <- function(decomp, prior, current) {
  metrics <- c("Expected Loss", "Gross Premium", "Net Premium", "Benchmark")
  prior_vals <- c(decomp$el_prior, decomp$prior_gross, decomp$prior_net, decomp$benchmark_gross)
  current_vals <- c(decomp$el_current, decomp$current_gross, decomp$current_net, decomp$benchmark_gross)

  plotly::plot_ly() %>%
    plotly::add_bars(
      x = metrics, y = prior_vals / 1e6, name = "Prior",
      marker = list(color = "#3498db"),
      text = paste0("\u00a3", round(prior_vals / 1e6, 2), "m"),
      textposition = "outside"
    ) %>%
    plotly::add_bars(
      x = metrics, y = current_vals / 1e6, name = "Current",
      marker = list(color = "#e74c3c"),
      text = paste0("\u00a3", round(current_vals / 1e6, 2), "m"),
      textposition = "outside"
    ) %>%
    plotly::layout(
      title = list(text = "<b>Prior vs Current: Loss & Premium</b>", font = list(size = 14), x = 0.5),
      xaxis = list(title = ""),
      yaxis = list(title = "Value (\u00a3m)", tickformat = ",.1f", tickprefix = "\u00a3", ticksuffix = "m"),
      barmode = "group",
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      margin = list(l = 70, r = 20, t = 50, b = 70),
      annotations = list(
        list(
          x = 0.5, y = 1.08, xref = "paper", yref = "paper",
          text = paste0(
            "LR: Prior ", sprintf("%.1f%%", prior$lr * 100),
            " \u2192 Current ", sprintf("%.1f%%", current$lr * 100),
            " | Adequacy: ", sprintf("%.1f%%", decomp$price_adequacy * 100)
          ),
          showarrow = FALSE, font = list(size = 11, color = "#666")
        )
      )
    )
}

#' Render MBBEFD exposure curve chart
#'
#' @param prior Prior scenario results list
#' @param current Current scenario results list
#' @param c_param Swiss Re curve parameter
#' @param attachment_prior Prior attachment point
#' @param attachment_current Current attachment point
#' @return A plotly exposure curve chart
render_ec_curve <- function(prior, current, c_param, attachment_prior, attachment_current) {
  MPL0 <- max(prior$values)
  MPL1 <- max(current$values)
  L0 <- prior$limit
  L1 <- current$limit
  A0 <- attachment_prior
  A1 <- attachment_current

  s0 <- layer_share_from_curve(A0, L0, MPL0, c_param)
  s1 <- layer_share_from_curve(A1, L1, MPL1, c_param)

  xs <- seq(0, 1, length.out = 200)
  Gs <- G_mbbefd(xs, c_param)
  df_curve <- data.frame(x = xs, G = Gs)

  pts <- data.frame(
    scen = c("Prior", "Prior", "Current", "Current"),
    x = c(s0$d, s0$u, s1$d, s1$u),
    y = c(s0$Gd, s0$Gu, s1$Gd, s1$Gu),
    lab = c("d (att/MPL)", "u ((att+lim)/MPL)", "d (att/MPL)", "u ((att+lim)/MPL)")
  )

  ann <- paste0(
    "Prior layer share: ", sprintf("%.1f%%", 100 * s0$share),
    "<br>Current layer share: ", sprintf("%.1f%%", 100 * s1$share)
  )

  plotly::plot_ly() %>%
    plotly::add_lines(data = df_curve, x = ~x, y = ~G, name = "G(x)", hoverinfo = "skip") %>%
    plotly::add_markers(
      data = pts, x = ~x, y = ~y, color = ~scen, symbol = ~lab,
      hovertext = ~paste0(scen, "<br>", lab, "<br>x=", sprintf("%.3f", x), "<br>G(x)=", sprintf("%.3f", y)),
      hoverinfo = "text", marker = list(size = 9)
    ) %>%
    plotly::layout(
      xaxis = list(title = "Deductible as % of MPL (x)"),
      yaxis = list(title = "Exposure curve G(x)"),
      margin = list(l = 60, r = 20, t = 10, b = 50),
      annotations = list(
        list(x = 0.02, y = 0.98, xref = "paper", yref = "paper",
             text = ann, showarrow = FALSE, align = "left")
      ),
      legend = list(orientation = "h", x = 0, y = -0.2)
    )
}

#' Render risk line chart (LR trend)
#'
#' @param prior Prior scenario results list
#' @param current Current scenario results list
#' @param target_lr Target loss ratio
#' @return A plotly line chart
render_risk_line <- function(prior, current, target_lr) {
  df <- data.frame(
    scenario = c("Prior", "Current"),
    lr = c(prior$lr, current$lr)
  )

  plotly::plot_ly(df, x = ~scenario, y = ~lr, type = "scatter", mode = "lines+markers",
                  hovertext = ~paste0("Implied LR: ", if (is.na(lr)) "\u2014" else fmt_pct(lr, 1)),
                  hoverinfo = "text", name = "Implied LR") %>%
    plotly::add_lines(x = df$scenario, y = rep(target_lr, 2), name = "Target LR",
                      hovertext = paste0("Target: ", fmt_pct(target_lr, 1)),
                      hoverinfo = "text", line = list(dash = "dot")) %>%
    plotly::layout(
      yaxis = list(title = "Loss ratio", tickformat = ".0%"),
      xaxis = list(title = ""),
      margin = list(l = 60, r = 20, t = 10, b = 50),
      legend = list(orientation = "h", x = 0, y = -0.3)
    )
}

# ==============================================================================
# Per-Satellite Exposure Curve Functions
# ==============================================================================

#' Render per-satellite MBBEFD exposure curves
#'
#' Shows separate exposure curves for prior and current scenarios per satellite.
#' Prior curves use prior q_gu (solid lines), current curves use current q_gu (dashed).
#'
#' @param diag_prior Data frame from compute_per_satellite_curve_diagnostics (prior)
#' @param diag_current Data frame from compute_per_satellite_curve_diagnostics (current)
#' @param sat_names Vector of satellite names
#' @return A plotly chart with prior and current curves per satellite
render_per_satellite_curves <- function(diag_prior, diag_current, sat_names) {
  # Define colors for satellites
  colors <- c("#3498db", "#e74c3c", "#27ae60", "#9b59b6", "#f39c12", "#1abc9c")

  # Create base plot
  p <- plotly::plot_ly()

  # Generate x values for curve plotting
  xs <- seq(0, 1, length.out = 100)

  # Add curves for each satellite - both prior and current

  for (i in seq_along(sat_names)) {
    sat_color <- colors[(i - 1) %% length(colors) + 1]

    # Prior curve (solid line) using prior c value
    c_prior <- diag_prior$c_i[i]
    Gs_prior <- G_mbbefd(xs, c_prior)

    p <- p %>% plotly::add_lines(
      x = xs, y = Gs_prior,
      name = paste0(sat_names[i], " Prior (c=", round(c_prior, 2), ")"),
      line = list(color = sat_color, width = 2, dash = "solid"),
      legendgroup = sat_names[i],
      hoverinfo = "text",
      hovertext = paste0(sat_names[i], " Prior<br>c=", round(c_prior, 3),
                         "<br>x=", round(xs, 3), "<br>G(x)=", round(Gs_prior, 3))
    )

    # Current curve (dashed line) using current c value
    c_current <- diag_current$c_i[i]
    Gs_current <- G_mbbefd(xs, c_current)

    # Only add current curve if c differs from prior (avoid duplicate lines)
    if (abs(c_current - c_prior) > 0.001) {
      p <- p %>% plotly::add_lines(
        x = xs, y = Gs_current,
        name = paste0(sat_names[i], " Current (c=", round(c_current, 2), ")"),
        line = list(color = sat_color, width = 2, dash = "dash"),
        legendgroup = sat_names[i],
        hoverinfo = "text",
        hovertext = paste0(sat_names[i], " Current<br>c=", round(c_current, 3),
                           "<br>x=", round(xs, 3), "<br>G(x)=", round(Gs_current, 3))
      )
    }

    # Add prior layer points (circles) on prior curve
    p <- p %>% plotly::add_markers(
      x = c(diag_prior$d[i], diag_prior$u[i]),
      y = c(diag_prior$Gd[i], diag_prior$Gu[i]),
      name = paste0(sat_names[i], " Prior pts"),
      marker = list(color = sat_color, symbol = "circle", size = 8),
      legendgroup = sat_names[i],
      showlegend = FALSE,
      hoverinfo = "text",
      hovertext = c(
        paste0(sat_names[i], " Prior<br>d=", sprintf("%.3f", diag_prior$d[i]),
               "<br>G(d)=", sprintf("%.3f", diag_prior$Gd[i]),
               "<br>Share=", sprintf("%.2f%%", diag_prior$Share[i] * 100)),
        paste0(sat_names[i], " Prior<br>u=", sprintf("%.3f", diag_prior$u[i]),
               "<br>G(u)=", sprintf("%.3f", diag_prior$Gu[i]))
      )
    )

    # Add current layer points (diamonds) on current curve
    p <- p %>% plotly::add_markers(
      x = c(diag_current$d[i], diag_current$u[i]),
      y = c(diag_current$Gd[i], diag_current$Gu[i]),
      name = paste0(sat_names[i], " Current pts"),
      marker = list(color = sat_color, symbol = "diamond", size = 10),
      legendgroup = sat_names[i],
      showlegend = FALSE,
      hoverinfo = "text",
      hovertext = c(
        paste0(sat_names[i], " Current<br>d=", sprintf("%.3f", diag_current$d[i]),
               "<br>G(d)=", sprintf("%.3f", diag_current$Gd[i]),
               "<br>Share=", sprintf("%.2f%%", diag_current$Share[i] * 100)),
        paste0(sat_names[i], " Current<br>u=", sprintf("%.3f", diag_current$u[i]),
               "<br>G(u)=", sprintf("%.3f", diag_current$Gu[i]))
      )
    )
  }

  p %>% plotly::layout(
    title = list(text = "<b>Per-Satellite MBBEFD Curves (Prior vs Current)</b>", font = list(size = 14), x = 0.5),
    xaxis = list(title = "Deductible as % of MPL (x)", range = c(0, 1)),
    yaxis = list(title = "Exposure curve G(x)", range = c(0, 1)),
    margin = list(l = 60, r = 20, t = 50, b = 80),
    legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 8)),
    annotations = list(
      list(
        x = 0.02, y = 0.02, xref = "paper", yref = "paper",
        text = "Solid = Prior curve | Dashed = Current curve | Circle = Prior pts | Diamond = Current pts",
        showarrow = FALSE, font = list(size = 8, color = "#666")
      )
    )
  )
}

#' Render per-satellite curve diagnostics table
#'
#' Creates a DT datatable showing curve diagnostics for each satellite.
#'
#' @param diag_prior Data frame from compute_per_satellite_curve_diagnostics (prior)
#' @param diag_current Data frame from compute_per_satellite_curve_diagnostics (current)
#' @return A DT datatable with per-satellite curve metrics
render_per_satellite_curve_table <- function(diag_prior, diag_current) {
  # Combine prior and current into a comparison table
  df <- data.frame(
    Satellite = diag_prior$Satellite,
    Prior_c = diag_prior$c_i,
    Current_c = diag_current$c_i,
    Prior_d = diag_prior$d,
    Current_d = diag_current$d,
    Prior_Gd = diag_prior$Gd,
    Current_Gd = diag_current$Gd,
    Prior_Gu = diag_prior$Gu,
    Current_Gu = diag_current$Gu,
    Prior_Share = diag_prior$Share,
    Current_Share = diag_current$Share,
    Share_Change = diag_current$Share - diag_prior$Share,
    stringsAsFactors = FALSE
  )

  DT::datatable(
    df,
    colnames = c(
      "Sat", "Prior c", "Curr c",
      "Prior d", "Curr d",
      "Prior G(d)", "Curr G(d)",
      "Prior G(u)", "Curr G(u)",
      "Prior Share", "Curr Share", "\u0394 Share"
    ),
    rownames = FALSE,
    options = list(
      dom = "t",
      pageLength = 10,
      ordering = FALSE,
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-center", targets = "_all")
      )
    )
  ) %>%
    DT::formatRound(c("Prior_c", "Current_c"), digits = 2) %>%
    DT::formatRound(c("Prior_d", "Current_d", "Prior_Gd", "Current_Gd", "Prior_Gu", "Current_Gu"), digits = 3) %>%
    DT::formatPercentage(c("Prior_Share", "Current_Share", "Share_Change"), digits = 1)
}

#' Render layer share comparison bar chart
#'
#' Shows prior vs current layer shares per satellite as a grouped bar chart.
#'
#' @param diag_prior Data frame from compute_per_satellite_curve_diagnostics (prior)
#' @param diag_current Data frame from compute_per_satellite_curve_diagnostics (current)
#' @return A plotly grouped bar chart
render_layer_share_comparison <- function(diag_prior, diag_current) {
  plotly::plot_ly() %>%
    plotly::add_bars(
      x = diag_prior$Satellite,
      y = diag_prior$Share * 100,
      name = "Prior",
      marker = list(color = "#3498db"),
      text = paste0(round(diag_prior$Share * 100, 1), "%"),
      textposition = "outside",
      hoverinfo = "text",
      hovertext = paste0(
        diag_prior$Satellite, " Prior<br>",
        "Share: ", sprintf("%.2f%%", diag_prior$Share * 100), "<br>",
        "c: ", round(diag_prior$c_i, 2)
      )
    ) %>%
    plotly::add_bars(
      x = diag_current$Satellite,
      y = diag_current$Share * 100,
      name = "Current",
      marker = list(color = "#e74c3c"),
      text = paste0(round(diag_current$Share * 100, 1), "%"),
      textposition = "outside",
      hoverinfo = "text",
      hovertext = paste0(
        diag_current$Satellite, " Current<br>",
        "Share: ", sprintf("%.2f%%", diag_current$Share * 100), "<br>",
        "c: ", round(diag_current$c_i, 2)
      )
    ) %>%
    plotly::layout(
      title = list(text = "<b>Layer Share by Satellite</b>", font = list(size = 14), x = 0.5),
      xaxis = list(title = ""),
      yaxis = list(title = "Layer Share (%)", ticksuffix = "%"),
      barmode = "group",
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      margin = list(l = 60, r = 20, t = 50, b = 50)
    )
}

# ==============================================================================
# END R/mod_visualizations.R
# ==============================================================================
