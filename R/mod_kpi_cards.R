# ==============================================================================
# R/mod_kpi_cards.R - KPI Value Box Rendering Functions
# ==============================================================================
#
# This module contains functions to render the KPI value boxes and risk view
# cards with consistent formatting.
#
# ==============================================================================

#' Render TIV KPI card content
#'
#' @param prior_tiv Prior total insured value
#' @param current_tiv Current total insured value
#' @return HTML content for the TIV value box
render_kpi_tiv <- function(prior_tiv, current_tiv) {
  chg <- if (prior_tiv == 0) NA_real_ else (current_tiv / prior_tiv) - 1
  shiny::HTML(paste0(
    "<div style='line-height:1.2'>",
    "<div><b>Prior:</b> ", fmt_bn(prior_tiv), "</div>",
    "<div><b>Current:</b> ", fmt_bn(current_tiv), "</div>",
    "<div style='opacity:0.8'>Change: ", if (is.na(chg)) "\u2014" else fmt_pct(chg, 1), "</div>",
    "</div>"
  ))
}

#' Render Layer Limit KPI card content
#'
#' @param prior_limit Prior layer limit
#' @param current_limit Current layer limit
#' @return HTML content for the limit value box
render_kpi_limit <- function(prior_limit, current_limit) {
  chg <- if (prior_limit == 0) NA_real_ else (current_limit / prior_limit) - 1
  shiny::HTML(paste0(
    "<div style='line-height:1.2'>",
    "<div><b>Prior:</b> ", fmt_bn(prior_limit), "</div>",
    "<div><b>Current:</b> ", fmt_bn(current_limit), "</div>",
    "<div style='opacity:0.8'>Change: ", if (is.na(chg)) "\u2014" else fmt_pct(chg, 1), "</div>",
    "</div>"
  ))
}

#' Render Premium KPI card content
#'
#' @param prior_gross Prior gross premium
#' @param current_gross Current gross premium
#' @param prior_net Prior net premium
#' @param current_net Current net premium
#' @return HTML content for the premium value box
render_kpi_premium <- function(prior_gross, current_gross, prior_net, current_net) {
  idx <- if (prior_gross == 0) NA_real_ else current_gross / prior_gross
  shiny::HTML(paste0(
    "<div style='line-height:1.25'>",
    "<div><b>Prior gross:</b> ", fmt_mn(prior_gross), " &nbsp;&nbsp; <b>net:</b> ", fmt_mn(prior_net), "</div>",
    "<div><b>Current gross:</b> ", fmt_mn(current_gross), " &nbsp;&nbsp; <b>net:</b> ", fmt_mn(current_net), "</div>",
    "<div style='opacity:0.8'>Premium index: ",
    if (is.na(idx)) "\u2014" else fmt_pct(idx - 1, 1),
    " (", if (is.na(idx)) "\u2014" else paste0(round(100 * idx, 1), "%"), ")</div>",
    "</div>"
  ))
}

#' Render Expected Loss KPI card content
#'
#' @param prior_el Prior expected loss
#' @param current_el Current expected loss
#' @return HTML content for the EL value box
render_kpi_el <- function(prior_el, current_el) {
  chg <- if (prior_el == 0) NA_real_ else (current_el / prior_el) - 1
  shiny::HTML(paste0(
    "<div style='line-height:1.2'>",
    "<div><b>Prior:</b> ", fmt_mn(prior_el), "</div>",
    "<div><b>Current:</b> ", fmt_mn(current_el), "</div>",
    "<div style='opacity:0.8'>Change: ", if (is.na(chg)) "\u2014" else fmt_pct(chg, 1), "</div>",
    "</div>"
  ))
}

#' Render Risk View cards
#'
#' @param prior Prior scenario results list
#' @param current Current scenario results list
#' @param target_lr Target loss ratio
#' @return A layout with risk view value boxes
render_risk_cards <- function(prior, current, target_lr) {
  dist_p <- if (is.na(prior$lr)) NA_real_ else prior$lr - target_lr
  dist_c <- if (is.na(current$lr)) NA_real_ else current$lr - target_lr

  f_lr <- function(x) if (is.na(x)) "\u2014" else fmt_pct(x, 1)
  f_dist <- function(x) {
    if (is.na(x)) return("\u2014")
    paste0(if (x > 0) "+" else "", fmt_pct(x, 1))
  }

  ind0 <- prior$indicated_gross
  ind1 <- current$indicated_gross

  bslib::layout_column_wrap(
    width = 1 / 3,
    gap = "0.75rem",
    bslib::value_box(
      title = "Implied LR (prior)",
      value = shiny::HTML(paste0(
        "<div style='line-height:1.15'>",
        "<div>", f_lr(prior$lr), "</div>",
        "<div style='opacity:0.8'>vs target: ", f_dist(dist_p), "</div>",
        "</div>"
      )),
      showcase = shiny::icon("bullseye"),
      theme_color = "secondary"
    ),
    bslib::value_box(
      title = "Implied LR (current)",
      value = shiny::HTML(paste0(
        "<div style='line-height:1.15'>",
        "<div>", f_lr(current$lr), "</div>",
        "<div style='opacity:0.8'>vs target: ", f_dist(dist_c), "</div>",
        "</div>"
      )),
      showcase = shiny::icon("chart-line"),
      theme_color = "primary"
    ),
    bslib::value_box(
      title = "Indicated premium",
      value = shiny::HTML(paste0(
        "<div style='line-height:1.15'>",
        "<div><b>Prior:</b> ", if (is.finite(ind0)) fmt_mn(ind0) else "\u2014", "</div>",
        "<div><b>Current:</b> ", if (is.finite(ind1)) fmt_mn(ind1) else "\u2014", "</div>",
        "</div>"
      )),
      showcase = shiny::icon("scale-balanced"),
      theme_color = "info"
    )
  )
}

# ==============================================================================
# END R/mod_kpi_cards.R
# ==============================================================================
