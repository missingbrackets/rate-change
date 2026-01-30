# app.R ---- Lloyd's Rate Change Shiny Application ----
# Polished underwriting tool: Lloyd's rate change analysis with satellite-level inputs
#
# ==============================================================================
# IMPLEMENTATION SUMMARY
# ==============================================================================
# This app implements Lloyd's PMDR-style rate change decomposition with:
# - Satellite-level exposure inputs (Prior/Current)
# - Satellite-level ROL inputs (Prior/Current)
# - Satellite-level failure rates QGU (Prior/Current)
# - Satellite-level attachment/limit inputs (Prior/Current)
# - Dual brokerage inputs (Prior/Current)
# - PMDR decomposition with Deductions Change component
# - Consistent Expected Loss formula throughout
# - Layer allocation output table
# ==============================================================================

# ==============================================================================
# LLOYD'S RATE CHANGE REQUIREMENTS SUMMARY
# ==============================================================================
#
# KEY CONCEPTS:
# -------------
# 1. RARC (Risk Adjusted Rate Change):
#    - Measures price difference between current year charge vs. what would have
#      been charged LAST YEAR for the EXACT SAME RISK
#    - Net of claims inflation to show true impact on expected loss ratios
#
# 2. Benchmark Price:
#    - Theoretical gross premium required to achieve the loss ratio set in the
#      approved Syndicate Business Forecast (SBF)
#
# 3. Price Adequacy:
#    - Comparison of achieved price against benchmark price
#
# PMDR (Performance Management Data Return) - 5 COMPONENTS:
# ---------------------------------------------------------
#   Component 1: Change in Deductible / Attachment Point
#   Component 2: Change in Breadth of Cover
#   Component 3: Other Factors
#   Component 4: Deductions Change (brokerage impact)
#   Component 5: Pure Rate Change (THE RARC - residual)
#
# ==============================================================================

message("
================================================================================
LLOYD'S RATE CHANGE APP - Enhanced with Satellite-Level Inputs
================================================================================
Features:
  - Satellite-level ROL, QGU, Attachment, Limit (Prior/Current)
  - Dual brokerage (Prior/Current)
  - PMDR with Deductions Change component
  - Consistent Expected Loss formula
  - Layer allocation output table
================================================================================
")

# ==============================================================================
# LOAD LIBRARIES
# ==============================================================================
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)
library(mbbefd)

# ==============================================================================
# SOURCE MODULES
# ==============================================================================
source("R/constants.R")
source("R/metrics.R")
source("R/ui_components.R")
source("R/mod_input_tables.R")
source("R/mod_kpi_cards.R")
source("R/mod_visualizations.R")
source("R/mod_decomposition_table.R")

# ==============================================================================
# C-PARAMETER LOOKUP FROM CSV
# ==============================================================================
c_params_csv <- tryCatch(
  read.csv("c parameters.csv", stringsAsFactors = FALSE),
  error = function(e) {
    message("Warning: Could not read 'c parameters.csv'. Using fallback.")
    data.frame(c = c(0, 5, 10), expectation = c(1, 0.2, 0.001))
  }
)

names(c_params_csv) <- gsub("^\\W+", "", names(c_params_csv))
c_params_csv <- c_params_csv[order(c_params_csv$expectation, decreasing = TRUE), ]

message(sprintf(
  "C-parameters CSV loaded: %d rows, c range [%.2f, %.2f], expectation range [%.6f, %.6f]",
  nrow(c_params_csv),
  min(c_params_csv$c), max(c_params_csv$c),
  min(c_params_csv$expectation), max(c_params_csv$expectation)
))

# ==============================================================================
# UI DEFINITION
# ==============================================================================
theme <- bs_theme(version = 5, bootswatch = "flatly")

ui <- page_sidebar(
  title = "Satellite Rate Change",
  theme = theme,
  sidebar = sidebar(
    width = SIDEBAR_WIDTH,
    build_deal_inputs_card(),
    build_pmdr_accordion()
  ),
  useShinyjs(),
  tags$head(tooltip_init_script()),

  # Main Content
  layout_column_wrap(
    width = 1,
    gap = "1rem",
    build_kpi_row(),
    build_charts_row(),
    build_allocation_risk_row(),
    build_curve_diagnostics_row()
  )
)

# ==============================================================================
# SERVER DEFINITION
# ==============================================================================
server <- function(input, output, session) {

  # ==========================================================================
  # REACTIVE STATE: EDITABLE TABLES
  # ==========================================================================
  schedule <- reactiveVal(default_schedule_df())
  rol_state <- reactiveVal(default_rol_df())
  qgu_state <- reactiveVal(default_qgu_df())
  layer_terms_state <- reactiveVal(default_layer_terms_df())

  # ==========================================================================
  # TABLE RENDERERS
  # ==========================================================================
  output$schedule_tbl <- renderDT({ render_schedule_table(schedule()) })
  output$rol_tbl <- renderDT({ render_rol_table(rol_state()) })
  output$qgu_tbl <- renderDT({ render_qgu_table(qgu_state()) })
  output$layer_terms_tbl <- renderDT({ render_layer_terms_table(layer_terms_state()) })

  # ==========================================================================
  # TABLE EDIT HANDLERS
  # ==========================================================================
  observeEvent(input$schedule_tbl_cell_edit, {
    schedule(handle_schedule_edit(schedule(), input$schedule_tbl_cell_edit))
  })

  observeEvent(input$rol_tbl_cell_edit, {
    rol_state(handle_rol_edit(rol_state(), input$rol_tbl_cell_edit))
  })

  observeEvent(input$qgu_tbl_cell_edit, {
    qgu_state(handle_qgu_edit(qgu_state(), input$qgu_tbl_cell_edit))
  })

  observeEvent(input$layer_terms_tbl_cell_edit, {
    layer_terms_state(handle_layer_terms_edit(layer_terms_state(), input$layer_terms_tbl_cell_edit))
  })

  # ==========================================================================
  # RESET BUTTONS
  # ==========================================================================
  observeEvent(input$reset_defaults, {
    schedule(default_schedule_df())
    rol_state(default_rol_df())
    qgu_state(default_qgu_df())
    layer_terms_state(default_layer_terms_df())
    updateNumericInput(session, "prior_brokerage", value = DEFAULT_BROKERAGE)
    updateNumericInput(session, "current_brokerage", value = DEFAULT_BROKERAGE)
    updateSliderInput(session, "target_lr", value = DEFAULT_TARGET_LR)
    updateSliderInput(session, "breadth_of_cover_change", value = 0)
    updateSliderInput(session, "other_exposure_change", value = 0)
  })

  observeEvent(input$reset_current, {
    schedule(copy_prior_to_current_schedule(schedule()))
    rol_state(copy_prior_to_current_rol(rol_state()))
    qgu_state(copy_prior_to_current_qgu(qgu_state()))
    layer_terms_state(copy_prior_to_current_layer_terms(layer_terms_state()))
    updateNumericInput(session, "current_brokerage", value = input$prior_brokerage)
  })

  # ==========================================================================
  # HELPER REACTIVES: EXTRACT VALUES FROM TABLES
  # ==========================================================================
  values_prior <- reactive(schedule()$PriorExposure)
  values_current <- reactive(schedule()$CurrentExposure)
  rols_prior <- reactive(rol_state()$PriorROL)
  rols_current <- reactive(rol_state()$CurrentROL)
  qgu_prior <- reactive(qgu_state()$PriorQGU)
  qgu_current <- reactive(qgu_state()$CurrentQGU)

  # Portfolio-level attachment/limit (use first satellite's values as representative)
  attachment_prior <- reactive(layer_terms_state()$PriorAttachment[1])
  attachment_current <- reactive(layer_terms_state()$CurrentAttachment[1])
  limit_prior <- reactive(layer_terms_state()$PriorLimit[1])
  limit_current <- reactive(layer_terms_state()$CurrentLimit[1])

  # ==========================================================================
  # EFFECTIVE C PARAMETER
  # ==========================================================================
  effective_c <- reactive({
    if (input$c_mode == "manual") {
      as.numeric(input$sr_c)
    } else {
      compute_portfolio_c(qgu_current(), values_current(), c_params_csv)
    }
  })

  # ==========================================================================
  # AUTO C-PARAMETER DISPLAY
  # ==========================================================================
  output$auto_c_display <- renderUI({
    render_auto_c_display(qgu_current(), values_current(), SAT_NAMES, c_params_csv)
  })

  # ==========================================================================
  # SCENARIO CALCULATIONS
  # ==========================================================================
  prior <- reactive({
    vals <- values_prior()
    A <- attachment_prior()
    L <- limit_prior()
    rols <- rols_prior()
    bro <- input$prior_brokerage
    c_curve <- effective_c()
    q_gu <- qgu_prior()
    tgt_lr <- input$target_lr

    tiv <- sum(vals)
    cq <- curve_allocation_and_q(values = vals, A = A, L = L, c = c_curve, q_gu = q_gu)
    vil <- value_in_layer(vals, A, L)
    gp <- premium_gross_satellite(rols, vil)
    np <- premium_net(gp, bro)
    el <- expected_loss(cq$q_layer, vil)
    lr <- implied_lr(el, np)
    indicated_gross <- benchmark_premium(el, tgt_lr, bro)

    list(
      values = vals, tiv = tiv, limit = L, attachment = A, vil = vil,
      gp = gp, np = np, el = el, lr = lr,
      indicated_gross = indicated_gross,
      shares = cq$shares, w_curve = cq$w_curve, q_layer = cq$q_layer,
      rols = rols, brokerage = bro, q_gu = q_gu
    )
  })

  current <- reactive({
    vals <- values_current()
    A <- attachment_current()
    L <- limit_current()
    rols <- rols_current()
    bro <- input$current_brokerage
    c_curve <- effective_c()
    q_gu <- qgu_current()
    tgt_lr <- input$target_lr

    tiv <- sum(vals)
    cq <- curve_allocation_and_q(values = vals, A = A, L = L, c = c_curve, q_gu = q_gu)
    vil <- value_in_layer(vals, A, L)
    gp <- premium_gross_satellite(rols, vil)
    np <- premium_net(gp, bro)
    el <- expected_loss(cq$q_layer, vil)
    lr <- implied_lr(el, np)
    indicated_gross <- benchmark_premium(el, tgt_lr, bro)

    list(
      values = vals, tiv = tiv, limit = L, attachment = A, vil = vil,
      gp = gp, np = np, el = el, lr = lr,
      indicated_gross = indicated_gross,
      shares = cq$shares, w_curve = cq$w_curve, q_layer = cq$q_layer,
      rols = rols, brokerage = bro, q_gu = q_gu
    )
  })

  # ==========================================================================
  # EXPOSURE CHANGE IMPACT (AUTO)
  # ==========================================================================
  exposure_change_delta <- reactive({
    c_curve <- effective_c()
    A <- attachment_current()
    L <- limit_current()
    rols <- rols_current()
    q_gu <- qgu_current()

    # Prior exposure at current terms
    vals_prior <- values_prior()
    vil_prior <- value_in_layer(vals_prior, A, L)
    gp_prior <- premium_gross_satellite(rols, vil_prior)

    # Current exposure at current terms
    vals_curr <- values_current()
    vil_curr <- value_in_layer(vals_curr, A, L)
    gp_curr <- premium_gross_satellite(rols, vil_curr)

    gp_curr - gp_prior
  })

  output$auto_exposure_change_display <- renderUI({
    render_auto_exposure_change_display(exposure_change_delta())
  })

  # ==========================================================================
  # DEDUCTIONS CHANGE (BROKERAGE IMPACT)
  # ==========================================================================
  deductions_change <- reactive({
    gp <- current()$gp
    prior_bro <- input$prior_brokerage
    curr_bro <- input$current_brokerage
    gp * (prior_bro - curr_bro)
  })

  # ==========================================================================
  # LLOYD'S PMDR DECOMPOSITION
  # ==========================================================================
  lloyds_decomp <- reactive({
    p <- prior()
    c <- current()
    tgt_lr <- input$target_lr

    prior_gross <- p$gp
    prior_net <- p$np
    current_gross <- c$gp
    current_net <- c$np
    total_change <- current_gross - prior_gross

    # Component 1: Deductible/Attachment Change
    # Intermediate: prior values/ROL but at current attachment/limit
    vals <- values_prior()
    A_curr <- attachment_current()
    L_curr <- limit_current()
    rols <- rols_prior()
    c_curve <- effective_c()
    q_gu <- qgu_prior()

    vil_int <- value_in_layer(vals, A_curr, L_curr)
    gp_int <- premium_gross_satellite(rols, vil_int)
    delta_attachment <- gp_int - prior_gross

    # Component 2: Breadth of Cover (user input)
    delta_breadth <- input$breadth_of_cover_change

    # Component 3: Other Exposure Change (auto + manual)
    delta_other <- exposure_change_delta() + input$other_exposure_change

    # Component 4: Deductions Change (brokerage impact)
    delta_deductions <- deductions_change()

    # Component 5: Pure Rate Change (RARC) - residual
    delta_pure_rate <- total_change - delta_attachment - delta_breadth - delta_other - delta_deductions

    # Benchmark
    el_current <- c$el
    benchmark_gross <- benchmark_premium(el_current, tgt_lr, input$current_brokerage)
    benchmark_net <- if (!is.na(benchmark_gross)) benchmark_gross * (1 - input$current_brokerage) else NA_real_

    # Price adequacy
    price_adeq <- price_adequacy(current_gross, benchmark_gross)

    # Verification
    reconstructed <- prior_gross + delta_attachment + delta_breadth + delta_other + delta_deductions + delta_pure_rate
    decomp_error <- abs(reconstructed - current_gross)

    list(
      prior_gross = prior_gross, prior_net = prior_net,
      delta_attachment = delta_attachment, delta_breadth = delta_breadth,
      delta_other = delta_other, delta_deductions = delta_deductions,
      delta_pure_rate = delta_pure_rate,
      current_gross = current_gross, current_net = current_net,
      total_change = total_change,
      benchmark_gross = benchmark_gross, benchmark_net = benchmark_net,
      price_adequacy = price_adeq,
      el_prior = p$el, el_current = el_current,
      reconstructed = reconstructed, decomp_error = decomp_error
    )
  })

  # ==========================================================================
  # RARC INDEX
  # ==========================================================================
  rarc_index <- reactive({
    vals <- values_prior()
    A <- attachment_prior()
    L <- limit_prior()
    c_curve <- effective_c()
    q_gu <- qgu_prior()

    # Prior exposure at prior ROL
    rols_p <- rols_prior()
    vil_p <- value_in_layer(vals, A, L)
    gp_prior <- premium_gross_satellite(rols_p, vil_p)

    # Prior exposure at current ROL
    rols_c <- rols_current()
    gp_rerated <- premium_gross_satellite(rols_c, vil_p)

    calculate_rarc_index(gp_rerated, gp_prior)
  })

  # ==========================================================================
  # OUTPUT: LLOYD'S DECOMPOSITION TABLE
  # ==========================================================================
  output$lloyds_decomp_table <- renderUI({
    render_lloyds_decomp_table(lloyds_decomp(), rarc_index())
  })

  # ==========================================================================
  # OUTPUT: LAYER ALLOCATION TABLE
  # ==========================================================================
  output$layer_allocation_tbl <- renderDT({
    render_layer_allocation_table(prior(), current(), SAT_NAMES)
  })

  # ==========================================================================
  # KPI CARDS
  # ==========================================================================
  output$kpi_tiv <- renderUI({
    render_kpi_tiv(prior()$tiv, current()$tiv)
  })

  output$kpi_limit <- renderUI({
    render_kpi_limit(prior()$limit, current()$limit)
  })

  output$kpi_prem <- renderUI({
    render_kpi_premium(prior()$gp, current()$gp, prior()$np, current()$np)
  })

  output$kpi_el <- renderUI({
    render_kpi_el(prior()$el, current()$el)
  })

  # ==========================================================================
  # VISUALIZATIONS
  # ==========================================================================
  output$rarc_gauge <- renderPlotly({
    render_rarc_gauge(rarc_index())
  })

  output$lloyds_waterfall <- renderPlotly({
    render_pmdr_waterfall(lloyds_decomp())
  })

  output$loss_premium_comparison <- renderPlotly({
    render_loss_premium_comparison(lloyds_decomp(), prior(), current())
  })

  output$ec_curve <- renderPlotly({
    render_ec_curve(prior(), current(), effective_c(), attachment_prior(), attachment_current())
  })

  # ==========================================================================
  # RISK VIEW
  # ==========================================================================
  output$risk_cards <- renderUI({
    render_risk_cards(prior(), current(), input$target_lr)
  })

  output$risk_line <- renderPlotly({
    render_risk_line(prior(), current(), input$target_lr)
  })

  # ==========================================================================
  # PER-SATELLITE CURVE DIAGNOSTICS
  # ==========================================================================

  # Compute per-satellite curve diagnostics for prior scenario
  curve_diag_prior <- reactive({
    compute_per_satellite_curve_diagnostics(
      sat_names = SAT_NAMES,
      values = values_prior(),
      q_gu = qgu_prior(),
      A = attachment_prior(),
      L = limit_prior(),
      lookup_df = c_params_csv
    )
  })

  # Compute per-satellite curve diagnostics for current scenario
  curve_diag_current <- reactive({
    compute_per_satellite_curve_diagnostics(
      sat_names = SAT_NAMES,
      values = values_current(),
      q_gu = qgu_current(),
      A = attachment_current(),
      L = limit_current(),
      lookup_df = c_params_csv
    )
  })

  # Per-satellite curves chart
  output$per_satellite_curves <- renderPlotly({
    render_per_satellite_curves(curve_diag_prior(), curve_diag_current(), SAT_NAMES)
  })

  # Curve diagnostics table
  output$curve_diagnostics_tbl <- renderDT({
    render_per_satellite_curve_table(curve_diag_prior(), curve_diag_current())
  })

  # Layer share comparison chart
  output$layer_share_comparison <- renderPlotly({
    render_layer_share_comparison(curve_diag_prior(), curve_diag_current())
  })
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================
shiny::runApp(shiny::shinyApp(ui, server), launch.browser = TRUE)
