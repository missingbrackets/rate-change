# app.R  ---- Single-file Shiny app (no external data) ----
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
# Source: "Instructions for what is needed wit.txt"
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

# Source the metrics calculation functions
source("R/metrics.R")

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

# --------------------------
# Default Values
# --------------------------
sat_names <- paste0("V", 1:4)
base_values <- c(6509542466, 6709868493, 6218354896, 8458413699)

default_q_gu <- c(
  0.0010601787456726477,
  0.0009041025302225488,
  0.0009040826278695704,
  0.006066660583587538
)

default_rol <- 0.0105

# Default attachment and limit (per satellite, derived from values)
default_attachment <- 5e9
default_limit <- max(base_values) - default_attachment

# UI Helper
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
# UI
# --------------------------
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly"
)

ui <- page_sidebar(
    title = "Satellite Rate Change",
    theme = theme,
    sidebar = sidebar(
        width = 560,
        card(
            card_header(tagList(icon("sliders"), "Deal Inputs")),
            layout_column_wrap(
                width = 1 / 2,
                switchInput("mode", label = "Sensitivity mode", value = TRUE, onLabel = "On", offLabel = "Off"),
                switchInput("unlock_rol", label = "Unlock ROL", value = FALSE, onLabel = "Unlocked", offLabel = "Fixed")
            ),
            tags$hr(),

            # ==================================================================
            # EXPOSURE TABLE (Prior/Current)
            # ==================================================================
            h6("Schedule Exposures (Direct Prior/Current)"),
            tags$small(style = "color: #6c757d;", "Edit exposure values directly. No depreciation adjustment."),
            DTOutput("schedule_tbl"),
            tags$hr(),

            # ==================================================================
            # ROL TABLE (Satellite-level, Prior/Current)
            # ==================================================================
            h6("Rate on Line (ROL) - Satellite Level"),
            tags$small(style = "color: #6c757d;", "Per-satellite ROL for prior and current year."),
            DTOutput("rol_tbl"),
            tags$hr(),

            # ==================================================================
            # QGU TABLE (Satellite-level failure rates, Prior/Current)
            # ==================================================================
            h6("Ground-Up Failure Rates (QGU) - Satellite Level"),
            tags$small(style = "color: #6c757d;", "Per-satellite ground-up failure rates."),
            DTOutput("qgu_tbl"),
            tags$hr(),

            # ==================================================================
            # ATTACHMENT/LIMIT TABLE (Satellite-level, Prior/Current)
            # ==================================================================
            h6("Layer Terms (Attachment/Limit) - Satellite Level"),
            tags$small(style = "color: #6c757d;", "Per-satellite attachment and limit for layer allocation."),
            DTOutput("layer_terms_tbl"),
            tags$hr(),

            # ==================================================================
            # BROKERAGE (Prior/Current)
            # ==================================================================
            h6("Brokerage / Deductions"),
            layout_column_wrap(
                width = 1 / 2,
                numericInput("prior_brokerage", "Prior Brokerage", value = 0.14, min = 0, max = 0.50, step = 0.005),
                numericInput("current_brokerage", "Current Brokerage", value = 0.14, min = 0, max = 0.50, step = 0.005)
            ),
            tags$hr(),

            # ==================================================================
            # OTHER PRICING TERMS
            # ==================================================================
            h6("Target Loss Ratio"),
            sliderInput("target_lr", "Target LR", min = 0.20, max = 0.80, value = 0.45, step = 0.01),
            tags$hr(),

            # ==================================================================
            # EXPOSURE CURVE SETTINGS
            # ==================================================================
            h6("Exposure Curve (Swiss Re / MBBEFD)"),
            selectInput(
                "c_mode",
                tip("C-parameter mode", "Auto: derive c from failure rates. Manual: use dropdown."),
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
                        "Portfolio c is computed as value-weighted average of per-satellite c values."
                    ),
                    uiOutput("auto_c_display")
                )
            ),
            tags$hr(),

            layout_column_wrap(
                width = 1 / 2,
                actionButton("reset_defaults", "Reset to Defaults", class = "btn btn-outline-primary w-100"),
                actionButton("reset_current", "Copy Prior to Current", class = "btn btn-primary w-100")
            )
        ),

        # ==================================================================
        # PMDR ADJUSTMENTS ACCORDION
        # ==================================================================
        accordion(
            id = "pmdr_accordion",
            accordion_panel(
                title = tagList(icon("chart-waterfall"), " PMDR Adjustments"),
                value = "pmdr_adjustments",
                tags$div(
                    style = "font-size: 0.92rem;",
                    tags$p(
                        style = "color: #6c757d; margin-bottom: 0.75rem;",
                        "Lloyd's PMDR manual adjustments for breadth of cover and other factors."
                    ),
                    sliderInput(
                        "breadth_of_cover_change",
                        tip("Breadth of Cover Change", "Premium impact from adding/removing perils."),
                        min = -5e6, max = 5e6, value = 0, step = 1e4, pre = "£"
                    ),
                    sliderInput(
                        "other_exposure_change",
                        tip("Other Exposure Change (manual)", "Manual premium adjustment for other factors."),
                        min = -5e6, max = 5e6, value = 0, step = 1e4, pre = "£"
                    ),
                    uiOutput("auto_exposure_change_display"),
                    tags$hr(),
                    tags$p(
                        style = "color: #6c757d; font-size: 0.85rem;",
                        "Deductions Change is computed automatically from brokerage difference."
                    )
                )
            ),
            accordion_panel(
                title = tagList(icon("table"), " Lloyd's Decomposition (verification)"),
                value = "lloyds_decomposition",
                tags$div(
                    style = "font-size: 0.92rem;",
                    uiOutput("lloyds_decomp_table")
                )
            ),
            accordion_panel(
                title = tagList(icon("calculator"), " Actuarial Formulas"),
                value = "actuarial_detail",
                tags$div(
                    style = "font-size: 0.95rem;",
                    tags$h6("Expected Loss Formula (Consistent Everywhere)"),
                    tags$pre("EL = Σ(q_gu_i × Exposure_i × LayerAllocation_i)"),
                    tags$h6("Premium (Satellite-level ROL)"),
                    tags$pre("Gross = Σ(ROL_i × ValueInLayer_i)\nNet = Gross × (1 − Brokerage)"),
                    tags$h6("Layer Allocation"),
                    tags$pre("Allocation_i = (LayerShare_i × Value_i) / Σ(...)"),
                    tags$h6("Implied LR"),
                    tags$pre("LR = EL / NetPremium")
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

    # ==================================================================
    # MAIN CONTENT
    # ==================================================================
    layout_column_wrap(
        width = 1,
        gap = "1rem",

        # KPI Row
        layout_column_wrap(
            width = 1 / 4,
            gap = "1rem",
            value_box(
                title = tip("Total TIV", "Sum of satellite schedule values."),
                value = uiOutput("kpi_tiv"),
                showcase = icon("satellite-dish"),
                theme_color = "primary"
            ),
            value_box(
                title = tip("Layer Limit", "Limit derived from max value minus attachment."),
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
                title = tip("Expected Loss", "EL = Σ(q_gu × exposure × allocation)."),
                value = uiOutput("kpi_el"),
                showcase = icon("triangle-exclamation"),
                theme_color = "warning"
            )
        ),

        # Charts Row
        layout_column_wrap(
            width = 1 / 2,
            gap = "1rem",
            card(
                full_screen = TRUE,
                card_header(tagList(icon("chart-bar"), "Lloyd's Rate Change Analysis")),
                navset_tab(
                    nav_panel(
                        "PMDR Waterfall",
                        plotlyOutput("lloyds_waterfall", height = 360),
                        tags$div(
                            style = "font-size:0.95rem; color:#5a6b7b;",
                            "Lloyd's PMDR decomposition with Deductions Change component"
                        )
                    ),
                    nav_panel(
                        "Loss vs Premium",
                        plotlyOutput("loss_premium_comparison", height = 360)
                    ),
                    nav_panel(
                        "MBBEFD Curve",
                        plotlyOutput("ec_curve", height = 360)
                    )
                )
            ),
            card(
                full_screen = TRUE,
                card_header(tagList(icon("gauge-high"), "RARC (Pure Rate Change) Index")),
                plotlyOutput("rarc_gauge", height = 270)
            )
        ),

        # Layer Allocation Table + Risk View
        layout_column_wrap(
            width = 1 / 2,
            gap = "1rem",
            card(
                full_screen = TRUE,
                card_header(tagList(icon("table-cells"), "Layer Allocation by Satellite")),
                DTOutput("layer_allocation_tbl"),
                tags$div(
                    style = "font-size:0.85rem; color:#5a6b7b; margin-top:0.5rem;",
                    "Shows layer allocation weights and expected loss contribution per satellite."
                )
            ),
            card(
                full_screen = TRUE,
                card_header(tagList(icon("heart-pulse"), "Risk View (EL, LR, Benchmark)")),
                uiOutput("risk_cards"),
                plotlyOutput("risk_line", height = 220)
            )
        )
    )
)

# --------------------------
# Server
# --------------------------
server <- function(input, output, session) {

    # ==========================================================================
    # EXPOSURE TABLE STATE
    # ==========================================================================
    schedule <- reactiveVal(
        data.frame(
            Satellite = sat_names,
            PriorExposure = base_values,
            CurrentExposure = base_values,
            stringsAsFactors = FALSE
        )
    )

    output$schedule_tbl <- renderDT({
        datatable(
            schedule(),
            colnames = c("Satellite", "Prior Exposure", "Current Exposure"),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 4, ordering = FALSE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatCurrency("PriorExposure", currency = "£", digits = 0) %>%
            formatCurrency("CurrentExposure", currency = "£", digits = 0)
    })

    observeEvent(input$schedule_tbl_cell_edit, {
        info <- input$schedule_tbl_cell_edit
        df <- schedule()
        i <- info$row
        j <- info$col
        v <- info$value
        if (j %in% c(1, 2)) {
            newv <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", v)))
            if (is.finite(newv) && newv >= 0) df[i, j + 1] <- newv
            schedule(df)
        }
    })

    # ==========================================================================
    # ROL TABLE STATE (Satellite-level, Prior/Current)
    # ==========================================================================
    rol_state <- reactiveVal(
        data.frame(
            Satellite = sat_names,
            PriorROL = rep(default_rol, 4),
            CurrentROL = rep(default_rol, 4),
            stringsAsFactors = FALSE
        )
    )

    output$rol_tbl <- renderDT({
        datatable(
            rol_state(),
            colnames = c("Satellite", "Prior ROL", "Current ROL"),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 4, ordering = FALSE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatPercentage("PriorROL", digits = 2) %>%
            formatPercentage("CurrentROL", digits = 2)
    })

    observeEvent(input$rol_tbl_cell_edit, {
        info <- input$rol_tbl_cell_edit
        df <- rol_state()
        i <- info$row
        j <- info$col
        v <- info$value
        if (j %in% c(1, 2)) {
            newv <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", v)))
            # Validate ROL >= 0
            if (is.finite(newv) && newv >= 0) {
                df[i, j + 1] <- newv
                rol_state(df)
            }
        }
    })

    # ==========================================================================
    # QGU TABLE STATE (Satellite-level failure rates, Prior/Current)
    # ==========================================================================
    qgu_state <- reactiveVal(
        data.frame(
            Satellite = sat_names,
            PriorQGU = default_q_gu,
            CurrentQGU = default_q_gu,
            stringsAsFactors = FALSE
        )
    )

    output$qgu_tbl <- renderDT({
        datatable(
            qgu_state(),
            colnames = c("Satellite", "Prior QGU", "Current QGU"),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 4, ordering = FALSE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatPercentage("PriorQGU", digits = 4) %>%
            formatPercentage("CurrentQGU", digits = 4)
    })

    observeEvent(input$qgu_tbl_cell_edit, {
        info <- input$qgu_tbl_cell_edit
        df <- qgu_state()
        i <- info$row
        j <- info$col
        v <- info$value
        if (j %in% c(1, 2)) {
            newv <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", v)))
            # Validate QGU in [0, 1]
            if (is.finite(newv) && newv >= 0 && newv <= 1) {
                df[i, j + 1] <- newv
                qgu_state(df)
            }
        }
    })

    # ==========================================================================
    # LAYER TERMS TABLE STATE (Attachment/Limit, Prior/Current)
    # ==========================================================================
    layer_terms_state <- reactiveVal(
        data.frame(
            Satellite = sat_names,
            PriorAttachment = rep(default_attachment, 4),
            PriorLimit = rep(default_limit, 4),
            CurrentAttachment = rep(default_attachment, 4),
            CurrentLimit = rep(default_limit, 4),
            stringsAsFactors = FALSE
        )
    )

    output$layer_terms_tbl <- renderDT({
        datatable(
            layer_terms_state(),
            colnames = c("Satellite", "Prior Att", "Prior Lim", "Current Att", "Current Lim"),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 4, ordering = FALSE, scrollX = TRUE),
            editable = list(target = "cell", disable = list(columns = c(0)))
        ) %>%
            formatCurrency(c("PriorAttachment", "PriorLimit", "CurrentAttachment", "CurrentLimit"),
                          currency = "£", digits = 0)
    })

    observeEvent(input$layer_terms_tbl_cell_edit, {
        info <- input$layer_terms_tbl_cell_edit
        df <- layer_terms_state()
        i <- info$row
        j <- info$col
        v <- info$value
        if (j %in% c(1, 2, 3, 4)) {
            newv <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", v)))
            if (is.finite(newv) && newv >= 0) {
                df[i, j + 1] <- newv
                layer_terms_state(df)
            }
        }
    })

    # ==========================================================================
    # RESET BUTTONS
    # ==========================================================================
    observeEvent(input$reset_defaults, {
        schedule(data.frame(
            Satellite = sat_names,
            PriorExposure = base_values,
            CurrentExposure = base_values,
            stringsAsFactors = FALSE
        ))
        rol_state(data.frame(
            Satellite = sat_names,
            PriorROL = rep(default_rol, 4),
            CurrentROL = rep(default_rol, 4),
            stringsAsFactors = FALSE
        ))
        qgu_state(data.frame(
            Satellite = sat_names,
            PriorQGU = default_q_gu,
            CurrentQGU = default_q_gu,
            stringsAsFactors = FALSE
        ))
        layer_terms_state(data.frame(
            Satellite = sat_names,
            PriorAttachment = rep(default_attachment, 4),
            PriorLimit = rep(default_limit, 4),
            CurrentAttachment = rep(default_attachment, 4),
            CurrentLimit = rep(default_limit, 4),
            stringsAsFactors = FALSE
        ))
        updateNumericInput(session, "prior_brokerage", value = 0.14)
        updateNumericInput(session, "current_brokerage", value = 0.14)
        updateSliderInput(session, "target_lr", value = 0.45)
        updateSliderInput(session, "breadth_of_cover_change", value = 0)
        updateSliderInput(session, "other_exposure_change", value = 0)
    })

    observeEvent(input$reset_current, {
        # Copy Prior values to Current
        sch <- schedule()
        sch$CurrentExposure <- sch$PriorExposure
        schedule(sch)

        rol <- rol_state()
        rol$CurrentROL <- rol$PriorROL
        rol_state(rol)

        qgu <- qgu_state()
        qgu$CurrentQGU <- qgu$PriorQGU
        qgu_state(qgu)

        lt <- layer_terms_state()
        lt$CurrentAttachment <- lt$PriorAttachment
        lt$CurrentLimit <- lt$PriorLimit
        layer_terms_state(lt)

        updateNumericInput(session, "current_brokerage", value = input$prior_brokerage)
    })

    # ==========================================================================
    # HELPER REACTIVES
    # ==========================================================================

    # Get values from tables
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

    # Effective c parameter
    effective_c <- reactive({
        mode <- input$c_mode
        if (mode == "manual") {
            as.numeric(input$sr_c)
        } else {
            # Auto mode: value-weighted average
            compute_portfolio_c(qgu_current(), values_current(), c_params_csv)
        }
    })

    # Display auto c values
    output$auto_c_display <- renderUI({
        c_per_sat <- compute_per_satellite_c(qgu_current(), c_params_csv)
        port_c <- compute_portfolio_c(qgu_current(), values_current(), c_params_csv)

        sat_df <- data.frame(
            Satellite = sat_names,
            q_gu = qgu_current(),
            c_i = round(c_per_sat, 3)
        )

        tags$div(
            style = "margin-top: 0.5rem;",
            tags$table(
                class = "table table-sm",
                style = "font-size: 0.8rem; margin-bottom: 0.5rem;",
                tags$thead(tags$tr(tags$th("Sat"), tags$th("QGU"), tags$th("c_i"))),
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

        # Calculate layer shares and allocation using exposure curve
        cq <- curve_allocation_and_q(values = vals, A = A, L = L, c = c_curve, q_gu = q_gu)

        # Value in layer per satellite
        vil <- value_in_layer(vals, A, L)

        # Gross premium = sum of satellite-level (ROL × VIL)
        gp <- premium_gross_satellite(rols, vil)
        np <- premium_net(gp, bro)

        # Expected loss using consistent formula: sum(q_layer × vil)
        el <- expected_loss(cq$q_layer, vil)
        lr <- implied_lr(el, np)

        # Benchmark premium
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
        # Calculate premium change due to exposure changes (values), holding other factors constant
        c_curve <- effective_c()
        A <- attachment_current()
        L <- limit_current()
        rols <- rols_current()
        bro <- input$current_brokerage
        q_gu <- qgu_current()

        # Prior exposure at current terms
        vals_prior <- values_prior()
        cq_prior <- curve_allocation_and_q(values = vals_prior, A = A, L = L, c = c_curve, q_gu = q_gu)
        vil_prior <- value_in_layer(vals_prior, A, L)
        gp_prior <- premium_gross_satellite(rols, vil_prior)

        # Current exposure at current terms
        vals_curr <- values_current()
        cq_curr <- curve_allocation_and_q(values = vals_curr, A = A, L = L, c = c_curve, q_gu = q_gu)
        vil_curr <- value_in_layer(vals_curr, A, L)
        gp_curr <- premium_gross_satellite(rols, vil_curr)

        gp_curr - gp_prior
    })

    output$auto_exposure_change_display <- renderUI({
        auto_delta <- exposure_change_delta()
        tags$div(
            style = "font-size: 0.85rem; color: #6c757d; margin-top: 0.25rem;",
            paste0("Auto exposure impact from schedule change: ", fmt_money(auto_delta))
        )
    })

    # ==========================================================================
    # DEDUCTIONS CHANGE (BROKERAGE IMPACT)
    # ==========================================================================
    deductions_change <- reactive({
        # Premium impact from brokerage change
        # If gross premium stayed same, but brokerage changed, net premium changes
        # We calculate this as: gross × (prior_brokerage - current_brokerage)
        # This represents the change in deductions

        # Use current gross as reference
        gp <- current()$gp
        prior_bro <- input$prior_brokerage
        curr_bro <- input$current_brokerage

        # Deductions change in gross terms (impact on reported gross premium)
        # Since we report gross premium, brokerage doesn't change gross
        # But we want to capture the NET impact for decomposition
        # For PMDR purposes, deductions change affects the net calculation

        # We'll report it as the net premium impact:
        # Net change = GP × (curr_bro - prior_bro) (negative if brokerage increased)
        # This is actually inverted: higher brokerage = lower net to insurer
        # So we report: GP × (prior_bro - curr_bro) as the "improvement" to insurer

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
        rols <- rols_prior()  # Keep prior ROLs
        bro <- input$prior_brokerage  # Keep prior brokerage
        c_curve <- effective_c()
        q_gu <- qgu_prior()  # Keep prior QGU

        cq_int <- curve_allocation_and_q(vals, A_curr, L_curr, c_curve, q_gu)
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
            prior_gross = prior_gross,
            prior_net = prior_net,
            delta_attachment = delta_attachment,
            delta_breadth = delta_breadth,
            delta_other = delta_other,
            delta_deductions = delta_deductions,
            delta_pure_rate = delta_pure_rate,
            current_gross = current_gross,
            current_net = current_net,
            total_change = total_change,
            benchmark_gross = benchmark_gross,
            benchmark_net = benchmark_net,
            price_adequacy = price_adeq,
            el_prior = p$el,
            el_current = el_current,
            reconstructed = reconstructed,
            decomp_error = decomp_error
        )
    })

    # ==========================================================================
    # RARC INDEX
    # ==========================================================================
    rarc_index <- reactive({
        # RARC = (prior exposure @ current ROL) / (prior exposure @ prior ROL)
        vals <- values_prior()
        A <- attachment_prior()
        L <- limit_prior()
        c_curve <- effective_c()
        q_gu <- qgu_prior()

        # Prior exposure at prior ROL
        rols_p <- rols_prior()
        cq_p <- curve_allocation_and_q(vals, A, L, c_curve, q_gu)
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
        decomp <- lloyds_decomp()
        rarc <- rarc_index()

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

            tags$h6("PMDR Decomposition", style = "margin-top: 0;"),
            tags$table(
                class = "table table-sm table-striped",
                style = "margin-bottom: 1rem;",
                tags$thead(tags$tr(tags$th("Component"), tags$th(style = "text-align: right;", "Value"))),
                tags$tbody(
                    tags$tr(tags$td("Prior Gross Premium"), tags$td(style = "text-align: right;", fmt(decomp$prior_gross))),
                    tags$tr(style = "background: #e3f2fd;", tags$td("1. Deductible/Attachment Change"), tags$td(style = "text-align: right;", fmt_delta(decomp$delta_attachment))),
                    tags$tr(style = "background: #e3f2fd;", tags$td("2. Breadth of Cover Change"), tags$td(style = "text-align: right;", fmt_delta(decomp$delta_breadth))),
                    tags$tr(style = "background: #e3f2fd;", tags$td("3. Other Exposure Change"), tags$td(style = "text-align: right;", fmt_delta(decomp$delta_other))),
                    tags$tr(style = "background: #fce4ec;", tags$td("4. Deductions Change"), tags$td(style = "text-align: right;", fmt_delta(decomp$delta_deductions))),
                    tags$tr(style = "background: #fff3cd; font-weight: bold;", tags$td("5. Pure Rate Change (RARC)"), tags$td(style = "text-align: right;", fmt_delta(decomp$delta_pure_rate))),
                    tags$tr(style = "font-weight: bold; border-top: 2px solid #333;", tags$td("Current Gross Premium"), tags$td(style = "text-align: right;", fmt(decomp$current_gross))),
                    tags$tr(style = "color: #6c757d;", tags$td("Total Change"), tags$td(style = "text-align: right;", fmt_delta(decomp$total_change)))
                )
            ),

            tags$h6("Benchmark & Price Adequacy"),
            tags$table(
                class = "table table-sm",
                style = "margin-bottom: 1rem;",
                tags$tbody(
                    tags$tr(tags$td("Expected Loss (current)"), tags$td(style = "text-align: right;", fmt(decomp$el_current))),
                    tags$tr(tags$td("Benchmark Gross Premium"), tags$td(style = "text-align: right;", fmt(decomp$benchmark_gross))),
                    tags$tr(style = "font-weight: bold;", tags$td("Price Adequacy"), tags$td(style = "text-align: right;", fmt_pct(decomp$price_adequacy)))
                )
            ),

            tags$h6("RARC Index"),
            tags$table(
                class = "table table-sm",
                tags$tbody(
                    tags$tr(style = "font-weight: bold; background: #d4edda;", tags$td("RARC Index"), tags$td(style = "text-align: right;", fmt_pct(rarc)))
                )
            ),

            if (decomp$decomp_error > 0.01) {
                tags$div(style = "color: #dc3545; font-size: 0.8rem;", icon("exclamation-triangle"),
                        sprintf(" Decomposition error: %s", fmt(decomp$decomp_error)))
            } else {
                tags$div(style = "color: #28a745; font-size: 0.8rem;", icon("check-circle"),
                        " Decomposition verified")
            }
        )
    })

    # ==========================================================================
    # OUTPUT: LAYER ALLOCATION TABLE
    # ==========================================================================
    output$layer_allocation_tbl <- renderDT({
        p <- prior()
        c <- current()

        df <- data.frame(
            Satellite = sat_names,
            PriorAllocation = p$w_curve,
            CurrentAllocation = c$w_curve,
            PriorEL = p$q_layer * p$vil,
            CurrentEL = c$q_layer * c$vil,
            stringsAsFactors = FALSE
        )

        # Add totals row
        df <- rbind(df, data.frame(
            Satellite = "TOTAL",
            PriorAllocation = sum(p$w_curve),
            CurrentAllocation = sum(c$w_curve),
            PriorEL = sum(p$q_layer * p$vil),
            CurrentEL = sum(c$q_layer * c$vil)
        ))

        datatable(
            df,
            colnames = c("Satellite", "Prior Alloc", "Current Alloc", "Prior EL", "Current EL"),
            rownames = FALSE,
            options = list(dom = "t", pageLength = 5, ordering = FALSE)
        ) %>%
            formatPercentage(c("PriorAllocation", "CurrentAllocation"), digits = 2) %>%
            formatCurrency(c("PriorEL", "CurrentEL"), currency = "£", digits = 0)
    })

    # ==========================================================================
    # KPI CARDS
    # ==========================================================================
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

    # ==========================================================================
    # RARC GAUGE
    # ==========================================================================
    output$rarc_gauge <- renderPlotly({
        rarc <- rarc_index()
        rarc_pct <- ifelse(is.na(rarc), NA_real_, rarc * 100)

        label <- if (is.na(rarc)) {
            "—"
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

        plot_ly(
            type = "indicator",
            mode = "gauge+number+delta",
            value = ifelse(is.na(rarc_pct), 100, rarc_pct),
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
            layout(
                margin = list(l = 30, r = 30, t = 60, b = 10),
                annotations = list(
                    list(
                        x = 0.5, y = -0.15, xref = "paper", yref = "paper",
                        text = "RARC = (Prior exposure @ Current ROL) / (Prior exposure @ Prior ROL)",
                        showarrow = FALSE, font = list(size = 10, color = "#666")
                    )
                )
            )
    })

    # ==========================================================================
    # LLOYD'S PMDR WATERFALL
    # ==========================================================================
    output$lloyds_waterfall <- renderPlotly({
        decomp <- lloyds_decomp()

        fmt_label <- function(x) {
            if (is.na(x)) return("—")
            prefix <- if (x >= 0) "+" else ""
            paste0(prefix, "£", format(round(x / 1e6, 2), nsmall = 2), "m")
        }

        fmt_pct_delta <- function(x, base) {
            if (is.na(x) || is.na(base) || base == 0) return("")
            pct <- x / base * 100
            prefix <- if (pct >= 0) "+" else ""
            paste0("(", prefix, sprintf("%.1f%%", pct), ")")
        }

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
            paste0("£", format(round(decomp$prior_gross / 1e6, 2), nsmall = 2), "m"),
            paste0(fmt_label(decomp$delta_attachment)),
            paste0(fmt_label(decomp$delta_breadth)),
            paste0(fmt_label(decomp$delta_other)),
            paste0(fmt_label(decomp$delta_deductions)),
            paste0(fmt_label(decomp$delta_pure_rate)),
            paste0("£", format(round(decomp$current_gross / 1e6, 2), nsmall = 2), "m")
        )

        plot_ly(
            type = "waterfall",
            x = steps,
            y = values,
            measure = measure,
            text = text_labels,
            textposition = "inside",
            textfont = list(color = "white", size = 9),
            hoverinfo = "text",
            hovertext = c(
                paste0("Prior Gross: £", format(round(decomp$prior_gross), big.mark = ",")),
                paste0("Attachment Change: ", fmt_label(decomp$delta_attachment)),
                paste0("Breadth Change: ", fmt_label(decomp$delta_breadth)),
                paste0("Other Factors: ", fmt_label(decomp$delta_other)),
                paste0("Deductions Change: ", fmt_label(decomp$delta_deductions)),
                paste0("Pure Rate (RARC): ", fmt_label(decomp$delta_pure_rate)),
                paste0("Current Gross: £", format(round(decomp$current_gross), big.mark = ","))
            ),
            connector = list(line = list(color = "#aaa", width = 1)),
            increasing = list(marker = list(color = "#27ae60")),
            decreasing = list(marker = list(color = "#e74c3c")),
            totals = list(marker = list(color = "#2c3e50"))
        ) %>%
            layout(
                title = list(text = "<b>Lloyd's PMDR Premium Decomposition</b>", font = list(size = 14), x = 0.5),
                xaxis = list(title = "", tickfont = list(size = 9)),
                yaxis = list(title = "Gross Premium (£)", tickformat = ",.0f", tickprefix = "£"),
                margin = list(l = 80, r = 20, t = 50, b = 100),
                showlegend = FALSE
            )
    })

    # ==========================================================================
    # LOSS VS PREMIUM COMPARISON
    # ==========================================================================
    output$loss_premium_comparison <- renderPlotly({
        decomp <- lloyds_decomp()
        p <- prior()
        c <- current()

        metrics <- c("Expected Loss", "Gross Premium", "Net Premium", "Benchmark")
        prior_vals <- c(decomp$el_prior, decomp$prior_gross, decomp$prior_net, decomp$benchmark_gross)
        current_vals <- c(decomp$el_current, decomp$current_gross, decomp$current_net, decomp$benchmark_gross)

        plot_ly() %>%
            add_bars(x = metrics, y = prior_vals / 1e6, name = "Prior",
                    marker = list(color = "#3498db"),
                    text = paste0("£", round(prior_vals / 1e6, 2), "m"),
                    textposition = "outside") %>%
            add_bars(x = metrics, y = current_vals / 1e6, name = "Current",
                    marker = list(color = "#e74c3c"),
                    text = paste0("£", round(current_vals / 1e6, 2), "m"),
                    textposition = "outside") %>%
            layout(
                title = list(text = "<b>Prior vs Current: Loss & Premium</b>", font = list(size = 14), x = 0.5),
                xaxis = list(title = ""),
                yaxis = list(title = "Value (£m)", tickformat = ",.1f", tickprefix = "£", ticksuffix = "m"),
                barmode = "group",
                legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
                margin = list(l = 70, r = 20, t = 50, b = 70),
                annotations = list(
                    list(x = 0.5, y = 1.08, xref = "paper", yref = "paper",
                        text = paste0("LR: Prior ", sprintf("%.1f%%", p$lr * 100),
                                     " → Current ", sprintf("%.1f%%", c$lr * 100),
                                     " | Adequacy: ", sprintf("%.1f%%", decomp$price_adequacy * 100)),
                        showarrow = FALSE, font = list(size = 11, color = "#666"))
                )
            )
    })

    # ==========================================================================
    # MBBEFD EXPOSURE CURVE
    # ==========================================================================
    output$ec_curve <- renderPlotly({
        vals0 <- values_prior()
        MPL0 <- max(vals0)
        vals1 <- values_current()
        MPL1 <- max(vals1)

        L0 <- prior()$limit
        L1 <- current()$limit
        A0 <- attachment_prior()
        A1 <- attachment_current()

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

        ann <- paste0(
            "Prior layer share: ", sprintf("%.1f%%", 100 * s0$share),
            "<br>Current layer share: ", sprintf("%.1f%%", 100 * s1$share)
        )

        plot_ly() %>%
            add_lines(data = df_curve, x = ~x, y = ~G, name = "G(x)", hoverinfo = "skip") %>%
            add_markers(
                data = pts, x = ~x, y = ~y, color = ~scen, symbol = ~lab,
                hovertext = ~paste0(scen, "<br>", lab, "<br>x=", sprintf("%.3f", x), "<br>G(x)=", sprintf("%.3f", y)),
                hoverinfo = "text", marker = list(size = 9)
            ) %>%
            layout(
                xaxis = list(title = "Deductible as % of MPL (x)"),
                yaxis = list(title = "Exposure curve G(x)"),
                margin = list(l = 60, r = 20, t = 10, b = 50),
                annotations = list(
                    list(x = 0.02, y = 0.98, xref = "paper", yref = "paper",
                        text = ann, showarrow = FALSE, align = "left")
                ),
                legend = list(orientation = "h", x = 0, y = -0.2)
            )
    })

    # ==========================================================================
    # RISK VIEW
    # ==========================================================================
    output$risk_cards <- renderUI({
        p <- prior()
        c <- current()
        tgt <- input$target_lr

        dist_p <- ifelse(is.na(p$lr), NA_real_, p$lr - tgt)
        dist_c <- ifelse(is.na(c$lr), NA_real_, c$lr - tgt)

        f_lr <- function(x) ifelse(is.na(x), "—", fmt_pct(x, 1))
        f_dist <- function(x) ifelse(is.na(x), "—", paste0(ifelse(x > 0, "+", ""), fmt_pct(x, 1)))

        ind0 <- p$indicated_gross
        ind1 <- c$indicated_gross
        ind_chg <- ifelse(!is.finite(ind0) || ind0 == 0, NA_real_, (ind1 / ind0) - 1)

        layout_column_wrap(
            width = 1 / 3,
            gap = "0.75rem",
            value_box(
                title = "Implied LR (prior)",
                value = HTML(paste0(
                    "<div style='line-height:1.15'>",
                    "<div>", f_lr(p$lr), "</div>",
                    "<div style='opacity:0.8'>vs target: ", f_dist(dist_p), "</div>",
                    "</div>"
                )),
                showcase = icon("bullseye"), theme_color = "secondary"
            ),
            value_box(
                title = "Implied LR (current)",
                value = HTML(paste0(
                    "<div style='line-height:1.15'>",
                    "<div>", f_lr(c$lr), "</div>",
                    "<div style='opacity:0.8'>vs target: ", f_dist(dist_c), "</div>",
                    "</div>"
                )),
                showcase = icon("chart-line"), theme_color = "primary"
            ),
            value_box(
                title = "Indicated premium",
                value = HTML(paste0(
                    "<div style='line-height:1.15'>",
                    "<div><b>Prior:</b> ", ifelse(is.finite(ind0), fmt_mn(ind0), "—"), "</div>",
                    "<div><b>Current:</b> ", ifelse(is.finite(ind1), fmt_mn(ind1), "—"), "</div>",
                    "</div>"
                )),
                showcase = icon("scale-balanced"), theme_color = "info"
            )
        )
    })

    output$risk_line <- renderPlotly({
        p <- prior()
        c <- current()
        tgt <- input$target_lr

        df <- tibble::tibble(
            scenario = c("Prior", "Current"),
            lr = c(p$lr, c$lr)
        )

        plot_ly(df, x = ~scenario, y = ~lr, type = "scatter", mode = "lines+markers",
                hovertext = ~paste0("Implied LR: ", ifelse(is.na(lr), "—", fmt_pct(lr, 1))),
                hoverinfo = "text", name = "Implied LR") %>%
            add_lines(x = df$scenario, y = rep(tgt, 2), name = "Target LR",
                     hovertext = paste0("Target: ", fmt_pct(tgt, 1)),
                     hoverinfo = "text", line = list(dash = "dot")) %>%
            layout(
                yaxis = list(title = "Loss ratio", tickformat = ".0%"),
                xaxis = list(title = ""),
                margin = list(l = 60, r = 20, t = 10, b = 50),
                legend = list(orientation = "h", x = 0, y = -0.3)
            )
    })
}

shiny::runApp(shiny::shinyApp(ui, server), launch.browser = TRUE)
