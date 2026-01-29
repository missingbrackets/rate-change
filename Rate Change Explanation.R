# app.R  ---- Single-file Shiny app (no external data) ----
# Polished underwriting tool: why premium index < 100% even with flat ROL


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
            "sr_c",
            "Swiss Re curve (c)",
            choices = c("Y1 (c=1.5)" = 1.5, "Y2 (c=2)" = 2, "Y3 (c=3)" = 3, "Y4 (c=4)" = 4, "Lloyd’s-style (c=5)" = 5),
            selected = 4
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

    prior <- reactive({
        calc_scenario(
            values_base = values_base(),
            dep = 0,
            attachment = input$excess_prior,
            rol = input$rol,
            brokerage = input$brokerage,
            c_curve = as.numeric(input$sr_c),
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
            c_curve = as.numeric(input$sr_c),
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
        s0 <- calc_scenario(v0, 0, A0, rol, bro, as.numeric(input$sr_c), q_gu)
        s1 <- calc_scenario(v0, dep1, A0, rol, bro, as.numeric(input$sr_c), q_gu)
        s2 <- calc_scenario(v0, dep1, A1, rol, bro, as.numeric(input$sr_c), q_gu)
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

        cpar <- as.numeric(input$sr_c)

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
            cpar <- as.numeric(input$sr_c)
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
