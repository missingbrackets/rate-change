# ==============================================================================
# R/metrics.R - Core Calculation Functions for Rate Change Application
# ==============================================================================
#
# This file contains all reusable calculation functions extracted from the main
# Shiny application. These functions are deterministic and provide consistent
# calculations across all outputs.
#
# DESIGN PRINCIPLES:
# - All functions are pure (no side effects, no reactive dependencies)
# - Results are deterministic given the same inputs
# - Layer allocation uses MBBEFD/Swiss Re exposure curves
# - Expected Loss = Ground-up failure rate × Ground-up exposure × Layer Allocation
#
# ==============================================================================

# --------------------------
# Formatting Helpers
# --------------------------

#' Format currency in British Pounds
#' @param x Numeric value to format
#' @return Character string formatted as £X,XXX
fmt_money <- function(x) scales::dollar(x, accuracy = 1, big.mark = ",", prefix = "$")

#' Format value in billions (£Xbn)
#' @param x Numeric value to format
#' @return Character string formatted as £X.XXXbn
fmt_bn <- function(x) paste0("£", scales::comma(x / 1e9, accuracy = 0.001), "bn")

#' Format value in millions (£Xm)
#' @param x Numeric value to format
#' @return Character string formatted as £X.XXm
fmt_mn <- function(x) paste0("£", scales::comma(x / 1e6, accuracy = 0.01), "m")

#' Format as percentage
#' @param x Numeric value (0-1 scale) to format
#' @param digits Number of decimal places
#' @return Character string formatted as X.X%
fmt_pct <- function(x, digits = 1) scales::percent(x, accuracy = 10^(-digits))

#' Clamp value to [0, 1] interval
#' @param x Numeric value
#' @return Numeric value clamped between 0 and 1
clamp01 <- function(x) pmax(0, pmin(1, x))

# --------------------------
# Exposure Curve Functions (MBBEFD / Swiss Re)
# --------------------------

#' Check if mbbefd package is available
#' @return Logical indicating mbbefd availability
has_mbbefd <- function() requireNamespace("mbbefd", quietly = TRUE)

#' MBBEFD exposure curve G(x) using Swiss Re parameterization
#'
#' The exposure curve G(x) maps the deductible as a fraction of MPL to the
#' proportion of losses that exceed the deductible. This is fundamental to
#' layer pricing.
#'
#' @param x Numeric value in [0,1] representing deductible/MPL
#' @param c Swiss Re curve parameter (higher c = more severe loss distribution)
#' @return Numeric value G(x) representing exposure curve value
#' @details
#'   - c=1 corresponds to uniform distribution
#'   - c=2-4 typical for property risks
#'   - c=5+ for more severe/catastrophic risks (Lloyd's style)
#'
#'   If mbbefd package unavailable, uses fallback approximation G(d)=d(2-d)
G_mbbefd <- function(x, c) {
  x <- pmax(0, pmin(1, x))

  if (has_mbbefd()) {
    pars <- mbbefd::swissRe(as.numeric(c))
    return(mbbefd::ecMBBEFD(x, b = pars[1], g = pars[2]))
  }

  # Fallback approximation (NOT true MBBEFD)
  x * (2 - x)
}

#' Calculate layer share from exposure curve
#'
#' Computes the proportion of ground-up losses that fall within a layer defined
#' by attachment point A and limit L, given an assumed Maximum Probable Loss (MPL).
#'
#' @param A Attachment point (deductible)
#' @param L Limit (layer size)
#' @param MPL Maximum Probable Loss (typically max satellite value)
#' @param c Swiss Re curve parameter
#' @return List containing:
#'   - d: lower bound as fraction of MPL (A/MPL)
#'   - u: upper bound as fraction of MPL ((A+L)/MPL)
#'   - Gd: exposure curve value at lower bound
#'   - Gu: exposure curve value at upper bound
#'   - share: layer share = G(u) - G(d)
#' @details
#'   Layer share represents the proportion of ground-up expected loss
#'   that the layer (xs L of A) is expected to absorb.
layer_share_from_curve <- function(A, L, MPL, c) {
  if (!is.finite(MPL) || MPL <= 0 || !is.finite(L) || L <= 0) {
    return(list(d = NA_real_, u = NA_real_, Gd = NA_real_, Gu = NA_real_, share = 0))
  }
  d <- A / MPL
  u <- (A + L) / MPL
  d <- pmax(0, pmin(1, d))
  u <- pmax(0, pmin(1, u))
  G_d <- G_mbbefd(d, c)
  G_u <- G_mbbefd(u, c)
  list(d = d, u = u, Gd = G_d, Gu = G_u, share = pmax(G_u - G_d, 0))
}

#' Calculate curve-based allocation and layer-adjusted failure rates
#'
#' For each satellite, computes its layer share and allocation weight based on
#' the exposure curve. Also computes layer-adjusted failure rates.
#'
#' @param values Vector of satellite values (used as per-satellite MPL)
#' @param A Attachment point
#' @param L Limit
#' @param c Swiss Re curve parameter
#' @param q_gu Vector of ground-up failure rates per satellite
#' @return List containing:
#'   - shares: vector of layer shares per satellite
#'   - w_curve: allocation weights (normalized, sum to 1)
#'   - q_layer: layer-adjusted failure rates (q_gu × shares)
#' @details
#'   ASSUMPTION: Allocation weight is proportional to (layer share × value).
#'   This weights higher-value satellites more heavily.
curve_allocation_and_q <- function(values, A, L, c, q_gu) {
  shares <- vapply(values, function(MPL_i) layer_share_from_curve(A, L, MPL_i, c)$share, numeric(1))
  # Allocation weight proportional to share × value (severity base)
  raw <- shares * pmax(values, 0)
  w_curve <- if (sum(raw) > 0) raw / sum(raw) else rep(0, length(values))
  q_layer <- q_gu * shares
  list(shares = shares, w_curve = w_curve, q_layer = q_layer)
}

#' Compute curve-implied allocation weights
#'
#' Simplified version of curve_allocation_and_q that returns only the
#' allocation weights.
#'
#' @param values Vector of satellite values
#' @param A Attachment point
#' @param L Limit
#' @param c Swiss Re curve parameter
#' @return Vector of allocation weights (sum to 1)
alloc_from_curve <- function(values, A, L, c) {
  parts <- vapply(values, function(MPL_i) {
    out <- layer_share_from_curve(A, L, MPL_i, c)
    if (is.na(out$share)) return(0)
    pmax(out$share, 0)
  }, numeric(1))

  raw <- parts * pmax(values, 0)
  if (sum(raw) <= 0) return(rep(0, length(values)))
  raw / sum(raw)
}

#' Calculate allocation shift percentage between two allocation vectors
#'
#' Measures how much the allocation has shifted using half the sum of
#' absolute differences (TVD/2).
#'
#' @param w0 Prior allocation weights
#' @param w1 Current allocation weights
#' @return Numeric value between 0 (identical) and 1 (completely different)
alloc_shift_pct <- function(w0, w1) 0.5 * sum(abs(w1 - w0))

# --------------------------
# Layer and Value Functions
# --------------------------

#' Calculate layer limit from satellite values
#'
#' Implements the "Tower top" assumption: the layer limit is determined by
#' the maximum satellite value minus the attachment point.
#'
#' @param values Vector of satellite values
#' @param attachment Attachment point (deductible)
#' @return Numeric layer limit (floored at 0)
#' @details
#'   ASSUMPTION: Tower top = max(V_i). This means the layer can respond
#'   up to the value of the largest satellite in the portfolio.
limit_from_values <- function(values, attachment) {
  pmax(max(values) - attachment, 0)
}

#' Calculate value-in-layer for each satellite
#'
#' For each satellite, computes how much of its value sits within the layer
#' defined by attachment and limit.
#'
#' @param values Vector of satellite values
#' @param attachment Attachment point
#' @param limit Layer limit
#' @return Vector of value-in-layer amounts per satellite
#' @details
#'   For satellite i: VIL_i = max(min(V_i - attachment, limit), 0)
#'   This caps the exposure at the layer limit and floors at zero.
value_in_layer <- function(values, attachment, limit) {
  pmax(pmin(values - attachment, limit), 0)
}

# --------------------------
# Premium Calculations
# --------------------------

#' Calculate gross premium from ROL and limit
#'
#' @param rol Rate on Line (premium as fraction of limit)
#' @param limit Layer limit
#' @return Gross premium amount
#' @details
#'   ROL-based pricing: Premium = ROL × Limit
#'   This is the standard approach for excess-of-loss layers.
premium_gross <- function(rol, limit) rol * limit

#' Calculate net premium after brokerage deduction
#'
#' @param gross Gross premium
#' @param brokerage Brokerage rate (0 to 1)
#' @return Net premium (to insurer)
#' @details
#'   Net Premium = Gross × (1 - brokerage)
#'   Brokerage includes commission and other acquisition costs.
premium_net <- function(gross, brokerage) gross * (1 - brokerage)

#' Calculate gross premium from satellite-level ROLs
#'
#' When each satellite has its own ROL, gross premium is calculated as the
#' sum of each satellite's (ROL × value-in-layer).
#'
#' @param rols Vector of ROL values per satellite
#' @param values_in_layer Vector of value-in-layer per satellite
#' @return Total gross premium
#' @details
#'   Gross Premium = sum(ROL_i × VIL_i)
#'   This allows for risk-differentiated pricing by satellite.
premium_gross_satellite <- function(rols, values_in_layer) {
  sum(rols * values_in_layer)
}

# --------------------------
# Expected Loss Calculations
# --------------------------

#' Calculate expected loss using layer-adjusted failure rates
#'
#' CRITICAL FORMULA - Must be consistent everywhere:
#' Expected Loss = sum(q_layer_i × value_in_layer_i)
#'
#' Where q_layer_i = q_gu_i × layer_share_i
#'
#' @param q_layer Vector of layer-adjusted failure rates
#' @param vil Vector of values-in-layer
#' @return Total expected loss
expected_loss <- function(q_layer, vil) sum(q_layer * vil)

#' Calculate expected loss using ground-up failure rates and allocation
#'
#' Alternative formula using explicit allocation:
#' EL = sum(q_gu_i × exposure_i × allocation_i)
#'
#' This formulation explicitly shows the three components:
#' 1. Ground-up failure rate (inherent risk)
#' 2. Ground-up exposure (satellite value)
#' 3. Layer allocation (how much of that exposure is in our layer)
#'
#' @param q_gu Vector of ground-up failure rates
#' @param exposures Vector of ground-up exposures (satellite values)
#' @param allocations Vector of layer allocation weights
#' @return Total expected loss
#' @details
#'   This is the CONSISTENT EL FORMULA required by Phase 7:
#'   EL = sum(q_gu_i × GU_exposure_i × layer_alloc_i)
expected_loss_from_allocation <- function(q_gu, exposures, allocations) {
  sum(q_gu * exposures * allocations)
}

#' Calculate implied loss ratio
#'
#' @param el Expected loss
#' @param net_prem Net premium
#' @return Loss ratio (EL / Net Premium), or NA if net premium is 0
implied_lr <- function(el, net_prem) {
  ifelse(net_prem <= 0, NA_real_, el / net_prem)
}

#' Calculate benchmark premium
#'
#' The theoretical gross premium required to achieve target loss ratio,
#' given expected loss and brokerage.
#'
#' @param el Expected loss
#' @param target_lr Target loss ratio
#' @param brokerage Brokerage rate
#' @return Benchmark gross premium
#' @details
#'   Benchmark = EL / target_lr / (1 - brokerage)
#'
#'   Derivation:
#'   - Target LR = EL / Net Premium
#'   - Net Premium = Gross × (1 - brokerage)
#'   - Gross = EL / target_lr / (1 - brokerage)
benchmark_premium <- function(el, target_lr, brokerage) {
  if (target_lr <= 0 || brokerage >= 1) return(NA_real_)
  el / target_lr / (1 - brokerage)
}

#' Calculate price adequacy
#'
#' Ratio of actual premium to benchmark premium. Values > 100% indicate
#' pricing above benchmark; < 100% indicates pricing below benchmark.
#'
#' @param actual_gross Actual gross premium charged
#' @param benchmark_gross Benchmark gross premium
#' @return Price adequacy ratio
price_adequacy <- function(actual_gross, benchmark_gross) {
  if (is.na(benchmark_gross) || benchmark_gross <= 0) return(NA_real_)
  actual_gross / benchmark_gross
}

# --------------------------
# C-Parameter Functions (CSV Lookup)
# --------------------------

#' Map a failure rate to a Swiss Re c parameter
#'
#' Uses linear interpolation from the c parameters CSV to find the
#' appropriate curve parameter for a given failure rate.
#'
#' @param failure_rate Ground-up failure rate (q_gu)
#' @param lookup_df Data frame with 'c' and 'expectation' columns
#' @return Interpolated c value
#' @details
#'   The CSV maps c parameters to expected loss ratios ("expectation").
#'   Higher failure rates map to lower c values (less severe distribution).
#'   Lower failure rates map to higher c values (more severe distribution).
map_failure_rate_to_c <- function(failure_rate, lookup_df) {
  if (is.na(failure_rate) || !is.finite(failure_rate)) return(NA_real_)

  exp_col <- lookup_df$expectation
  c_col <- lookup_df$c

  min_exp <- min(exp_col)
  max_exp <- max(exp_col)

  if (failure_rate >= max_exp) {
    return(c_col[which.max(exp_col)])
  }
  if (failure_rate <= min_exp) {
    return(c_col[which.min(exp_col)])
  }

  sorted_idx <- order(exp_col)
  approx(
    x = exp_col[sorted_idx],
    y = c_col[sorted_idx],
    xout = failure_rate,
    method = "linear",
    rule = 2
  )$y
}

#' Compute per-satellite c values from failure rates
#'
#' @param q_gu_vec Vector of failure rates
#' @param lookup_df Data frame with 'c' and 'expectation' columns
#' @return Vector of c values (same length as input)
compute_per_satellite_c <- function(q_gu_vec, lookup_df) {
  vapply(q_gu_vec, function(q) map_failure_rate_to_c(q, lookup_df), numeric(1))
}

#' Compute portfolio-level c as value-weighted average
#'
#' @param q_gu_vec Vector of failure rates
#' @param values_vec Vector of satellite values
#' @param lookup_df Data frame with 'c' and 'expectation' columns
#' @return Single portfolio-level c value
#' @details
#'   ASSUMPTION: Portfolio c = sum(c_i × value_i) / sum(value_i)
#'   This weights higher-value satellites more heavily in determining
#'   the overall curve shape.
compute_portfolio_c <- function(q_gu_vec, values_vec, lookup_df) {
  c_per_sat <- compute_per_satellite_c(q_gu_vec, lookup_df)
  weights <- values_vec / sum(values_vec)
  sum(c_per_sat * weights, na.rm = TRUE)
}

#' Compute per-satellite curve diagnostics
#'
#' Calculates detailed curve metrics for each satellite using per-satellite
#' c parameters derived from their individual failure rates.
#'
#' @param sat_names Vector of satellite names
#' @param values Vector of satellite values (used as per-satellite MPL)
#' @param q_gu Vector of ground-up failure rates per satellite
#' @param A Attachment point
#' @param L Limit
#' @param lookup_df Data frame with 'c' and 'expectation' columns for c lookup
#' @return Data frame with columns:
#'   - Satellite: satellite name
#'   - Value: satellite value (MPL)
#'   - QGU: ground-up failure rate
#'   - c_i: per-satellite curve parameter
#'   - d: lower bound as fraction of MPL (A/MPL)
#'   - u: upper bound as fraction of MPL ((A+L)/MPL)
#'   - Gd: exposure curve value at lower bound G(d)
#'   - Gu: exposure curve value at upper bound G(u)
#'   - Share: layer share = G(u) - G(d)
#'   - q_layer: layer-adjusted failure rate (q_gu × share)
compute_per_satellite_curve_diagnostics <- function(sat_names, values, q_gu, A, L, lookup_df) {
  n <- length(sat_names)

  # Get per-satellite c values from failure rates
  c_per_sat <- compute_per_satellite_c(q_gu, lookup_df)

  # Compute curve diagnostics for each satellite using its own c parameter
  diagnostics <- lapply(seq_len(n), function(i) {
    MPL_i <- values[i]
    c_i <- c_per_sat[i]

    # Use per-satellite c for the layer share calculation
    share_info <- layer_share_from_curve(A, L, MPL_i, c_i)

    data.frame(
      Satellite = sat_names[i],
      Value = values[i],
      QGU = q_gu[i],
      c_i = c_i,
      d = share_info$d,
      u = share_info$u,
      Gd = share_info$Gd,
      Gu = share_info$Gu,
      Share = share_info$share,
      q_layer = q_gu[i] * share_info$share,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, diagnostics)
}

#' Compute curve allocation using per-satellite c parameters
#'
#' Alternative to curve_allocation_and_q that uses per-satellite c values
#' derived from each satellite's failure rate, rather than a single portfolio c.
#'
#' @param sat_names Vector of satellite names
#' @param values Vector of satellite values (used as per-satellite MPL)
#' @param q_gu Vector of ground-up failure rates per satellite
#' @param A Attachment point
#' @param L Limit
#' @param lookup_df Data frame with 'c' and 'expectation' columns
#' @return List containing:
#'   - shares: vector of layer shares per satellite (using per-sat c)
#'   - w_curve: allocation weights (normalized, sum to 1)
#'   - q_layer: layer-adjusted failure rates (q_gu × shares)
#'   - c_per_sat: per-satellite c values used
curve_allocation_per_satellite_c <- function(sat_names, values, q_gu, A, L, lookup_df) {
  diag <- compute_per_satellite_curve_diagnostics(sat_names, values, q_gu, A, L, lookup_df)

  shares <- diag$Share
  q_layer <- diag$q_layer
  c_per_sat <- diag$c_i

  # Allocation weight proportional to share × value
  raw <- shares * pmax(values, 0)
  w_curve <- if (sum(raw) > 0) raw / sum(raw) else rep(0, length(values))

  list(
    shares = shares,
    w_curve = w_curve,
    q_layer = q_layer,
    c_per_sat = c_per_sat
  )
}

# --------------------------
# Scenario Calculation
# --------------------------

#' Calculate complete scenario metrics
#'
#' Computes all relevant metrics for a pricing scenario including TIV,
#' limit, premiums, expected loss, and loss ratio.
#'
#' @param values Vector of satellite values (ground-up exposures)
#' @param attachment Layer attachment point
#' @param rols Vector of ROLs per satellite (or single ROL to apply to all)
#' @param brokerage Brokerage rate
#' @param c_curve Swiss Re curve parameter
#' @param q_gu Vector of ground-up failure rates
#' @param target_lr Target loss ratio (for benchmark calculation)
#' @return List containing all scenario metrics:
#'   - values: input satellite values
#'   - tiv: total insured value
#'   - limit: layer limit
#'   - vil: values-in-layer vector
#'   - gp: gross premium
#'   - np: net premium
#'   - el: expected loss
#'   - lr: implied loss ratio
#'   - indicated_gross: benchmark gross premium
#'   - shares: layer shares per satellite
#'   - w_curve: allocation weights
#'   - q_layer: layer-adjusted failure rates
calc_scenario <- function(values, attachment, rols, brokerage, c_curve, q_gu, target_lr = 0.45) {
  tiv <- sum(values)
  lim <- limit_from_values(values, attachment)
  vil <- value_in_layer(values, attachment, lim)

  # Curve-driven allocation + layer-adjusted failure rates
  cq <- curve_allocation_and_q(values = values, A = attachment, L = lim, c = c_curve, q_gu = q_gu)
  q_layer <- cq$q_layer

  # Premium calculation - handle single ROL vs vector of ROLs
  if (length(rols) == 1) {
    gp <- premium_gross(rols, lim)
  } else {
    # Satellite-level ROLs: sum(ROL_i × VIL_i)
    gp <- premium_gross_satellite(rols, vil)
  }
  np <- premium_net(gp, brokerage)

  # Expected loss (consistent formula)
  el <- expected_loss(q_layer, vil)
  lr <- implied_lr(el, np)

  # Benchmark premium
  indicated_gross <- benchmark_premium(el, target_lr, brokerage)

  list(
    values = values, tiv = tiv, limit = lim, vil = vil,
    gp = gp, np = np, el = el, lr = lr,
    indicated_gross = indicated_gross,
    shares = cq$shares, w_curve = cq$w_curve, q_layer = q_layer
  )
}

# --------------------------
# PMDR Decomposition
# --------------------------

#' Calculate Lloyd's PMDR decomposition
#'
#' Decomposes the total premium change into PMDR components:
#' 1. Deductible/Attachment Change
#' 2. Breadth of Cover Change
#' 3. Other Factors (exposure change)
#' 4. Pure Rate Change (RARC) - the residual
#' 5. Deductions Change (brokerage impact)
#'
#' @param prior_gross Prior gross premium
#' @param current_gross Current gross premium
#' @param prior_net Prior net premium
#' @param current_net Current net premium
#' @param delta_attachment Premium impact from attachment change
#' @param delta_breadth Premium impact from breadth of cover (user input)
#' @param delta_other Premium impact from other exposure factors
#' @param delta_deductions Premium impact from brokerage change
#' @return List with all decomposition components
#' @details
#'   CONSTRAINT: prior_gross + sum(deltas) = current_gross
#'   Pure rate change is calculated as the residual to ensure this constraint.
calculate_pmdr_decomposition <- function(
    prior_gross,
    current_gross,
    prior_net,
    current_net,
    delta_attachment,
    delta_breadth,
    delta_other,
    delta_deductions
) {
  total_change <- current_gross - prior_gross

  # Pure rate change is the residual after accounting for all other components
  delta_pure_rate <- total_change - delta_attachment - delta_breadth - delta_other - delta_deductions

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
    reconstructed = reconstructed,
    decomp_error = decomp_error
  )
}

#' Calculate RARC index
#'
#' Risk Adjusted Rate Change = (prior exposure @ current ROL) / (prior exposure @ prior ROL)
#'
#' @param rerated_gross Gross premium for prior exposure at current rates
#' @param prior_gross Gross premium for prior exposure at prior rates
#' @return RARC index (1.0 = flat, >1 = increase, <1 = decrease)
calculate_rarc_index <- function(rerated_gross, prior_gross) {
  if (prior_gross <= 0) return(NA_real_)
  rerated_gross / prior_gross
}

# --------------------------
# Validation Helpers
# --------------------------

#' Validate ROL inputs
#'
#' @param rols Vector or single ROL value
#' @return List with 'valid' flag and 'message'
validate_rols <- function(rols) {
  if (any(is.na(rols))) {
    return(list(valid = FALSE, message = "ROL contains NA values"))
  }
  if (any(rols < 0)) {
    return(list(valid = FALSE, message = "ROL must be >= 0"))
  }
  list(valid = TRUE, message = "OK")
}

#' Validate allocation weights
#'
#' @param weights Vector of allocation weights
#' @param tolerance Acceptable deviation from sum = 1
#' @return List with 'valid' flag and 'message'
validate_allocation <- function(weights, tolerance = 0.0001) {
  if (any(is.na(weights))) {
    return(list(valid = FALSE, message = "Allocation contains NA values"))
  }
  if (any(weights < 0)) {
    return(list(valid = FALSE, message = "Allocation weights must be >= 0"))
  }
  weight_sum <- sum(weights)
  if (abs(weight_sum - 1) > tolerance) {
    return(list(valid = FALSE, message = sprintf("Weights sum to %.4f (must equal 1.0)", weight_sum)))
  }
  list(valid = TRUE, message = "OK")
}

#' Validate failure rates
#'
#' @param q_gu Vector of ground-up failure rates
#' @return List with 'valid' flag and 'message'
validate_failure_rates <- function(q_gu) {
  if (any(is.na(q_gu))) {
    return(list(valid = FALSE, message = "Failure rate contains NA values"))
  }
  if (any(q_gu < 0)) {
    return(list(valid = FALSE, message = "Failure rates must be >= 0"))
  }
  if (any(q_gu > 1)) {
    return(list(valid = FALSE, message = "Failure rates must be <= 1"))
  }
  list(valid = TRUE, message = "OK")
}

# ==============================================================================
# END R/metrics.R
# ==============================================================================
