# ==============================================================================
# R/constants.R - Default Values and Configuration
# ==============================================================================
#
# This file contains all default values and configuration constants used
# throughout the Lloyd's rate change application.
#
# ==============================================================================

# Satellite names
SAT_NAMES <- paste0("V", 1:4)

# Base exposure values (GBP)
BASE_VALUES <- c(6509542466, 6709868493, 6218354896, 8458413699)

# Default ground-up failure rates per satellite
DEFAULT_Q_GU <- c(
  0.0010601787456726477,
  0.0009041025302225488,
  0.0009040826278695704,
  0.006066660583587538
)

# Default Rate on Line
DEFAULT_ROL <- 0.0105

# Default layer terms
DEFAULT_ATTACHMENT <- 5e9
DEFAULT_LIMIT <- max(BASE_VALUES) - DEFAULT_ATTACHMENT

# Default brokerage rate
DEFAULT_BROKERAGE <- 0.14

# Default target loss ratio
DEFAULT_TARGET_LR <- 0.45

# Swiss Re curve parameter options
CURVE_OPTIONS <- c(
  "Y1 (c=1.5)" = 1.5,
  "Y2 (c=2)" = 2,
  "Y3 (c=3)" = 3,
  "Y4 (c=4)" = 4,
  "Lloyd's-style (c=5)" = 5
)

# Default c parameter
DEFAULT_C <- 4

# PMDR adjustment ranges
PMDR_ADJUSTMENT_MIN <- -5e6
PMDR_ADJUSTMENT_MAX <- 5e6
PMDR_ADJUSTMENT_STEP <- 1e4

# Sidebar width
SIDEBAR_WIDTH <- 560

# ==============================================================================
# Helper function to create default data frames
# ==============================================================================

#' Create default schedule exposure data frame
#' @return Data frame with satellite exposure data
default_schedule_df <- function() {
  data.frame(
    Satellite = SAT_NAMES,
    PriorExposure = BASE_VALUES,
    CurrentExposure = BASE_VALUES,
    stringsAsFactors = FALSE
  )
}

#' Create default ROL data frame
#' @return Data frame with satellite ROL data
default_rol_df <- function() {
  data.frame(
    Satellite = SAT_NAMES,
    PriorROL = rep(DEFAULT_ROL, length(SAT_NAMES)),
    CurrentROL = rep(DEFAULT_ROL, length(SAT_NAMES)),
    stringsAsFactors = FALSE
  )
}

#' Create default QGU data frame
#' @return Data frame with satellite failure rate data
default_qgu_df <- function() {
  data.frame(
    Satellite = SAT_NAMES,
    PriorQGU = DEFAULT_Q_GU,
    CurrentQGU = DEFAULT_Q_GU,
    stringsAsFactors = FALSE
  )
}

#' Create default layer terms data frame
#' @return Data frame with satellite layer terms
default_layer_terms_df <- function() {
  data.frame(
    Satellite = SAT_NAMES,
    PriorAttachment = rep(DEFAULT_ATTACHMENT, length(SAT_NAMES)),
    PriorLimit = rep(DEFAULT_LIMIT, length(SAT_NAMES)),
    CurrentAttachment = rep(DEFAULT_ATTACHMENT, length(SAT_NAMES)),
    CurrentLimit = rep(DEFAULT_LIMIT, length(SAT_NAMES)),
    stringsAsFactors = FALSE
  )
}

# ==============================================================================
# END R/constants.R
# ==============================================================================
