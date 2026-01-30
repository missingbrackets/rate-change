# ==============================================================================
# R/mod_input_tables.R - Editable Input Tables Module
# ==============================================================================
#
# This module handles the rendering and editing of all input data tables:
# - Schedule Exposures (Prior/Current)
# - ROL (Prior/Current)
# - QGU (Prior/Current)
# - Layer Terms (Attachment/Limit, Prior/Current)
#
# ==============================================================================

#' Render a schedule exposure table
#'
#' @param data Data frame with columns: Satellite, PriorExposure, CurrentExposure
#' @return A DT datatable object
render_schedule_table <- function(data) {
  DT::datatable(
    data,
    colnames = c("Satellite", "Prior Exposure", "Current Exposure"),
    rownames = FALSE,
    options = list(dom = "t", pageLength = 4, ordering = FALSE),
    editable = list(target = "cell", disable = list(columns = c(0)))
  ) %>%
    DT::formatCurrency("PriorExposure", currency = "\u00a3", digits = 0) %>%
    DT::formatCurrency("CurrentExposure", currency = "\u00a3", digits = 0)
}

#' Render a ROL table
#'
#' @param data Data frame with columns: Satellite, PriorROL, CurrentROL
#' @return A DT datatable object
render_rol_table <- function(data) {
  DT::datatable(
    data,
    colnames = c("Satellite", "Prior ROL", "Current ROL"),
    rownames = FALSE,
    options = list(dom = "t", pageLength = 4, ordering = FALSE),
    editable = list(target = "cell", disable = list(columns = c(0)))
  ) %>%
    DT::formatPercentage("PriorROL", digits = 2) %>%
    DT::formatPercentage("CurrentROL", digits = 2)
}

#' Render a QGU (failure rate) table
#'
#' @param data Data frame with columns: Satellite, PriorQGU, CurrentQGU
#' @return A DT datatable object
render_qgu_table <- function(data) {
  DT::datatable(
    data,
    colnames = c("Satellite", "Prior QGU", "Current QGU"),
    rownames = FALSE,
    options = list(dom = "t", pageLength = 4, ordering = FALSE),
    editable = list(target = "cell", disable = list(columns = c(0)))
  ) %>%
    DT::formatPercentage("PriorQGU", digits = 4) %>%
    DT::formatPercentage("CurrentQGU", digits = 4)
}

#' Render a layer terms table
#'
#' @param data Data frame with columns: Satellite, PriorAttachment, PriorLimit,
#'             CurrentAttachment, CurrentLimit
#' @return A DT datatable object
render_layer_terms_table <- function(data) {
  DT::datatable(
    data,
    colnames = c("Satellite", "Prior Att", "Prior Lim", "Current Att", "Current Lim"),
    rownames = FALSE,
    options = list(dom = "t", pageLength = 4, ordering = FALSE, scrollX = TRUE),
    editable = list(target = "cell", disable = list(columns = c(0)))
  ) %>%
    DT::formatCurrency(
      c("PriorAttachment", "PriorLimit", "CurrentAttachment", "CurrentLimit"),
      currency = "\u00a3", digits = 0
    )
}

#' Render a layer allocation output table
#'
#' @param prior Prior scenario results list
#' @param current Current scenario results list
#' @param sat_names Vector of satellite names
#' @return A DT datatable object
render_layer_allocation_table <- function(prior, current, sat_names) {
  df <- data.frame(
    Satellite = sat_names,
    PriorAllocation = prior$w_curve,
    CurrentAllocation = current$w_curve,
    PriorEL = prior$q_layer * prior$vil,
    CurrentEL = current$q_layer * current$vil,
    stringsAsFactors = FALSE
  )

  # Add totals row
  df <- rbind(df, data.frame(
    Satellite = "TOTAL",
    PriorAllocation = sum(prior$w_curve),
    CurrentAllocation = sum(current$w_curve),
    PriorEL = sum(prior$q_layer * prior$vil),
    CurrentEL = sum(current$q_layer * current$vil)
  ))

  DT::datatable(
    df,
    colnames = c("Satellite", "Prior Alloc", "Current Alloc", "Prior EL", "Current EL"),
    rownames = FALSE,
    options = list(dom = "t", pageLength = 5, ordering = FALSE)
  ) %>%
    DT::formatPercentage(c("PriorAllocation", "CurrentAllocation"), digits = 2) %>%
    DT::formatCurrency(c("PriorEL", "CurrentEL"), currency = "\u00a3", digits = 0)
}

#' Parse numeric value from cell edit
#'
#' Extracts a numeric value from user input, removing currency symbols
#' and other non-numeric characters.
#'
#' @param value Raw value from cell edit
#' @return Numeric value or NA if parsing fails
parse_numeric_edit <- function(value) {
  suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", value)))
}

#' Handle schedule table cell edit
#'
#' Updates a schedule data frame based on cell edit input.
#'
#' @param df Current schedule data frame
#' @param edit_info Edit info from input$xxx_cell_edit
#' @return Updated data frame
handle_schedule_edit <- function(df, edit_info) {
  i <- edit_info$row
  j <- edit_info$col
  v <- edit_info$value

  if (j %in% c(1, 2)) {
    newv <- parse_numeric_edit(v)
    if (is.finite(newv) && newv >= 0) {
      df[i, j + 1] <- newv
    }
  }
  df
}

#' Handle ROL table cell edit
#'
#' Updates a ROL data frame based on cell edit input.
#'
#' @param df Current ROL data frame
#' @param edit_info Edit info from input$xxx_cell_edit
#' @return Updated data frame
handle_rol_edit <- function(df, edit_info) {
  i <- edit_info$row
  j <- edit_info$col
  v <- edit_info$value

  if (j %in% c(1, 2)) {
    newv <- parse_numeric_edit(v)
    # Validate ROL >= 0
    if (is.finite(newv) && newv >= 0) {
      df[i, j + 1] <- newv
    }
  }
  df
}

#' Handle QGU table cell edit
#'
#' Updates a QGU data frame based on cell edit input.
#'
#' @param df Current QGU data frame
#' @param edit_info Edit info from input$xxx_cell_edit
#' @return Updated data frame
handle_qgu_edit <- function(df, edit_info) {
  i <- edit_info$row
  j <- edit_info$col
  v <- edit_info$value

  if (j %in% c(1, 2)) {
    newv <- parse_numeric_edit(v)
    # Validate QGU in [0, 1]
    if (is.finite(newv) && newv >= 0 && newv <= 1) {
      df[i, j + 1] <- newv
    }
  }
  df
}

#' Handle layer terms table cell edit
#'
#' Updates a layer terms data frame based on cell edit input.
#'
#' @param df Current layer terms data frame
#' @param edit_info Edit info from input$xxx_cell_edit
#' @return Updated data frame
handle_layer_terms_edit <- function(df, edit_info) {
  i <- edit_info$row
  j <- edit_info$col
  v <- edit_info$value

  if (j %in% c(1, 2, 3, 4)) {
    newv <- parse_numeric_edit(v)
    if (is.finite(newv) && newv >= 0) {
      df[i, j + 1] <- newv
    }
  }
  df
}

#' Copy prior values to current for schedule
#'
#' @param df Schedule data frame
#' @return Updated data frame with current values = prior values
copy_prior_to_current_schedule <- function(df) {
  df$CurrentExposure <- df$PriorExposure
  df
}

#' Copy prior values to current for ROL
#'
#' @param df ROL data frame
#' @return Updated data frame with current values = prior values
copy_prior_to_current_rol <- function(df) {
  df$CurrentROL <- df$PriorROL
  df
}

#' Copy prior values to current for QGU
#'
#' @param df QGU data frame
#' @return Updated data frame with current values = prior values
copy_prior_to_current_qgu <- function(df) {
  df$CurrentQGU <- df$PriorQGU
  df
}

#' Copy prior values to current for layer terms
#'
#' @param df Layer terms data frame
#' @return Updated data frame with current values = prior values
copy_prior_to_current_layer_terms <- function(df) {
  df$CurrentAttachment <- df$PriorAttachment
  df$CurrentLimit <- df$PriorLimit
  df
}

# ==============================================================================
# Bulk Paste Data Parsing
# ==============================================================================

#' Parse bulk paste data from text input
#'
#' Parses CSV or tab-separated data into a data frame with columns:
#' Satellite, Prior, Current. Handles various formats and cleans numeric values.
#'
#' @param text Raw text from text area input
#' @return Data frame with Satellite, Prior, Current columns, or NULL if parsing fails
parse_bulk_paste_data <- function(text) {
  if (is.null(text) || nchar(trimws(text)) == 0) {
    return(NULL)
  }

  # Split into lines
  lines <- strsplit(text, "\n")[[1]]
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]

  if (length(lines) == 0) {
    return(NULL)
  }

  # Parse each line
  result <- lapply(lines, function(line) {
    # Try comma first, then tab
    if (grepl(",", line)) {
      parts <- strsplit(line, ",")[[1]]
    } else if (grepl("\t", line)) {
      parts <- strsplit(line, "\t")[[1]]
    } else {
      parts <- strsplit(line, "\\s+")[[1]]
    }

    parts <- trimws(parts)

    if (length(parts) >= 3) {
      sat <- parts[1]
      prior <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", parts[2])))
      current <- suppressWarnings(as.numeric(gsub("[^0-9eE.+-]", "", parts[3])))

      if (!is.na(prior) && !is.na(current)) {
        return(data.frame(
          Satellite = sat,
          Prior = prior,
          Current = current,
          stringsAsFactors = FALSE
        ))
      }
    }
    NULL
  })

  # Combine valid rows
  result <- result[!sapply(result, is.null)]

  if (length(result) == 0) {
    return(NULL)
  }

  do.call(rbind, result)
}

# ==============================================================================
# END R/mod_input_tables.R
# ==============================================================================
