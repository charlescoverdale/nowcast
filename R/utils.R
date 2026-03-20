# Internal validation and date utilities

validate_date <- function(x, arg = "x") {
  if (!inherits(x, "Date")) {
    cli_abort("{.arg {arg}} must be a {.cls Date} vector.")
  }
}

validate_numeric <- function(x, arg = "x") {
  if (!is.numeric(x)) {
    cli_abort("{.arg {arg}} must be numeric.")
  }
}

validate_data_frame <- function(x, arg = "data") {
  if (!is.data.frame(x)) {
    cli_abort("{.arg {arg}} must be a data frame.")
  }
}

# Detect frequency from a sorted date vector
detect_frequency <- function(dates) {
  if (length(dates) < 2) return("unknown")
  diffs <- as.numeric(diff(sort(dates)))
  med_diff <- stats::median(diffs)
  if (med_diff <= 7) return("daily")
  if (med_diff <= 35) return("monthly")
  if (med_diff <= 100) return("quarterly")
  if (med_diff <= 200) return("semi-annual")
  return("annual")
}

# Assign quarter dates (first day of quarter) from dates
to_quarter_date <- function(dates) {
  m <- as.integer(format(dates, "%m"))
  q <- (m - 1L) %/% 3L
  as.Date(paste0(format(dates, "%Y"), "-", sprintf("%02d", q * 3L + 1L), "-01"))
}

# Assign year dates from dates
to_year_date <- function(dates) {
  as.Date(paste0(format(dates, "%Y"), "-01-01"))
}

# Count available observations per quarter
count_obs_in_quarter <- function(dates) {

  qd <- to_quarter_date(dates)
  as.data.frame(table(quarter = qd), stringsAsFactors = FALSE)
}

# Construct a nowcast_result object
new_nowcast_result <- function(nowcast, se = NA_real_, ci_lower = NA_real_,
                                ci_upper = NA_real_, method, target_period,
                                info_set_date = Sys.Date(),
                                fitted_values = data.frame(),
                                model = NULL, details = list(),
                                alpha = 0.05) {
  structure(
    list(
      nowcast = nowcast,
      se = se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      method = method,
      target_period = target_period,
      info_set_date = info_set_date,
      fitted_values = fitted_values,
      model = model,
      details = details,
      alpha = alpha
    ),
    class = "nowcast_result"
  )
}

# Construct an nc_dataset object
new_nc_dataset <- function(data, target_col, indicator_cols, target_freq,
                            indicator_freq, availability) {
  structure(
    list(
      data = data,
      target_col = target_col,
      indicator_cols = indicator_cols,
      target_freq = target_freq,
      indicator_freq = indicator_freq,
      availability = availability
    ),
    class = "nc_dataset"
  )
}

# Construct a nowcast_backtest object
new_nowcast_backtest <- function(results, actuals, method, window_type,
                                  metrics) {
  structure(
    list(
      results = results,
      actuals = actuals,
      method = method,
      window_type = window_type,
      metrics = metrics
    ),
    class = "nowcast_backtest"
  )
}
