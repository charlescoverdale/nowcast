#' Temporal Aggregation
#'
#' Aggregate a time series from a higher frequency to a lower frequency.
#' For example, convert monthly data to quarterly by taking the mean, sum,
#' or last observation.
#'
#' The default aggregation function is [mean], which is appropriate for
#' **flow variables** measured as rates or indices (e.g. GDP growth, CPI).
#' For flow variables measured in levels (e.g. total retail sales), use
#' `fun = sum`. For **stock variables** (e.g. interest rates, exchange
#' rates), use `fun = function(x, ...) tail(x, 1)` to take the
#' end-of-period value.
#'
#' @param data A data frame with columns `date` (Date) and `value` (numeric).
#' @param to Character. Target frequency: `"quarterly"` or `"annual"`.
#' @param fun Function used for aggregation (default [mean]).
#'
#' @return A data frame with columns `date` and `value` at the target frequency.
#'
#' @export
#' @examples
#' monthly <- data.frame(
#'   date = seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
#'   value = rnorm(12)
#' )
#' nc_aggregate(monthly, to = "quarterly")
#' nc_aggregate(monthly, to = "annual", fun = sum)
nc_aggregate <- function(data, to = c("quarterly", "annual"),
                         fun = mean) {
  to <- match.arg(to)
  validate_data_frame(data, "data")
  if (!all(c("date", "value") %in% names(data))) {
    cli_abort("{.arg data} must have columns {.field date} and {.field value}.")
  }

  if (to == "quarterly") {
    data$period <- to_quarter_date(data$date)
  } else {
    data$period <- to_year_date(data$date)
  }

  agg <- stats::aggregate(data$value, by = list(date = data$period),
                          FUN = fun, na.rm = TRUE)
  names(agg) <- c("date", "value")
  agg[order(agg$date), ]
}

#' Stationarity Transformations
#'
#' Apply common transformations to make a time series stationary. Supports
#' first differencing, log differencing (growth rates), and standardisation.
#'
#' @param data A data frame with columns `date` and `value`, or a numeric
#'   vector.
#' @param method Character. One of `"diff"` (first difference), `"log_diff"`
#'   (log first difference, i.e. percentage growth rate), or `"standardize"`
#'   (zero mean, unit variance).
#'
#' @return A data frame with columns `date` and `value` (if input was a data
#'   frame), or a numeric vector (if input was numeric). The output is shorter
#'   by one observation for `"diff"` and `"log_diff"`.
#'
#' @export
#' @examples
#' df <- data.frame(
#'   date = seq(as.Date("2020-01-01"), as.Date("2020-06-01"), by = "month"),
#'   value = c(100, 102, 101, 105, 103, 108)
#' )
#' nc_transform(df, method = "diff")
#' nc_transform(df, method = "log_diff")
#' nc_transform(df, method = "standardize")
nc_transform <- function(data,
                         method = c("diff", "log_diff", "standardize")) {
  method <- match.arg(method)

  if (is.numeric(data) && !is.data.frame(data)) {
    return(.transform_vec(data, method))
  }

  validate_data_frame(data, "data")
  if (!all(c("date", "value") %in% names(data))) {
    cli_abort("{.arg data} must have columns {.field date} and {.field value}.")
  }

  data <- data[order(data$date), ]
  vals <- .transform_vec(data$value, method)

  if (method %in% c("diff", "log_diff")) {
    data.frame(date = data$date[-1], value = vals)
  } else {
    data.frame(date = data$date, value = vals)
  }
}

.transform_vec <- function(x, method) {
  if (method == "log_diff") {
    if (any(x <= 0, na.rm = TRUE)) {
      cli_abort("{.val log_diff} requires all values to be strictly positive.")
    }
  }
  if (method == "standardize") {
    s <- stats::sd(x, na.rm = TRUE)
    if (is.na(s) || s < .Machine$double.eps) {
      cli_abort("Cannot standardize: standard deviation is zero or NA.")
    }
  }
  switch(method,
    diff = diff(x),
    log_diff = diff(log(x)) * 100,
    standardize = (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  )
}
