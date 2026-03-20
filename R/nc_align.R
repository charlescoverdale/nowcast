#' Align Mixed-Frequency Time Series
#'
#' Merges a quarterly target series with one or more higher-frequency indicator
#' series into a single aligned dataset. Monthly indicators are aggregated to
#' the quarterly frequency using the specified function (default: mean).
#'
#' @param target A data frame with columns `date` (Date) and `value` (numeric)
#'   containing the quarterly target variable (e.g. GDP growth).
#' @param ... One or more data frames, each with columns `date` and `value`.
#'   Names are used as column names in the output; if unnamed, generic names
#'   are assigned.
#' @param freq_ratio Integer. The number of high-frequency observations per
#'   low-frequency period (default 3 for monthly-to-quarterly).
#' @param agg_fun Function used to aggregate indicators to the target frequency
#'   (default [mean]).
#'
#' @return An `nc_dataset` object — a list with components:
#' \describe{
#'   \item{data}{A data frame with `date` (quarter start dates) plus columns
#'     for the target and each indicator.}
#'   \item{target_col}{Name of the target column.}
#'   \item{indicator_cols}{Character vector of indicator column names.}
#'   \item{target_freq}{Detected frequency of the target series.}
#'   \item{indicator_freq}{Detected frequency of the indicator series.}
#'   \item{availability}{A data frame summarising data availability per series.}
#' }
#'
#' @export
#' @examples
#' # Create synthetic quarterly target and monthly indicators
#' target <- data.frame(
#'   date = as.Date(paste0(2020:2023, "-01-01")),
#'   value = c(0.5, -0.3, 0.8, 0.2)
#' )
#' ind1 <- data.frame(
#'   date = seq(as.Date("2020-01-01"), as.Date("2023-12-01"), by = "month"),
#'   value = rnorm(48)
#' )
#' aligned <- nc_align(target, indicator1 = ind1)
nc_align <- function(target, ..., freq_ratio = 3L,
                     agg_fun = mean) {
  validate_data_frame(target, "target")
  if (!all(c("date", "value") %in% names(target))) {
    cli_abort("{.arg target} must have columns {.field date} and {.field value}.")
  }
  validate_date(target$date, "target$date")
  validate_numeric(target$value, "target$value")

  indicators <- list(...)
  if (length(indicators) == 0) {
    cli_abort("At least one indicator series must be provided.")
  }

  # Name indicators
  ind_names <- names(indicators)
  if (is.null(ind_names)) ind_names <- rep("", length(indicators))
  for (i in seq_along(ind_names)) {
    if (ind_names[i] == "") ind_names[i] <- paste0("indicator_", i)
  }
  names(indicators) <- ind_names

  # Validate indicators

  for (nm in ind_names) {
    ind <- indicators[[nm]]
    validate_data_frame(ind, nm)
    if (!all(c("date", "value") %in% names(ind))) {
      cli_abort("Indicator {.field {nm}} must have columns {.field date} and {.field value}.")
    }
    validate_date(ind$date, paste0(nm, "$date"))
    validate_numeric(ind$value, paste0(nm, "$value"))
  }

  # Detect frequencies
  target_freq <- detect_frequency(target$date)
  ind_freqs <- vapply(indicators, function(ind) detect_frequency(ind$date),
                      character(1))

  # Assign quarter dates to target
  target$quarter_date <- to_quarter_date(target$date)
  merged <- data.frame(date = sort(unique(target$quarter_date)))
  merged$target <- target$value[match(merged$date, target$quarter_date)]

  # Aggregate each indicator to quarterly

  for (nm in ind_names) {
    ind <- indicators[[nm]]
    ind$quarter_date <- to_quarter_date(ind$date)
    agg <- stats::aggregate(ind$value, by = list(quarter_date = ind$quarter_date),
                            FUN = agg_fun, na.rm = TRUE)
    names(agg) <- c("date", nm)
    merged <- merge(merged, agg, by = "date", all.x = TRUE)
  }

  # Build availability summary
  avail <- data.frame(
    series = c("target", ind_names),
    first_date = c(min(target$date, na.rm = TRUE),
                   vapply(indicators, function(x) min(x$date, na.rm = TRUE),
                          numeric(1))),
    last_date = c(max(target$date, na.rm = TRUE),
                  vapply(indicators, function(x) max(x$date, na.rm = TRUE),
                         numeric(1))),
    n_obs = c(sum(!is.na(target$value)),
              vapply(indicators, function(x) sum(!is.na(x$value)), integer(1))),
    stringsAsFactors = FALSE
  )
  avail$first_date <- as.Date(avail$first_date, origin = "1970-01-01")
  avail$last_date <- as.Date(avail$last_date, origin = "1970-01-01")

  new_nc_dataset(
    data = merged,
    target_col = "target",
    indicator_cols = ind_names,
    target_freq = target_freq,
    indicator_freq = ind_freqs,
    availability = avail
  )
}

#' Summarise Ragged-Edge Data Availability
#'
#' Shows which series have data through which date, highlighting the ragged
#' edge where indicators have different publication lags.
#'
#' @param data An `nc_dataset` object, or a data frame with a `date` column.
#'
#' @return A data frame with columns `series`, `first_date`, `last_date`,
#'   `n_obs`, and `n_missing`.
#'
#' @export
#' @examples
#' target <- data.frame(
#'   date = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01")),
#'   value = c(0.5, -0.3, 0.8)
#' )
#' ind <- data.frame(
#'   date = seq(as.Date("2020-01-01"), as.Date("2020-09-01"), by = "month"),
#'   value = rnorm(9)
#' )
#' ds <- nc_align(target, indicator = ind)
#' nc_ragged_edge(ds)
nc_ragged_edge <- function(data) {
  if (inherits(data, "nc_dataset")) {
    df <- data$data
    cols <- c(data$target_col, data$indicator_cols)
  } else {
    validate_data_frame(data, "data")
    if (!"date" %in% names(data)) {
      cli_abort("{.arg data} must have a {.field date} column.")
    }
    df <- data
    cols <- setdiff(names(df), "date")
  }

  result <- data.frame(
    series = cols,
    first_date = as.Date(NA),
    last_date = as.Date(NA),
    n_obs = integer(length(cols)),
    n_missing = integer(length(cols)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(cols)) {
    col <- cols[i]
    vals <- df[[col]]
    non_na <- !is.na(vals)
    result$n_obs[i] <- sum(non_na)
    result$n_missing[i] <- sum(is.na(vals))
    if (any(non_na)) {
      result$first_date[i] <- min(df$date[non_na])
      result$last_date[i] <- max(df$date[non_na])
    }
  }

  result
}
