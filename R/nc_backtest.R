#' Pseudo-Real-Time Backtesting
#'
#' Evaluate a nowcasting method by simulating its real-time performance.
#' At each evaluation date, the model is estimated using only data that
#' would have been available at that point (expanding or rolling window),
#' then used to nowcast the next period.
#'
#' @param formula A formula for the bridge equation (e.g. `target ~ ind1 + ind2`).
#' @param data A data frame (or `nc_dataset`) with a `date` column.
#' @param method Character. Currently only `"bridge"` is supported.
#' @param start Integer. Minimum number of observations before the first
#'   backtest evaluation (default 10).
#' @param window Character. `"expanding"` (default) or `"rolling"`.
#' @param window_size Integer. Number of observations in the rolling window
#'   (ignored if `window = "expanding"`).
#' @param alpha Numeric. Significance level for confidence intervals
#'   (default 0.05).
#'
#' @return A `nowcast_backtest` object with components:
#' \describe{
#'   \item{results}{A data frame with columns `date`, `nowcast`, `actual`,
#'     `error`, `ci_lower`, `ci_upper`.}
#'   \item{actuals}{The actual values used for evaluation.}
#'   \item{method}{The method used.}
#'   \item{window_type}{`"expanding"` or `"rolling"`.}
#'   \item{metrics}{A data frame with RMSE, MAE, and bias.}
#' }
#'
#' @export
#' @examples
#' set.seed(42)
#' n <- 30
#' d <- data.frame(
#'   date = seq(as.Date("2015-01-01"), by = "quarter", length.out = n),
#'   gdp = cumsum(rnorm(n, 0.5, 0.3)),
#'   ind1 = cumsum(rnorm(n, 0.4, 0.2))
#' )
#' bt <- nc_backtest(gdp ~ ind1, data = d, start = 15)
#' bt
nc_backtest <- function(formula, data, method = "bridge",
                        start = 10L, window = c("expanding", "rolling"),
                        window_size = 20L, alpha = 0.05) {
  window <- match.arg(window)

  if (inherits(data, "nc_dataset")) {
    data <- data$data
  }
  validate_data_frame(data, "data")

  if (!"date" %in% names(data)) {
    cli_abort("{.arg data} must have a {.field date} column.")
  }

  data <- data[order(data$date), ]
  target_name <- as.character(formula[[2]])
  n <- nrow(data)

  if (start >= n) {
    cli_abort("{.arg start} must be less than the number of rows in {.arg data}.")
  }

  # Complete cases for the formula variables
  all_vars <- all.vars(formula)
  cc <- stats::complete.cases(data[, all_vars, drop = FALSE])

  results <- vector("list", n - start)
  k <- 0L

  for (i in (start + 1):n) {
    # Determine training window
    if (window == "expanding") {
      idx <- seq_len(i - 1)
    } else {
      idx <- max(1L, i - window_size):(i - 1)
    }

    train <- data[idx, , drop = FALSE]
    train_cc <- cc[idx]

    # Skip if too few complete cases
    if (sum(train_cc) < length(all_vars) + 1) next

    # Nowcast for period i
    newdata_row <- data[i, , drop = FALSE]

    # Skip if newdata has missing indicators
    ind_vars <- all.vars(formula[[3]])
    if (any(is.na(newdata_row[, ind_vars, drop = FALSE]))) next

    fit <- tryCatch(
      stats::lm(formula, data = train),
      error = function(e) NULL
    )
    if (is.null(fit)) next

    nc_val <- as.numeric(stats::predict(fit, newdata = newdata_row))
    se <- stats::sigma(fit)
    z <- stats::qnorm(1 - alpha / 2)
    actual_val <- data[[target_name]][i]

    k <- k + 1L
    results[[k]] <- data.frame(
      date = data$date[i],
      nowcast = nc_val,
      actual = actual_val,
      error = nc_val - actual_val,
      ci_lower = nc_val - z * se,
      ci_upper = nc_val + z * se
    )
  }

  if (k == 0) {
    cli_abort("No valid backtest evaluations could be produced.")
  }

  results_df <- do.call(rbind, results[seq_len(k)])
  rownames(results_df) <- NULL

  metrics <- nc_evaluate(results_df$nowcast, results_df$actual)

  new_nowcast_backtest(
    results = results_df,
    actuals = results_df$actual,
    method = method,
    window_type = window,
    metrics = metrics
  )
}
