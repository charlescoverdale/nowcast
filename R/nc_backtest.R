#' Pseudo-Real-Time Backtesting
#'
#' Evaluate a nowcasting method by simulating its real-time performance on
#' final revised data. At each evaluation step, the model is estimated using
#' only data from periods 1 to *i*-1 (expanding window) or a rolling window
#' ending at *i*-1, then used to produce a nowcast for period *i*. The
#' nowcast is compared against the actual value at period *i*.
#'
#' This is a **pseudo-real-time** exercise: it uses final revised data
#' throughout, not the data that would have been available at each point
#' in time (vintage data). As noted by Banbura et al. (2013, ECB WP 1564),
#' data revisions can be material for some variables, so results may
#' overstate true real-time accuracy.
#'
#' Prediction intervals are computed using the full prediction standard
#' error from `predict.lm(..., interval = "prediction")`, accounting for
#' both residual variance and coefficient estimation uncertainty.
#'
#' @param formula A formula for the bridge equation (e.g. `target ~ ind1 + ind2`).
#' @param data A data frame (or `nc_dataset`) with a `date` column.
#' @param method Character. Currently only `"bridge"` is supported.
#' @param ar_order Integer. Number of autoregressive lags of the target to
#'   include (default 1). Set to 0 for a static bridge equation.
#' @param start Integer. Minimum number of observations before the first
#'   backtest evaluation (default 10).
#' @param window Character. `"expanding"` (default) or `"rolling"`.
#' @param window_size Integer. Number of observations in the rolling window
#'   (ignored if `window = "expanding"`).
#' @param alpha Numeric. Significance level for prediction intervals
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
nc_backtest <- function(formula, data, method = "bridge", ar_order = 1L,
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

  results <- vector("list", n - start)
  k <- 0L

  for (i in (start + 1):n) {
    # Determine training window: periods 1 to i-1
    if (window == "expanding") {
      idx <- seq_len(i - 1)
    } else {
      idx <- max(1L, i - window_size):(i - 1)
    }

    train <- data[idx, , drop = FALSE]

    # Build AR lags for training data
    train_fit <- train[, setdiff(names(train), "date"), drop = FALSE]
    ar_names <- character(0)
    work_formula <- formula
    if (ar_order > 0) {
      for (lag in seq_len(ar_order)) {
        ar_col <- paste0(".ar_", lag)
        ar_names <- c(ar_names, ar_col)
        train_fit[[ar_col]] <- c(rep(NA_real_, lag),
                                  train_fit[[target_name]][seq_len(nrow(train_fit) - lag)])
      }
      ar_terms <- paste(ar_names, collapse = " + ")
      work_formula <- stats::as.formula(
        paste(deparse(formula), "+", ar_terms),
        env = environment(formula)
      )
    }

    # Complete cases check
    all_vars <- all.vars(work_formula)
    train_cc <- stats::complete.cases(train_fit[, all_vars, drop = FALSE])
    if (sum(train_cc) < length(all_vars) + 1) next

    # Prepare newdata for period i
    newdata_row <- data[i, setdiff(names(data), "date"), drop = FALSE]

    # Skip if newdata has missing indicators
    ind_vars <- all.vars(formula[[3]])
    if (any(is.na(newdata_row[, ind_vars, drop = FALSE]))) next

    # Add AR lags to newdata
    if (ar_order > 0) {
      for (lag in seq_len(ar_order)) {
        ar_col <- paste0(".ar_", lag)
        # Use target value from i-lag (which is in training data)
        src_idx <- i - lag
        if (src_idx >= 1) {
          newdata_row[[ar_col]] <- data[[target_name]][src_idx]
        } else {
          newdata_row[[ar_col]] <- NA_real_
        }
      }
      if (any(is.na(newdata_row[, ar_names, drop = FALSE]))) next
    }

    fit <- tryCatch(
      stats::lm(work_formula, data = train_fit),
      error = function(e) NULL
    )
    if (is.null(fit)) next

    pred <- tryCatch(
      stats::predict(fit, newdata = newdata_row,
                     interval = "prediction", level = 1 - alpha),
      error = function(e) NULL
    )
    if (is.null(pred)) next

    nc_val <- as.numeric(pred[, "fit"])
    actual_val <- data[[target_name]][i]

    k <- k + 1L
    results[[k]] <- data.frame(
      date = data$date[i],
      nowcast = nc_val,
      actual = actual_val,
      error = nc_val - actual_val,
      ci_lower = as.numeric(pred[, "lwr"]),
      ci_upper = as.numeric(pred[, "upr"])
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
