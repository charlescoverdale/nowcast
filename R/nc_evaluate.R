#' Evaluate Nowcast Accuracy
#'
#' Compute standard forecast evaluation metrics: root mean squared error
#' (RMSE), mean absolute error (MAE), and mean bias.
#'
#' @param forecast Numeric vector of nowcast/forecast values.
#' @param actual Numeric vector of realised values (same length as `forecast`).
#'
#' @return A data frame with one row and columns `rmse`, `mae`, and `bias`.
#'
#' @export
#' @examples
#' nc_evaluate(c(1.0, 2.0, 3.0), c(1.1, 1.8, 3.2))
nc_evaluate <- function(forecast, actual) {
  validate_numeric(forecast, "forecast")
  validate_numeric(actual, "actual")
  if (length(forecast) != length(actual)) {
    cli_abort("{.arg forecast} and {.arg actual} must have the same length.")
  }

  # Remove pairs with NaN or Inf
  valid <- is.finite(forecast) & is.finite(actual)
  if (sum(valid) == 0) {
    cli_abort("No finite forecast-actual pairs.")
  }
  forecast <- forecast[valid]
  actual <- actual[valid]

  errors <- forecast - actual
  data.frame(
    rmse = sqrt(mean(errors^2)),
    mae = mean(abs(errors)),
    bias = mean(errors)
  )
}

#' Diebold-Mariano Test for Equal Predictive Accuracy
#'
#' Tests whether two sets of forecast errors have equal predictive accuracy.
#' Implements the modified test of Harvey, Leybourne, and Newbold (1997),
#' which applies a finite-sample correction to the original Diebold and
#' Mariano (1995) statistic and uses the *t* distribution rather than the
#' normal. The Bartlett (triangular) kernel is used for HAC variance
#' estimation, which guarantees a non-negative variance estimate.
#'
#' @param e1 Numeric vector. Forecast errors from model 1.
#' @param e2 Numeric vector. Forecast errors from model 2 (same length).
#' @param alternative Character. `"two.sided"`, `"less"` (model 1 better),
#'   or `"greater"` (model 2 better).
#' @param h Integer. Forecast horizon (default 1). Used for the
#'   Newey-West bandwidth and HLN correction.
#' @param loss Character. Loss function: `"squared"` or `"absolute"`.
#'
#' @return A list with components `statistic`, `p_value`, `alternative`,
#'   `method`, and `n`.
#'
#' @references
#' Diebold, F.X. and Mariano, R.S. (1995). Comparing predictive accuracy.
#' *Journal of Business & Economic Statistics*, 13(3), 253--263.
#' \doi{10.1080/07350015.1995.10524599}
#'
#' Harvey, D., Leybourne, S. and Newbold, P. (1997). Testing the equality
#' of prediction mean squared errors. *International Journal of Forecasting*,
#' 13(2), 281--291. \doi{10.1016/S0169-2070(96)00719-4}
#'
#' @export
#' @examples
#' set.seed(1)
#' e1 <- rnorm(50, sd = 1)
#' e2 <- rnorm(50, sd = 1.5)
#' nc_dm_test(e1, e2)
nc_dm_test <- function(e1, e2, alternative = c("two.sided", "less", "greater"),
                       h = 1L, loss = c("squared", "absolute")) {
  alternative <- match.arg(alternative)
  loss <- match.arg(loss)
  validate_numeric(e1, "e1")
  validate_numeric(e2, "e2")
  if (length(e1) != length(e2)) {
    cli_abort("{.arg e1} and {.arg e2} must have the same length.")
  }

  n <- length(e1)
  if (h >= n) {
    cli_abort("{.arg h} must be less than the number of observations ({n}).")
  }

  # Loss differentials
  if (loss == "squared") {
    d <- e1^2 - e2^2
  } else {
    d <- abs(e1) - abs(e2)
  }

  d_bar <- mean(d)

  # HAC variance estimate using Bartlett (triangular) kernel
  # This guarantees non-negative variance (Newey-West 1987)
  acf_obj <- stats::acf(d, lag.max = max(1L, h - 1L), type = "covariance",
                         plot = FALSE)
  gamma <- as.numeric(acf_obj$acf)  # gamma[1] = var, gamma[2] = cov(lag 1), ...

  if (h > 1) {
    # Bartlett weights: w_k = 1 - k/h
    weights <- 1 - seq_len(h - 1) / h
    var_d <- (gamma[1] + 2 * sum(weights * gamma[-1])) / n
  } else {
    var_d <- gamma[1] / n
  }

  if (var_d <= 0) {
    cli_warn("Estimated variance is non-positive; returning NA.")
    return(list(statistic = NA_real_, p_value = NA_real_,
                alternative = alternative, method = "Diebold-Mariano (HLN)",
                n = n))
  }

  dm_stat <- d_bar / sqrt(var_d)

  # Harvey-Leybourne-Newbold (1997) small-sample correction
  hln_factor <- sqrt((n + 1 - 2 * h + h * (h - 1) / n) / n)
  dm_stat <- dm_stat * hln_factor

  # Use t-distribution with n-1 degrees of freedom (HLN recommendation)
  p_val <- switch(alternative,
    two.sided = 2 * stats::pt(-abs(dm_stat), df = n - 1),
    less = stats::pt(dm_stat, df = n - 1),
    greater = stats::pt(-dm_stat, df = n - 1)
  )

  list(
    statistic = dm_stat,
    p_value = p_val,
    alternative = alternative,
    method = "Diebold-Mariano (HLN)",
    n = n
  )
}
