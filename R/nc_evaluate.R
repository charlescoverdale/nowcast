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

  errors <- forecast - actual
  data.frame(
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),
    mae = mean(abs(errors), na.rm = TRUE),
    bias = mean(errors, na.rm = TRUE)
  )
}

#' Diebold-Mariano Test for Equal Predictive Accuracy
#'
#' Tests whether two sets of forecast errors have equal predictive accuracy,
#' following Diebold and Mariano (1995).
#'
#' @param e1 Numeric vector. Forecast errors from model 1.
#' @param e2 Numeric vector. Forecast errors from model 2 (same length).
#' @param alternative Character. `"two.sided"`, `"less"` (model 1 better),
#'   or `"greater"` (model 2 better).
#' @param h Integer. Forecast horizon (default 1). Used for the
#'   Newey-West-style variance correction.
#' @param loss Character. Loss function: `"squared"` or `"absolute"`.
#'
#' @return A list with components `statistic`, `p_value`, `alternative`,
#'   and `method`.
#'
#' @references
#' Diebold, F.X. and Mariano, R.S. (1995). Comparing predictive accuracy.
#' *Journal of Business & Economic Statistics*, 13(3), 253--263.
#' \doi{10.1080/07350015.1995.10524599}
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

  # Loss differentials
  if (loss == "squared") {
    d <- e1^2 - e2^2
  } else {
    d <- abs(e1) - abs(e2)
  }

  d_bar <- mean(d)

  # HAC variance estimate (Newey-West with bandwidth h-1)
  gamma_0 <- stats::var(d)
  if (h > 1) {
    gamma_k <- vapply(seq_len(h - 1), function(k) {
      stats::cov(d[1:(n - k)], d[(k + 1):n])
    }, numeric(1))
    var_d <- (gamma_0 + 2 * sum(gamma_k)) / n
  } else {
    var_d <- gamma_0 / n
  }

  if (var_d <= 0) {
    cli_warn("Estimated variance is non-positive; returning NA.")
    return(list(statistic = NA_real_, p_value = NA_real_,
                alternative = alternative, method = "Diebold-Mariano"))
  }

  dm_stat <- d_bar / sqrt(var_d)

  p_val <- switch(alternative,
    two.sided = 2 * stats::pnorm(-abs(dm_stat)),
    less = stats::pnorm(dm_stat),
    greater = stats::pnorm(-dm_stat)
  )

  list(
    statistic = dm_stat,
    p_value = p_val,
    alternative = alternative,
    method = "Diebold-Mariano"
  )
}
