#' Bridge Equation Nowcast
#'
#' Estimate a bridge equation via OLS and produce a nowcast for the current
#' or specified period. Bridge equations are the simplest nowcasting method:
#' monthly indicators are aggregated to the quarterly frequency and used as
#' regressors for quarterly GDP growth.
#'
#' @param formula A formula with the target variable on the left-hand side
#'   and indicator variables on the right (e.g. `target ~ ind1 + ind2`).
#' @param data A data frame (or `nc_dataset`) containing the variables in
#'   `formula` plus a `date` column.
#' @param newdata A one-row data frame with indicator values for the nowcast
#'   period. If `NULL`, the last complete row of `data` is used.
#' @param alpha Numeric. Significance level for confidence intervals
#'   (default 0.05).
#'
#' @return A `nowcast_result` object.
#'
#' @export
#' @examples
#' # Synthetic example
#' set.seed(42)
#' d <- data.frame(
#'   date = as.Date(paste0(2015:2024, "-01-01")),
#'   gdp = cumsum(rnorm(10, 0.5, 0.3)),
#'   ind1 = cumsum(rnorm(10, 0.4, 0.2)),
#'   ind2 = cumsum(rnorm(10, 0.3, 0.4))
#' )
#' nc_bridge(gdp ~ ind1 + ind2, data = d)
nc_bridge <- function(formula, data, newdata = NULL, alpha = 0.05) {
  if (!inherits(formula, "formula")) {
    cli_abort("{.arg formula} must be a formula.")
  }

  # Extract data from nc_dataset
  if (inherits(data, "nc_dataset")) {
    data <- data$data
  }
  validate_data_frame(data, "data")

  # Get target name from formula

  target_name <- as.character(formula[[2]])
  if (!target_name %in% names(data)) {
    cli_abort("Target variable {.field {target_name}} not found in {.arg data}.")
  }

  # Store dates before model fitting
  if ("date" %in% names(data)) {
    dates <- data$date
  } else {
    dates <- seq_len(nrow(data))
  }

  # Fit model on complete cases
  fit_data <- data[, setdiff(names(data), "date"), drop = FALSE]
  cc <- stats::complete.cases(fit_data[, all.vars(formula), drop = FALSE])
  fit <- stats::lm(formula, data = fit_data[cc, , drop = FALSE])

  # Fitted values and residuals
  fitted_df <- data.frame(
    date = dates[cc],
    actual = fit_data[[target_name]][cc],
    fitted = as.numeric(stats::fitted(fit))
  )
  fitted_df$residual <- fitted_df$actual - fitted_df$fitted

  # Nowcast
  if (is.null(newdata)) {
    # Use last available row
    last_row <- fit_data[nrow(fit_data), , drop = FALSE]
    nc_val <- as.numeric(stats::predict(fit, newdata = last_row))
    target_period <- dates[length(dates)]
  } else {
    nc_val <- as.numeric(stats::predict(fit, newdata = newdata))
    if ("date" %in% names(newdata)) {
      target_period <- newdata$date[1]
    } else {
      target_period <- max(dates, na.rm = TRUE)
    }
  }

  # Standard error from residuals
  se <- stats::sigma(fit)
  z <- stats::qnorm(1 - alpha / 2)
  ci_lower <- nc_val - z * se
  ci_upper <- nc_val + z * se

  # Coefficients for details
  coefs <- as.data.frame(summary(fit)$coefficients)
  names(coefs) <- c("estimate", "std_error", "t_value", "p_value")

  new_nowcast_result(
    nowcast = nc_val,
    se = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    method = "bridge",
    target_period = as.Date(target_period, origin = "1970-01-01"),
    fitted_values = fitted_df,
    model = fit,
    details = list(
      coefficients = coefs,
      r_squared = summary(fit)$r.squared,
      adj_r_squared = summary(fit)$adj.r.squared,
      n_obs = sum(cc),
      formula = formula
    ),
    alpha = alpha
  )
}
