#' Bridge Equation Nowcast
#'
#' Estimate a bridge equation via OLS and produce a nowcast for the current
#' or specified period. Bridge equations translate higher-frequency indicators
#' (e.g. monthly) into estimates of a lower-frequency target (e.g. quarterly
#' GDP). Following standard central bank practice (Baffigi et al. 2004,
#' Runstler and Sedillot 2003), an autoregressive term for the target
#' variable can be included via the `ar_order` argument.
#'
#' Prediction intervals are computed using the full prediction standard error,
#' which accounts for both residual variance and estimation uncertainty in the
#' coefficients, evaluated against a *t* distribution with appropriate degrees
#' of freedom.
#'
#' When `ar_order > 0`, the model includes a lagged dependent variable.
#' The reported standard errors assume homoskedastic, serially uncorrelated
#' errors. If residuals exhibit autocorrelation (indicated by the Durbin-Watson
#' statistic in `details$dw_stat`), consider extracting the fitted model via
#' `result$model` and applying HAC standard errors from the
#' \pkg{sandwich} package.
#'
#' @param formula A formula with the target variable on the left-hand side
#'   and indicator variables on the right (e.g. `target ~ ind1 + ind2`).
#' @param data A data frame (or `nc_dataset`) containing the variables in
#'   `formula` plus a `date` column.
#' @param newdata A one-row data frame with indicator values for the nowcast
#'   period. If `NULL`, the last complete row of `data` is used.
#' @param ar_order Integer. Number of autoregressive lags of the target to
#'   include (default 1). Set to 0 for a static bridge equation with no
#'   AR terms.
#' @param alpha Numeric. Significance level for prediction intervals
#'   (default 0.05).
#'
#' @return A `nowcast_result` object.
#'
#' @references
#' Baffigi, A., Golinelli, R. and Parigi, G. (2004). Bridge models to
#' forecast the euro area GDP. *International Journal of Forecasting*,
#' 20(3), 447--460.
#'
#' @export
#' @examples
#' \donttest{
#' # Synthetic example
#' set.seed(42)
#' d <- data.frame(
#'   date = as.Date(paste0(2015:2024, "-01-01")),
#'   gdp = cumsum(rnorm(10, 0.5, 0.3)),
#'   ind1 = cumsum(rnorm(10, 0.4, 0.2)),
#'   ind2 = cumsum(rnorm(10, 0.3, 0.4))
#' )
#' nc_bridge(gdp ~ ind1 + ind2, data = d)
#' }
nc_bridge <- function(formula, data, newdata = NULL, ar_order = 1L,
                      alpha = 0.05) {
  if (!inherits(formula, "formula")) {
    cli_abort("{.arg formula} must be a formula.")
  }
  if (length(formula) < 3) {
    cli_abort("{.arg formula} must be two-sided (e.g. {.code y ~ x}).")
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

  # Add AR lags if requested
  fit_data <- data[, setdiff(names(data), "date"), drop = FALSE]
  ar_names <- character(0)
  if (ar_order > 0) {
    for (lag in seq_len(ar_order)) {
      ar_col <- paste0(".ar_", lag)
      ar_names <- c(ar_names, ar_col)
      fit_data[[ar_col]] <- c(rep(NA_real_, lag),
                               fit_data[[target_name]][seq_len(nrow(fit_data) - lag)])
    }
    # Extend formula to include AR terms
    ar_terms <- paste(ar_names, collapse = " + ")
    formula <- stats::as.formula(
      paste(deparse(formula), "+", ar_terms),
      env = environment(formula)
    )
  }

  # Check for zero-variance columns among predictors
  pred_vars <- all.vars(formula[[3]])
  for (pv in pred_vars) {
    if (pv %in% names(fit_data) && stats::var(fit_data[[pv]], na.rm = TRUE) < .Machine$double.eps) {
      cli_abort("Predictor {.field {pv}} has zero variance.")
    }
  }

  # Fit model on complete cases
  cc <- stats::complete.cases(fit_data[, all.vars(formula), drop = FALSE])
  fit <- tryCatch(
    stats::lm(formula, data = fit_data[cc, , drop = FALSE]),
    error = function(e) {
      cli_abort(c(
        "Model fitting failed.",
        "i" = "Check for multicollinearity or insufficient data.",
        "x" = conditionMessage(e)
      ))
    }
  )

  # Warn if any coefficients are NA (rank-deficient)
  if (any(is.na(stats::coef(fit)))) {
    cli_warn("Some coefficients are {.val NA} -- possible multicollinearity.")
  }

  # Fitted values and residuals
  fitted_df <- data.frame(
    date = dates[cc],
    actual = fit_data[[target_name]][cc],
    fitted = as.numeric(stats::fitted(fit))
  )
  fitted_df$residual <- fitted_df$actual - fitted_df$fitted

  # Nowcast with proper prediction intervals
  if (is.null(newdata)) {
    last_row <- fit_data[nrow(fit_data), , drop = FALSE]
    pred <- stats::predict(fit, newdata = last_row,
                           interval = "prediction", level = 1 - alpha)
    target_period <- dates[length(dates)]
  } else {
    # Add AR terms to newdata if needed
    if (ar_order > 0) {
      for (lag in seq_len(ar_order)) {
        ar_col <- paste0(".ar_", lag)
        if (!ar_col %in% names(newdata)) {
          # Use the last available target values
          n_data <- nrow(fit_data)
          newdata[[ar_col]] <- fit_data[[target_name]][n_data - lag + 1]
        }
      }
    }
    pred <- stats::predict(fit, newdata = newdata,
                           interval = "prediction", level = 1 - alpha)
    if ("date" %in% names(newdata)) {
      target_period <- newdata$date[1]
    } else {
      target_period <- max(dates, na.rm = TRUE)
    }
  }

  nc_val <- as.numeric(pred[, "fit"])
  ci_lower <- as.numeric(pred[, "lwr"])
  ci_upper <- as.numeric(pred[, "upr"])

  # SE from prediction interval width (back out from t-quantile)
  df_resid <- fit$df.residual
  t_crit <- stats::qt(1 - alpha / 2, df = df_resid)
  se <- (ci_upper - ci_lower) / (2 * t_crit)

  # Coefficients for details
  coefs <- as.data.frame(summary(fit)$coefficients)
  names(coefs) <- c("estimate", "std_error", "t_value", "p_value")

  # Durbin-Watson statistic for residual autocorrelation diagnostic
  resids <- stats::residuals(fit)
  dw_stat <- sum(diff(resids)^2) / sum(resids^2)
  if (ar_order > 0 && (dw_stat < 1.5 || dw_stat > 2.5)) {
    cli_inform(c(
      "i" = "Durbin-Watson = {.val {round(dw_stat, 3)}} suggests residual autocorrelation.",
      "i" = "OLS standard errors may be unreliable. Consider HAC SEs via {.pkg sandwich}."
    ))
  }

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
      aic = stats::AIC(fit),
      bic = stats::BIC(fit),
      dw_stat = dw_stat,
      n_obs = sum(cc),
      ar_order = ar_order,
      formula = formula
    ),
    alpha = alpha
  )
}
