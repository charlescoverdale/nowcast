#' Print Method for Nowcast Results
#'
#' @param x A `nowcast_result` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
print.nowcast_result <- function(x, ...) {
  method_names <- c(
    bridge = "Bridge Equation",
    midas = "MIDAS Regression",
    dfm = "Dynamic Factor Model",
    combined = "Combined Nowcast"
  )
  method_label <- method_names[x$method]

  cli_h1("Nowcast ({method_label})")
  cli_bullets(c(
    "*" = "Nowcast: {.val {round(x$nowcast, 4)}}",
    "*" = "SE: {.val {round(x$se, 4)}}",
    "*" = "{.val {(1 - x$alpha) * 100}%} CI: [{.val {round(x$ci_lower, 4)}}, {.val {round(x$ci_upper, 4)}}]",
    "*" = "Target period: {.val {x$target_period}}"
  ))

  if (!is.null(x$details$r_squared)) {
    cli_bullets(c(
      "*" = "R-squared: {.val {round(x$details$r_squared, 4)}} | Obs: {.val {x$details$n_obs}}"
    ))
  }
  invisible(x)
}

#' Summary Method for Nowcast Results
#'
#' @param object A `nowcast_result` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
summary.nowcast_result <- function(object, ...) {
  print(object, ...)

  if (!is.null(object$details$coefficients)) {
    cli_h1("Coefficients")
    coefs <- object$details$coefficients
    for (i in seq_len(nrow(coefs))) {
      sig <- ""
      pv <- coefs$p_value[i]
      if (!is.na(pv)) {
        if (pv < 0.001) sig <- " ***"
        else if (pv < 0.01) sig <- " **"
        else if (pv < 0.05) sig <- " *"
      }
      cli_bullets(c("*" = paste0(
        rownames(coefs)[i], ": ",
        round(coefs$estimate[i], 4),
        " (SE: ", round(coefs$std_error[i], 4), ")", sig
      )))
    }
  }

  invisible(object)
}

#' Print Method for nc_dataset Objects
#'
#' @param x An `nc_dataset` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
print.nc_dataset <- function(x, ...) {
  cli_h1("Aligned Nowcasting Dataset")
  cli_bullets(c(
    "*" = "Target: {.field {x$target_col}} ({x$target_freq})",
    "*" = "Indicators: {.val {length(x$indicator_cols)}} ({paste(x$indicator_cols, collapse = ', ')})",
    "*" = "Periods: {.val {nrow(x$data)}} ({min(x$data$date)} to {max(x$data$date)})"
  ))
  invisible(x)
}

#' Print Method for Backtest Results
#'
#' @param x A `nowcast_backtest` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
print.nowcast_backtest <- function(x, ...) {
  cli_h1("Nowcast Backtest ({x$method}, {x$window_type})")
  cli_bullets(c(
    "*" = "Evaluations: {.val {nrow(x$results)}}",
    "*" = "Period: {.val {min(x$results$date)}} to {.val {max(x$results$date)}}",
    "*" = "RMSE: {.val {round(x$metrics$rmse, 4)}}",
    "*" = "MAE: {.val {round(x$metrics$mae, 4)}}",
    "*" = "Bias: {.val {round(x$metrics$bias, 4)}}"
  ))
  invisible(x)
}

#' Summary Method for Backtest Results
#'
#' @param object A `nowcast_backtest` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return The input object, invisibly.
#'
#' @export
summary.nowcast_backtest <- function(object, ...) {
  print(object, ...)

  cli_h1("Error Distribution")
  err <- object$results$error
  cli_bullets(c(
    "*" = "Mean: {.val {round(mean(err), 4)}}",
    "*" = "Median: {.val {round(stats::median(err), 4)}}",
    "*" = "SD: {.val {round(stats::sd(err), 4)}}",
    "*" = "Min: {.val {round(min(err), 4)}} | Max: {.val {round(max(err), 4)}}"
  ))
  invisible(object)
}
