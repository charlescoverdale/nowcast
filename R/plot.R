#' Plot Method for Nowcast Results
#'
#' Plots actual versus fitted values from the nowcast model, with the
#' nowcast point highlighted.
#'
#' @param x A `nowcast_result` object.
#' @param ... Additional arguments passed to [plot()].
#'
#' @return The input object, invisibly.
#'
#' @export
plot.nowcast_result <- function(x, ...) {
  if (nrow(x$fitted_values) == 0) {
    cli_warn("No fitted values to plot.")
    return(invisible(x))
  }

  fv <- x$fitted_values
  ylim <- range(c(fv$actual, fv$fitted, x$nowcast, x$ci_lower, x$ci_upper),
                na.rm = TRUE)
  ylim <- ylim + c(-1, 1) * diff(ylim) * 0.1

  plot(fv$date, fv$actual, type = "l", lwd = 2,
       xlab = "Date", ylab = "Value",
       ylim = ylim,
       main = paste0("Nowcast: ", x$method),
       ...)
  lines(fv$date, fv$fitted, col = grDevices::rgb(0.2, 0.4, 0.8), lwd = 1.5,
        lty = 2)

  # Nowcast point
  points(x$target_period, x$nowcast, pch = 19, col = "red", cex = 1.5)

  # CI
  if (!is.na(x$ci_lower) && !is.na(x$ci_upper)) {
    arrows_x <- x$target_period
    graphics::arrows(arrows_x, x$ci_lower, arrows_x, x$ci_upper,
                     angle = 90, code = 3, length = 0.1, col = "red", lwd = 1.5)
  }

  legend("topleft", legend = c("Actual", "Fitted", "Nowcast"),
         col = c("black", grDevices::rgb(0.2, 0.4, 0.8), "red"),
         lty = c(1, 2, NA), pch = c(NA, NA, 19), lwd = c(2, 1.5, NA),
         bty = "n", cex = 0.8)

  invisible(x)
}

#' Plot Method for Backtest Results
#'
#' Plots nowcast versus actual values over the backtest evaluation period.
#'
#' @param x A `nowcast_backtest` object.
#' @param ... Additional arguments passed to [plot()].
#'
#' @return The input object, invisibly.
#'
#' @export
plot.nowcast_backtest <- function(x, ...) {
  res <- x$results
  ylim <- range(c(res$nowcast, res$actual, res$ci_lower, res$ci_upper),
                na.rm = TRUE)
  ylim <- ylim + c(-1, 1) * diff(ylim) * 0.1

  plot(res$date, res$actual, type = "l", lwd = 2,
       xlab = "Date", ylab = "Value",
       ylim = ylim,
       main = paste0("Backtest: ", x$method, " (", x$window_type, ")"),
       ...)

  # CI band
  polygon(c(res$date, rev(res$date)),
          c(res$ci_lower, rev(res$ci_upper)),
          col = grDevices::rgb(0.2, 0.4, 0.8, 0.15), border = NA)

  lines(res$date, res$nowcast, col = grDevices::rgb(0.2, 0.4, 0.8),
        lwd = 1.5, lty = 2)

  legend("topleft",
         legend = c("Actual", "Nowcast", "CI"),
         col = c("black", grDevices::rgb(0.2, 0.4, 0.8),
                 grDevices::rgb(0.2, 0.4, 0.8, 0.15)),
         lty = c(1, 2, NA), lwd = c(2, 1.5, NA),
         pch = c(NA, NA, 15),
         bty = "n", cex = 0.8)

  invisible(x)
}
