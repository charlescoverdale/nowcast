#' Predict Method for Nowcast Results
#'
#' Generate predictions from a fitted nowcast model for new indicator data.
#'
#' @param object A `nowcast_result` object.
#' @param newdata A data frame with indicator values.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of predictions.
#'
#' @export
predict.nowcast_result <- function(object, newdata, ...) {

  if (is.null(object$model)) {
    cli_abort("No fitted model available for prediction.")
  }

  as.numeric(stats::predict(object$model, newdata = newdata))
}
