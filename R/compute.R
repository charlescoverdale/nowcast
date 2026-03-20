#' List Available Nowcasting Methods
#'
#' Returns a data frame describing the nowcasting methods implemented in
#' the package.
#'
#' @return A data frame with columns `method`, `description`, and `available`.
#'
#' @export
#' @examples
#' nc_available()
nc_available <- function() {
  data.frame(
    method = "bridge",
    description = "Bridge equation via OLS with optional AR terms",
    available = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Compute a Nowcast by Method Name
#'
#' A generic dispatcher that calls the appropriate `nc_*` function based
#' on a string method name. Useful for programmatic workflows.
#'
#' @param data A data frame or `nc_dataset`.
#' @param method Character. Name of the method: `"bridge"`.
#' @param ... Additional arguments passed to the underlying function.
#'
#' @return A `nowcast_result` object.
#'
#' @export
#' @examples
#' set.seed(42)
#' d <- data.frame(
#'   date = as.Date(paste0(2015:2024, "-01-01")),
#'   gdp = cumsum(rnorm(10, 0.5, 0.3)),
#'   ind1 = cumsum(rnorm(10, 0.4, 0.2))
#' )
#' nc_compute(gdp ~ ind1, data = d, method = "bridge")
nc_compute <- function(data, method = "bridge", ...) {
  method <- match.arg(method, choices = c("bridge"))

  switch(method,
    bridge = {
      args <- list(...)
      if ("formula" %in% names(args)) {
        nc_bridge(formula = args$formula, data = data,
                  newdata = args$newdata,
                  ar_order = args$ar_order %||% 1L,
                  alpha = args$alpha %||% 0.05)
      } else {
        # If first unnamed arg is a formula
        dots <- list(...)
        if (length(dots) > 0 && inherits(dots[[1]], "formula")) {
          nc_bridge(formula = dots[[1]], data = data)
        } else {
          cli_abort("Method {.val bridge} requires a {.arg formula}.")
        }
      }
    }
  )
}

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
