# Synthetic data generators for nowcast tests

make_quarterly_gdp <- function(n_quarters = 40, ar_coef = 0.3, seed = 123) {
  set.seed(seed)
  y <- numeric(n_quarters)
  y[1] <- rnorm(1, 0.5, 0.3)
  for (i in 2:n_quarters) {
    y[i] <- ar_coef * y[i - 1] + rnorm(1, 0.5, 0.3)
  }
  start_date <- as.Date("2014-01-01")
  dates <- seq(start_date, by = "quarter", length.out = n_quarters)
  data.frame(date = dates, value = y)
}

make_monthly_indicators <- function(n_months = 120, n_indicators = 2,
                                     loadings = c(0.6, 0.4), seed = 123) {
  set.seed(seed)
  # Common factor
  factor <- rnorm(n_months)
  indicators <- list()
  for (i in seq_len(n_indicators)) {
    loading <- if (i <= length(loadings)) loadings[i] else 0.3
    vals <- loading * factor + rnorm(n_months, sd = 0.5)
    start_date <- as.Date("2014-01-01")
    dates <- seq(start_date, by = "month", length.out = n_months)
    indicators[[i]] <- data.frame(date = dates, value = vals)
  }
  list(indicators = indicators, factor = factor)
}

make_bridge_data <- function(n_quarters = 40, n_indicators = 2,
                              beta = c(0.5, 0.3), intercept = 0.2,
                              seed = 123) {
  set.seed(seed)
  dates <- seq(as.Date("2014-01-01"), by = "quarter", length.out = n_quarters)

  # Generate indicator series
  x <- matrix(rnorm(n_quarters * n_indicators), ncol = n_indicators)
  colnames(x) <- paste0("ind_", seq_len(n_indicators))

  # Generate target with known DGP
  beta_vec <- beta[seq_len(n_indicators)]
  y <- intercept + x %*% beta_vec + rnorm(n_quarters, sd = 0.3)

  d <- data.frame(date = dates, target = as.numeric(y))
  d <- cbind(d, as.data.frame(x))
  d
}

make_nowcast_data <- function(n_quarters = 40, n_indicators = 2,
                               ragged_months = 1, seed = 123) {
  set.seed(seed)

  # Quarterly target
  target <- make_quarterly_gdp(n_quarters, seed = seed)

  # Monthly indicators (3x as many months)
  n_months <- n_quarters * 3
  ind_data <- make_monthly_indicators(n_months, n_indicators, seed = seed)

  # Create ragged edge: remove last ragged_months from some indicators
  indicators <- ind_data$indicators
  for (i in seq_along(indicators)) {
    if (i > 1 && ragged_months > 0) {
      n <- nrow(indicators[[i]])
      remove_n <- ragged_months * (i - 1)
      if (remove_n < n) {
        indicators[[i]] <- indicators[[i]][seq_len(n - remove_n), ]
      }
    }
  }

  names(indicators) <- paste0("indicator_", seq_along(indicators))
  list(target = target, indicators = indicators)
}
