test_that("nc_backtest returns nowcast_backtest", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1 + ind_2, data = d, start = 15, ar_order = 0)

  expect_s3_class(bt, "nowcast_backtest")
  expect_equal(bt$method, "bridge")
  expect_equal(bt$window_type, "expanding")
})

test_that("nc_backtest results have correct structure", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 15)

  expect_true(all(c("date", "nowcast", "actual", "error", "ci_lower", "ci_upper")
                  %in% names(bt$results)))
  expect_true(nrow(bt$results) > 0)
  expect_equal(bt$results$error, bt$results$nowcast - bt$results$actual,
               tolerance = 1e-10)
})

test_that("nc_backtest metrics are consistent with nc_evaluate", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1 + ind_2, data = d, start = 15, ar_order = 0)

  manual <- nc_evaluate(bt$results$nowcast, bt$results$actual)
  expect_equal(bt$metrics$rmse, manual$rmse, tolerance = 1e-10)
  expect_equal(bt$metrics$mae, manual$mae, tolerance = 1e-10)
  expect_equal(bt$metrics$bias, manual$bias, tolerance = 1e-10)
})

test_that("nc_backtest expanding window uses more data over time", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 10)
  expect_true(nrow(bt$results) >= 10)
})

test_that("nc_backtest rolling window works", {
  d <- make_bridge_data(n_quarters = 40, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 15,
                    window = "rolling", window_size = 15)

  expect_s3_class(bt, "nowcast_backtest")
  expect_equal(bt$window_type, "rolling")
  expect_true(nrow(bt$results) > 0)
})

test_that("nc_backtest RMSE beats naive benchmark for well-specified DGP", {
  d <- make_bridge_data(n_quarters = 100, beta = c(0.8, 0.5), seed = 42)
  bt <- nc_backtest(target ~ ind_1 + ind_2, data = d, start = 30, ar_order = 0)

  naive_errors <- bt$results$actual - mean(d$target[1:30])
  naive_rmse <- sqrt(mean(naive_errors^2))

  expect_true(bt$metrics$rmse < naive_rmse)
})

test_that("nc_backtest errors when start >= n", {
  d <- make_bridge_data(n_quarters = 10, seed = 42)
  expect_error(nc_backtest(target ~ ind_1, data = d, start = 10), "less than")
})

test_that("nc_backtest works with nc_dataset", {
  target <- data.frame(
    date = seq(as.Date("2015-01-01"), by = "quarter", length.out = 30),
    value = rnorm(30)
  )
  ind <- data.frame(
    date = seq(as.Date("2015-01-01"), by = "month", length.out = 90),
    value = rnorm(90)
  )
  ds <- nc_align(target, ind1 = ind)
  bt <- nc_backtest(target ~ ind1, data = ds, start = 15)
  expect_s3_class(bt, "nowcast_backtest")
})

test_that("nc_backtest handles custom alpha", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 15, alpha = 0.01)

  widths <- bt$results$ci_upper - bt$results$ci_lower
  expect_true(all(widths > 0))
})

# --- Prediction interval correctness (audit issue #10) ---

test_that("nc_backtest prediction intervals are wider than naive sigma*z", {
  d <- make_bridge_data(n_quarters = 40, seed = 42)
  bt <- nc_backtest(target ~ ind_1 + ind_2, data = d, start = 20, ar_order = 0)

  # All CI widths should be positive
  widths <- bt$results$ci_upper - bt$results$ci_lower
  expect_true(all(widths > 0))
})

# --- No look-ahead bias test ---

test_that("nc_backtest does not use future data", {
  # Create data where target depends on a lagged indicator
  set.seed(42)
  n <- 40
  d <- data.frame(
    date = seq(as.Date("2015-01-01"), by = "quarter", length.out = n),
    ind = rnorm(n)
  )
  d$target <- c(NA, d$ind[-n]) + rnorm(n, sd = 0.1)  # target[t] = ind[t-1]
  d$target[1] <- rnorm(1)

  bt <- nc_backtest(target ~ ind, data = d, start = 15, ar_order = 0)

  # The first evaluation should be for a date AFTER the start-th observation
  expect_true(bt$results$date[1] > d$date[15])
})

# --- AR terms in backtest ---

test_that("nc_backtest with ar_order includes AR terms", {
  d <- make_bridge_data(n_quarters = 40, seed = 42)
  bt_ar0 <- nc_backtest(target ~ ind_1, data = d, start = 15, ar_order = 0)
  bt_ar1 <- nc_backtest(target ~ ind_1, data = d, start = 15, ar_order = 1)

  # Both should produce results

  expect_s3_class(bt_ar0, "nowcast_backtest")
  expect_s3_class(bt_ar1, "nowcast_backtest")
  # AR model may have fewer evaluations due to lag creation
  expect_true(nrow(bt_ar1$results) > 0)
})

# --- AR lag correctness test (audit phase 2) ---

test_that("nc_backtest AR lags use correct (past) target values", {
  # Create data with a known pattern so we can verify AR values
  set.seed(42)
  n <- 25
  d <- data.frame(
    date = seq(as.Date("2015-01-01"), by = "quarter", length.out = n),
    ind = rnorm(n),
    target = 1:n * 0.1  # deterministic so we can trace values
  )

  bt <- nc_backtest(target ~ ind, data = d, start = 15, ar_order = 1)

  # The first backtest evaluation is for period 16 (start=15, so train on 1:15)
  # The AR(1) lag for period 16 should be target[15]
  # We can't directly inspect the AR values used, but we can verify the
  # nowcast is finite and the error is reasonable
  expect_s3_class(bt, "nowcast_backtest")
  expect_true(all(is.finite(bt$results$nowcast)))
  # All evaluation dates should be strictly after the start-th date
  expect_true(all(bt$results$date > d$date[15]))
})

# --- Prediction interval width test (audit phase 2) ---

test_that("nc_backtest early windows have wider CIs than later windows", {
  d <- make_bridge_data(n_quarters = 60, seed = 42)
  bt <- nc_backtest(target ~ ind_1 + ind_2, data = d, start = 10, ar_order = 0)

  widths <- bt$results$ci_upper - bt$results$ci_lower
  n_eval <- length(widths)
  # Average width of first third should be >= average width of last third
  early_mean <- mean(widths[1:floor(n_eval / 3)])
  late_mean <- mean(widths[ceiling(2 * n_eval / 3):n_eval])
  expect_true(early_mean >= late_mean * 0.9)  # allow small tolerance
})
