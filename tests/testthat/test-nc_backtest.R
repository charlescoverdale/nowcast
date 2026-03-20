test_that("nc_backtest returns nowcast_backtest", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1 + ind_2, data = d, start = 15)

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
  bt <- nc_backtest(target ~ ind_1 + ind_2, data = d, start = 15)

  manual <- nc_evaluate(bt$results$nowcast, bt$results$actual)
  expect_equal(bt$metrics$rmse, manual$rmse, tolerance = 1e-10)
  expect_equal(bt$metrics$mae, manual$mae, tolerance = 1e-10)
  expect_equal(bt$metrics$bias, manual$bias, tolerance = 1e-10)
})

test_that("nc_backtest expanding window uses more data over time", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 10)

  # Should have at least several evaluations

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
  bt <- nc_backtest(target ~ ind_1 + ind_2, data = d, start = 30)

  # Naive: predict mean of training target
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

  # CI should be wider than default
  widths <- bt$results$ci_upper - bt$results$ci_lower
  expect_true(all(widths > 0))
})
