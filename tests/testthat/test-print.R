test_that("print.nowcast_result produces output", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)
  out <- capture.output(print(result), type = "message")
  expect_true(any(grepl("Nowcast", out)))
  expect_true(any(grepl("Bridge Equation", out)))
})

test_that("summary.nowcast_result shows coefficients", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)
  out <- capture.output(summary(result), type = "message")
  expect_true(any(grepl("Coefficients", out)))
})

test_that("print.nc_dataset produces output", {
  target <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    value = rnorm(4)
  )
  ind <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = rnorm(12)
  )
  ds <- nc_align(target, retail = ind)
  out <- capture.output(print(ds), type = "message")
  expect_true(any(grepl("Aligned Nowcasting Dataset", out)))
  expect_true(any(grepl("retail", out)))
})

test_that("print.nowcast_backtest produces output", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 15)
  out <- capture.output(print(bt), type = "message")
  expect_true(any(grepl("Backtest", out)))
  expect_true(any(grepl("RMSE", out)))
})

test_that("summary.nowcast_backtest shows error distribution", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 15)
  out <- capture.output(summary(bt), type = "message")
  expect_true(any(grepl("Error Distribution", out)))
})

test_that("print returns object invisibly", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1, data = d)
  out <- capture.output(ret <- print(result), type = "message")
  expect_identical(ret, result)
})
