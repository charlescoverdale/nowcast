test_that("plot.nowcast_result runs without error", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)
  expect_no_error(plot(result))
})

test_that("plot.nowcast_backtest runs without error", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 15)
  expect_no_error(plot(bt))
})

test_that("plot.nowcast_result returns object invisibly", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1, data = d)
  ret <- plot(result)
  expect_identical(ret, result)
})

test_that("plot.nowcast_backtest returns object invisibly", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  bt <- nc_backtest(target ~ ind_1, data = d, start = 15)
  ret <- plot(bt)
  expect_identical(ret, bt)
})
