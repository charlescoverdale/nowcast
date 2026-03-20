test_that("detect_frequency works for monthly dates", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  expect_equal(detect_frequency(dates), "monthly")
})

test_that("detect_frequency works for quarterly dates", {

  dates <- seq(as.Date("2020-01-01"), by = "quarter", length.out = 8)
  expect_equal(detect_frequency(dates), "quarterly")
})

test_that("detect_frequency works for daily dates", {
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 30)
  expect_equal(detect_frequency(dates), "daily")
})

test_that("detect_frequency works for annual dates", {
  dates <- as.Date(paste0(2010:2020, "-01-01"))
  expect_equal(detect_frequency(dates), "annual")
})

test_that("detect_frequency handles single date", {
  expect_equal(detect_frequency(as.Date("2020-01-01")), "unknown")
})

test_that("to_quarter_date assigns correct quarters", {
  dates <- as.Date(c("2020-01-15", "2020-04-20", "2020-07-01", "2020-10-31"))
  expected <- as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"))
  expect_equal(to_quarter_date(dates), expected)
})

test_that("to_quarter_date handles all months correctly", {
  dates <- as.Date(paste0("2020-", sprintf("%02d", 1:12), "-01"))
  quarters <- to_quarter_date(dates)
  expect_equal(quarters[1:3], rep(as.Date("2020-01-01"), 3))
  expect_equal(quarters[4:6], rep(as.Date("2020-04-01"), 3))
  expect_equal(quarters[7:9], rep(as.Date("2020-07-01"), 3))
  expect_equal(quarters[10:12], rep(as.Date("2020-10-01"), 3))
})

test_that("validate_date rejects non-Date input", {
  expect_error(validate_date("2020-01-01"), "must be a.*Date")
  expect_error(validate_date(123), "must be a.*Date")
})

test_that("validate_numeric rejects non-numeric input", {
  expect_error(validate_numeric("abc"), "must be numeric")
  expect_error(validate_numeric(TRUE), "must be numeric")
})

test_that("validate_data_frame rejects non-data.frame input", {
  expect_error(validate_data_frame(list(a = 1)), "must be a data frame")
  expect_error(validate_data_frame(1:5), "must be a data frame")
})

test_that("new_nowcast_result creates correct class", {
  res <- new_nowcast_result(
    nowcast = 1.5, se = 0.3, ci_lower = 0.9, ci_upper = 2.1,
    method = "bridge", target_period = as.Date("2024-01-01")
  )
  expect_s3_class(res, "nowcast_result")
  expect_equal(res$nowcast, 1.5)
  expect_equal(res$method, "bridge")
})

test_that("new_nc_dataset creates correct class", {
  ds <- new_nc_dataset(
    data = data.frame(date = Sys.Date(), target = 1, ind = 2),
    target_col = "target", indicator_cols = "ind",
    target_freq = "quarterly", indicator_freq = "monthly",
    availability = data.frame()
  )
  expect_s3_class(ds, "nc_dataset")
})

test_that("new_nowcast_backtest creates correct class", {
  bt <- new_nowcast_backtest(
    results = data.frame(date = Sys.Date(), nowcast = 1, actual = 1.1, error = -0.1),
    actuals = 1.1, method = "bridge", window_type = "expanding",
    metrics = data.frame(rmse = 0.1, mae = 0.1, bias = -0.1)
  )
  expect_s3_class(bt, "nowcast_backtest")
})
