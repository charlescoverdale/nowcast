test_that("nc_aggregate converts monthly to quarterly", {
  monthly <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = 1:12
  )
  quarterly <- nc_aggregate(monthly, to = "quarterly")
  expect_equal(nrow(quarterly), 4)
  expect_equal(quarterly$value[1], mean(1:3))
  expect_equal(quarterly$value[4], mean(10:12))
})

test_that("nc_aggregate converts monthly to annual", {
  monthly <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = 1:12
  )
  annual <- nc_aggregate(monthly, to = "annual")
  expect_equal(nrow(annual), 1)
  expect_equal(annual$value[1], mean(1:12))
})

test_that("nc_aggregate respects custom function", {
  monthly <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 3),
    value = c(10, 20, 30)
  )
  quarterly <- nc_aggregate(monthly, to = "quarterly", fun = sum)
  expect_equal(quarterly$value[1], 60)
})

test_that("nc_aggregate errors without required columns", {
  expect_error(nc_aggregate(data.frame(x = 1), to = "quarterly"), "date.*value")
})

test_that("nc_transform diff works on data frame", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 5),
    value = c(100, 102, 105, 103, 108)
  )
  result <- nc_transform(df, method = "diff")
  expect_equal(nrow(result), 4)
  expect_equal(result$value[1], 2)
  expect_equal(result$value[2], 3)
})

test_that("nc_transform log_diff computes growth rates", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 3),
    value = c(100, 110, 121)
  )
  result <- nc_transform(df, method = "log_diff")
  expect_equal(nrow(result), 2)
  expect_equal(result$value[1], log(110 / 100) * 100, tolerance = 1e-6)
})

test_that("nc_transform standardize has zero mean and unit variance", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 20),
    value = rnorm(20, mean = 50, sd = 10)
  )
  result <- nc_transform(df, method = "standardize")
  expect_equal(nrow(result), 20)
  expect_equal(mean(result$value), 0, tolerance = 1e-10)
  expect_equal(sd(result$value), 1, tolerance = 1e-10)
})

test_that("nc_transform works on numeric vector", {
  x <- c(100, 102, 105)
  result <- nc_transform(x, method = "diff")
  expect_equal(result, c(2, 3))
})

test_that("nc_transform errors without required columns", {
  expect_error(nc_transform(data.frame(x = 1), method = "diff"), "date.*value")
})

test_that("nc_transform preserves date ordering", {
  df <- data.frame(
    date = as.Date(c("2020-03-01", "2020-01-01", "2020-02-01")),
    value = c(300, 100, 200)
  )
  result <- nc_transform(df, method = "diff")
  expect_equal(result$date, as.Date(c("2020-02-01", "2020-03-01")))
  expect_equal(result$value, c(100, 100))
})

# --- Edge case tests (audit issue #11) ---

test_that("nc_transform log_diff errors on non-positive values", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 3),
    value = c(100, -5, 50)
  )
  expect_error(nc_transform(df, method = "log_diff"), "strictly positive")
})

test_that("nc_transform log_diff errors on zero values", {
  expect_error(nc_transform(c(100, 0, 50), method = "log_diff"), "strictly positive")
})

test_that("nc_transform standardize errors on constant values", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 5),
    value = rep(5, 5)
  )
  expect_error(nc_transform(df, method = "standardize"), "zero")
})

# --- Audit phase 2: nc_aggregate with sum and NAs ---

test_that("nc_aggregate with sum drops NAs silently", {
  monthly <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 3),
    value = c(10, NA, 30)
  )
  quarterly <- nc_aggregate(monthly, to = "quarterly", fun = sum)
  # sum(10, NA, 30, na.rm = TRUE) = 40, not 60
  expect_equal(quarterly$value[1], 40)
})
