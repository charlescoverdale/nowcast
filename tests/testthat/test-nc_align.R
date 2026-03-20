test_that("nc_align merges quarterly target with monthly indicator", {
  target <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    value = c(1, 2, 3, 4)
  )
  monthly <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = 1:12
  )
  ds <- nc_align(target, ind1 = monthly)

  expect_s3_class(ds, "nc_dataset")
  expect_equal(ds$target_col, "target")
  expect_equal(ds$indicator_cols, "ind1")
  expect_equal(nrow(ds$data), 4)
  expect_true(all(c("date", "target", "ind1") %in% names(ds$data)))
})

test_that("nc_align aggregates monthly to quarterly via mean", {
  target <- data.frame(
    date = as.Date("2020-01-01"),
    value = 10
  )
  monthly <- data.frame(
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    value = c(3, 6, 9)
  )
  ds <- nc_align(target, ind = monthly)
  expect_equal(ds$data$ind[1], 6)  # mean(3, 6, 9) = 6
})

test_that("nc_align handles multiple indicators", {
  target <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 2),
    value = c(1, 2)
  )
  ind1 <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
    value = rnorm(6)
  )
  ind2 <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
    value = rnorm(6)
  )
  ds <- nc_align(target, retail = ind1, production = ind2)
  expect_equal(length(ds$indicator_cols), 2)
  expect_equal(ds$indicator_cols, c("retail", "production"))
})

test_that("nc_align errors on missing date/value columns", {
  target <- data.frame(x = 1, y = 2)
  ind <- data.frame(date = Sys.Date(), value = 1)
  expect_error(nc_align(target, ind = ind), "date.*value")
})

test_that("nc_align errors with no indicators", {
  target <- data.frame(date = Sys.Date(), value = 1)
  expect_error(nc_align(target), "At least one indicator")
})

test_that("nc_align assigns generic names to unnamed indicators", {
  target <- data.frame(
    date = as.Date("2020-01-01"),
    value = 1
  )
  ind <- data.frame(
    date = as.Date("2020-01-01"),
    value = 2
  )
  ds <- nc_align(target, ind)
  expect_equal(ds$indicator_cols, "indicator_1")
})

test_that("nc_align detects frequencies correctly", {
  target <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 8),
    value = rnorm(8)
  )
  monthly <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = rnorm(24)
  )
  ds <- nc_align(target, ind = monthly)
  expect_equal(ds$target_freq, "quarterly")
})

test_that("nc_align builds availability summary", {
  target <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    value = c(1, 2, 3, 4)
  )
  ind <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = rnorm(12)
  )
  ds <- nc_align(target, ind = ind)
  avail <- ds$availability
  expect_equal(nrow(avail), 2)
  expect_equal(avail$series, c("target", "ind"))
  expect_equal(avail$n_obs, c(4L, 12L))
})

test_that("nc_ragged_edge works on nc_dataset", {
  target <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    value = c(1, 2, NA, NA)
  )
  ind <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 9),
    value = c(rnorm(9))
  )
  ds <- nc_align(target, ind = ind)
  re <- nc_ragged_edge(ds)

  expect_equal(nrow(re), 2)
  expect_true("n_missing" %in% names(re))
  expect_equal(re$n_missing[re$series == "target"], 2L)
})

test_that("nc_ragged_edge works on plain data frame", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01")),
    gdp = c(1, 2, NA),
    retail = c(5, 6, 7)
  )
  re <- nc_ragged_edge(df)
  expect_equal(nrow(re), 2)
  expect_equal(re$n_missing[re$series == "gdp"], 1L)
  expect_equal(re$n_missing[re$series == "retail"], 0L)
})

test_that("nc_ragged_edge errors on data without date column", {
  expect_error(nc_ragged_edge(data.frame(x = 1)), "date")
})
