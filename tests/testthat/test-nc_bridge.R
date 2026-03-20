test_that("nc_bridge returns nowcast_result", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)

  expect_s3_class(result, "nowcast_result")
  expect_equal(result$method, "bridge")
  expect_true(is.numeric(result$nowcast))
  expect_true(is.numeric(result$se))
  expect_true(is.numeric(result$ci_lower))
  expect_true(is.numeric(result$ci_upper))
  expect_true(result$ci_lower < result$nowcast)
  expect_true(result$ci_upper > result$nowcast)
})

test_that("nc_bridge recovers known DGP coefficients", {
  d <- make_bridge_data(
    n_quarters = 500, n_indicators = 2,
    beta = c(0.5, 0.3), intercept = 0.2, seed = 42
  )
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)

  coefs <- result$details$coefficients
  expect_equal(coefs["(Intercept)", "estimate"], 0.2, tolerance = 0.15)
  expect_equal(coefs["ind_1", "estimate"], 0.5, tolerance = 0.1)
  expect_equal(coefs["ind_2", "estimate"], 0.3, tolerance = 0.1)
})

test_that("nc_bridge R-squared is reasonable for well-specified model", {
  d <- make_bridge_data(n_quarters = 200, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)
  expect_true(result$details$r_squared > 0.5)
})

test_that("nc_bridge fitted_values has correct structure", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)

  fv <- result$fitted_values
  expect_true(all(c("date", "actual", "fitted", "residual") %in% names(fv)))
  expect_equal(nrow(fv), result$details$n_obs)
  expect_equal(fv$residual, fv$actual - fv$fitted, tolerance = 1e-10)
})

test_that("nc_bridge works with nc_dataset input", {
  target <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "quarter", length.out = 20),
    value = rnorm(20)
  )
  ind <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 60),
    value = rnorm(60)
  )
  ds <- nc_align(target, ind1 = ind)
  result <- nc_bridge(target ~ ind1, data = ds)
  expect_s3_class(result, "nowcast_result")
})

test_that("nc_bridge uses newdata when provided", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  newdata <- data.frame(ind_1 = 0.5, ind_2 = 0.3)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d, newdata = newdata)
  expect_s3_class(result, "nowcast_result")
})

test_that("nc_bridge errors on non-formula", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  expect_error(nc_bridge("target ~ ind_1", data = d), "formula")
})

test_that("nc_bridge errors on missing target variable", {
  d <- data.frame(date = Sys.Date(), x = 1)
  expect_error(nc_bridge(gdp ~ x, data = d), "not found")
})

test_that("nc_bridge handles custom alpha", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  r90 <- nc_bridge(target ~ ind_1, data = d, alpha = 0.10)
  r99 <- nc_bridge(target ~ ind_1, data = d, alpha = 0.01)

  # 99% CI should be wider than 90% CI
  width_90 <- r90$ci_upper - r90$ci_lower
  width_99 <- r99$ci_upper - r99$ci_lower
  expect_true(width_99 > width_90)
})

test_that("nc_bridge handles missing values in data", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  d$ind_1[5] <- NA
  d$ind_2[10] <- NA
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)
  expect_s3_class(result, "nowcast_result")
  expect_equal(result$details$n_obs, 18)
})

test_that("nc_bridge stores model for predict method", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)
  expect_true(!is.null(result$model))
  expect_s3_class(result$model, "lm")
})
