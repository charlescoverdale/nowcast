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
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d, ar_order = 0)

  coefs <- result$details$coefficients
  expect_equal(coefs["(Intercept)", "estimate"], 0.2, tolerance = 0.15)
  expect_equal(coefs["ind_1", "estimate"], 0.5, tolerance = 0.1)
  expect_equal(coefs["ind_2", "estimate"], 0.3, tolerance = 0.1)
})

test_that("nc_bridge R-squared is reasonable for well-specified model", {
  d <- make_bridge_data(n_quarters = 200, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d, ar_order = 0)
  expect_true(result$details$r_squared > 0.5)
})

test_that("nc_bridge fitted_values has correct structure", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d, ar_order = 0)

  fv <- result$fitted_values
  expect_true(all(c("date", "actual", "fitted", "residual") %in% names(fv)))
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
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d, newdata = newdata,
                      ar_order = 0)
  expect_s3_class(result, "nowcast_result")
})

test_that("nc_bridge errors on non-formula", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  expect_error(nc_bridge("target ~ ind_1", data = d), "formula")
})

test_that("nc_bridge errors on one-sided formula", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  expect_error(nc_bridge(~ ind_1, data = d), "two-sided")
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
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d, ar_order = 0)
  expect_s3_class(result, "nowcast_result")
  expect_equal(result$details$n_obs, 18)
})

test_that("nc_bridge stores model for predict method", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)
  expect_true(!is.null(result$model))
  expect_s3_class(result$model, "lm")
})

# --- Prediction interval tests (audit issue #2) ---

test_that("nc_bridge prediction intervals use proper t-distribution width", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d, ar_order = 0)

  # CI should be wider than just sigma * z (because of estimation uncertainty)
  sigma <- stats::sigma(result$model)
  z_width <- 2 * qnorm(0.975) * sigma
  actual_width <- result$ci_upper - result$ci_lower
  expect_true(actual_width > z_width)
})

test_that("nc_bridge 95% CI achieves ~95% coverage on synthetic DGP", {
  # Generate many datasets and check coverage
  set.seed(123)
  n_sims <- 200
  covers <- logical(n_sims)
  for (sim in seq_len(n_sims)) {
    d <- make_bridge_data(n_quarters = 50, beta = c(0.5, 0.3),
                          intercept = 0.2, seed = sim)
    # Hold out last row as true value
    true_val <- d$target[50]
    newdata <- d[50, c("ind_1", "ind_2"), drop = FALSE]
    train <- d[1:49, ]

    result <- nc_bridge(target ~ ind_1 + ind_2, data = train,
                        newdata = newdata, ar_order = 0, alpha = 0.05)
    covers[sim] <- (true_val >= result$ci_lower) & (true_val <= result$ci_upper)
  }
  coverage <- mean(covers)
  # Should be around 0.95 — allow 0.85 to 1.00 for simulation noise
  expect_true(coverage >= 0.85)
  expect_true(coverage <= 1.00)
})

# --- AR order tests (audit issue #1) ---

test_that("nc_bridge includes AR terms when ar_order > 0", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  result <- nc_bridge(target ~ ind_1, data = d, ar_order = 1)

  expect_equal(result$details$ar_order, 1)
  expect_true(".ar_1" %in% rownames(result$details$coefficients))
})

test_that("nc_bridge ar_order = 0 gives static model", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  result <- nc_bridge(target ~ ind_1, data = d, ar_order = 0)

  expect_equal(result$details$ar_order, 0)
  expect_false(".ar_1" %in% rownames(result$details$coefficients))
})

test_that("nc_bridge ar_order = 2 includes two lags", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  result <- nc_bridge(target ~ ind_1, data = d, ar_order = 2)

  expect_true(".ar_1" %in% rownames(result$details$coefficients))
  expect_true(".ar_2" %in% rownames(result$details$coefficients))
})

# --- AIC/BIC tests (audit issue #7) ---

test_that("nc_bridge returns AIC and BIC", {
  d <- make_bridge_data(n_quarters = 30, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)
  expect_true(!is.null(result$details$aic))
  expect_true(!is.null(result$details$bic))
  expect_true(is.numeric(result$details$aic))
  expect_true(is.numeric(result$details$bic))
})

# --- Edge case tests (audit phase 3) ---

test_that("nc_bridge errors on zero-variance predictor", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  d$ind_1 <- 5  # constant
  expect_error(nc_bridge(target ~ ind_1, data = d), "zero variance")
})

test_that("nc_bridge warns on multicollinear predictors", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  d$ind_3 <- d$ind_1 * 2  # perfect collinearity
  expect_warning(
    nc_bridge(target ~ ind_1 + ind_2 + ind_3, data = d, ar_order = 0),
    "NA"
  )
})
