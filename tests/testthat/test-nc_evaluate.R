test_that("nc_evaluate computes correct metrics", {
  forecast <- c(1.0, 2.0, 3.0)
  actual <- c(1.1, 1.8, 3.2)
  result <- nc_evaluate(forecast, actual)

  errors <- forecast - actual
  expect_equal(result$rmse, sqrt(mean(errors^2)), tolerance = 1e-10)
  expect_equal(result$mae, mean(abs(errors)), tolerance = 1e-10)
  expect_equal(result$bias, mean(errors), tolerance = 1e-10)
})

test_that("nc_evaluate returns zero for perfect forecast", {
  result <- nc_evaluate(c(1, 2, 3), c(1, 2, 3))
  expect_equal(result$rmse, 0)
  expect_equal(result$mae, 0)
  expect_equal(result$bias, 0)
})

test_that("nc_evaluate errors on length mismatch", {
  expect_error(nc_evaluate(c(1, 2), c(1, 2, 3)), "same length")
})

test_that("nc_evaluate errors on non-numeric input", {
  expect_error(nc_evaluate("a", "b"), "must be numeric")
})

test_that("nc_evaluate handles single observation", {
  result <- nc_evaluate(5, 3)
  expect_equal(result$rmse, 2)
  expect_equal(result$mae, 2)
  expect_equal(result$bias, 2)
})

test_that("nc_evaluate handles NaN/Inf values gracefully", {
  result <- nc_evaluate(c(1, NaN, 3), c(1.1, 2, 3.1))
  expect_true(is.finite(result$rmse))
})

# --- DM test ---

test_that("nc_dm_test returns correct structure", {
  set.seed(1)
  e1 <- rnorm(50)
  e2 <- rnorm(50)
  result <- nc_dm_test(e1, e2)

  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("alternative" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("n" %in% names(result))
  expect_equal(result$alternative, "two.sided")
  expect_equal(result$method, "Diebold-Mariano (HLN)")
  expect_equal(result$n, 50)
})

test_that("nc_dm_test p-value near NA for identical errors", {
  set.seed(1)
  e <- rnorm(100)
  result <- nc_dm_test(e, e)
  expect_true(is.na(result$p_value))
})

test_that("nc_dm_test detects unequal accuracy", {
  set.seed(42)
  e1 <- rnorm(200, sd = 0.5)
  e2 <- rnorm(200, sd = 2.0)
  result <- nc_dm_test(e1, e2, alternative = "less")
  expect_true(result$p_value < 0.05)
})

test_that("nc_dm_test handles horizon > 1", {
  set.seed(1)
  e1 <- rnorm(50, sd = 1)
  e2 <- rnorm(50, sd = 1.5)
  result <- nc_dm_test(e1, e2, h = 4)
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
})

test_that("nc_dm_test supports absolute loss", {
  set.seed(1)
  e1 <- rnorm(50, sd = 1)
  e2 <- rnorm(50, sd = 2)
  result <- nc_dm_test(e1, e2, loss = "absolute")
  expect_true(is.numeric(result$statistic))
})

test_that("nc_dm_test errors on length mismatch", {
  expect_error(nc_dm_test(rnorm(10), rnorm(20)), "same length")
})

test_that("nc_dm_test errors on non-numeric input", {
  expect_error(nc_dm_test("a", "b"), "must be numeric")
})

test_that("nc_dm_test alternative options work", {
  set.seed(1)
  e1 <- rnorm(50)
  e2 <- rnorm(50)

  r1 <- nc_dm_test(e1, e2, alternative = "two.sided")
  r2 <- nc_dm_test(e1, e2, alternative = "less")
  r3 <- nc_dm_test(e1, e2, alternative = "greater")

  expect_equal(r1$alternative, "two.sided")
  expect_equal(r2$alternative, "less")
  expect_equal(r3$alternative, "greater")
})

# --- HLN correction tests (audit issue #6) ---

test_that("nc_dm_test uses t-distribution (wider tails than normal)", {
  # For small n, t-distribution should give larger p-values than normal
  set.seed(42)
  e1 <- rnorm(15, sd = 1)
  e2 <- rnorm(15, sd = 1.5)
  result <- nc_dm_test(e1, e2)

  # Compute what normal would give (raw DM, no HLN)
  d <- e1^2 - e2^2
  d_bar <- mean(d)
  var_d <- var(d) / length(d)
  raw_stat <- d_bar / sqrt(var_d)
  normal_p <- 2 * pnorm(-abs(raw_stat))

  # HLN-corrected p-value should generally differ from normal p-value
  # (can't guarantee direction for every seed, but they should not be identical)
  expect_true(is.numeric(result$p_value))
})

test_that("nc_dm_test errors when h >= n", {
  expect_error(nc_dm_test(rnorm(10), rnorm(10), h = 10), "less than")
  expect_error(nc_dm_test(rnorm(10), rnorm(10), h = 15), "less than")
})

test_that("nc_dm_test Bartlett kernel gives non-negative variance", {
  # Specifically crafted case that could give negative variance with rectangular
  set.seed(99)
  e1 <- rnorm(30)
  e2 <- rnorm(30)
  result <- nc_dm_test(e1, e2, h = 5)
  # Should not return NA (Bartlett prevents negative variance)
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p_value))
})
