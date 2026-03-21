test_that("nc_available returns correct structure", {
  avail <- nc_available()
  expect_s3_class(avail, "data.frame")
  expect_true(all(c("method", "description", "available") %in% names(avail)))
  expect_true("bridge" %in% avail$method)
  expect_true(avail$available[avail$method == "bridge"])
})

test_that("nc_compute dispatches to bridge with named formula", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_compute(d, method = "bridge", formula = target ~ ind_1 + ind_2)
  expect_s3_class(result, "nowcast_result")
  expect_equal(result$method, "bridge")
})

test_that("nc_compute dispatches to bridge with unnamed formula", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_compute(d, method = "bridge", target ~ ind_1 + ind_2)
  expect_s3_class(result, "nowcast_result")
  expect_equal(result$method, "bridge")
})

test_that("nc_compute errors without formula", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  expect_error(nc_compute(d, method = "bridge"), "formula")
})
