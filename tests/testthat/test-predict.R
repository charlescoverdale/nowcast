test_that("predict.nowcast_result generates predictions", {
  d <- make_bridge_data(n_quarters = 20, seed = 42)
  result <- nc_bridge(target ~ ind_1 + ind_2, data = d)

  newdata <- data.frame(ind_1 = c(0.5, -0.3), ind_2 = c(0.2, 0.1))
  preds <- predict(result, newdata = newdata)
  expect_length(preds, 2)
  expect_true(is.numeric(preds))
})

test_that("predict.nowcast_result errors without model", {
  res <- new_nowcast_result(
    nowcast = 1, method = "bridge",
    target_period = Sys.Date(), model = NULL
  )
  expect_error(predict(res, newdata = data.frame(x = 1)), "No fitted model")
})
