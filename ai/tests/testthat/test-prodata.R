test_that("Data processing works", {
  model_data <- data.frame(a = c(1,2,3,4,5,6), b = c(1,2,3,4,5,6), s = c(1,2,3,4,5,6))
  expect_true(is.list(prodata(data = model_data, status_colname = "s")))
})
