test_that("Config processing works", {
  expect_true(is.list(config(formula = "Status ~ Value")))
})
