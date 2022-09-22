
test_that("impute_censored() replaces censored values with predictions.", {
  x <- tibble::tibble(
    y = rnorm(100),
    .prediction = 10 * y,
    cens = sample(c("left", "right"), length(y), replace = TRUE)
  )
  input <- dplyr::select(x, y, cens)
  imp <- impute_censored(x, input, "y", "cens")
  expect_equal(imp$y, x$.prediction)

})
