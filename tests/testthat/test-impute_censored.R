
library("dplyr")

test_that("impute_censored() replaces censored values with predictions.", {
  x <- tibble(
    x = 1:10,
    y = rnorm(10),
    .prediction = rnorm(10),
    cens = "left"
  )
  input <- select(x, x, y, cens)
  imp <- impute_censored(x, input, "y", "cens")
  expect_equal(imp$y, x$.prediction)

})
