
library("withr")
library("dplyr")
library("tibble")

test_that("filter_car1() works the same as stats::filter() for regularly spaced data.", {
  x <- rnorm(100)
  d_x = rep(1, length(x))
  f1 <- filter_car1(x, .5, s = d_x)
  f2 <- stats::filter(x, filter = .5, method = "recursive")
  expect_equal(as.numeric(f1), as.numeric(f2))
})

test_that("filter_car1() returns an error for NAs.", {
  x <- rnorm(100)
  x[50] <- NA
  d_x = rep(1, length(x))
  expect_error(filter_car1(x, .5, s = d_x))
})

test_that("filter_car1() generates an irregularly-sampled AR(1) process.", {

  phi <- .8

  data <- with_seed(356, {
    tibble(
      x = rnorm(200)
    ) %>%
      tibble::rowid_to_column()
  })

  sub <- with_seed(341, {slice_sample(data, prop = .7)}) %>%
    arrange(rowid) %>%
    mutate(
      d_x = replace_na(rowid - lag(rowid), 0),
      car1 = filter_car1(x, phi, s = d_x)
    )

  fit <- data %>%
    left_join(sub, by = c("rowid", "x")) %>%
    with(arima(car1, order = c(1, 0, 0)))

  expect_lt(abs(phi - as.numeric(fit$coef[1])), .1)
})
