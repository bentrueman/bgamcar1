
library("tidyr")
library("dplyr")
library("withr")

phi <- .7

car1_input <- crossing(.index = 1:2, location = letters[1:2], rep = 1:200) %>%
  mutate(
    `ar[1]` = phi,
    nu = 1e3,
    sigma = 1,
    .epred = 0
  )

car1_input_test <- car1_input

test_that("add_car1_err() generates a regular AR(1) process.", {

  data_car1 <- with_seed(32567, {
    car1_input %>%
      mutate(d_x = 1) %>%
      add_car1_err(gr_vars = c(".index", "location")) %>%
      filter(.index == 1, location == "a")
  })

  fit <- arima(data_car1$.prediction, order = c(1, 0, 0))

  expect_equal(as.numeric(fit$coef[1]), phi, tolerance = .1)

})

test_that("add_car1_err() generates an irregular AR(1) process.", {

  sub <- with_seed(219, {
    car1_input %>%
      group_by(.index, location) %>%
      slice_sample(prop = .6) %>%
      ungroup() %>%
      arrange(.index, location, rep) %>%
      mutate(d_x = replace_na(rep - lag(rep), 0))
  })

  data_car1 <- withr::with_seed(32567, {
    sub %>%
      add_car1_err(gr_vars = c(".index", "location")) %>%
      filter(.index == 1, location == "a")
  })

  car1_input %>%
    left_join(
      data_car1,
      c(".index", "location", "rep", "ar[1]", "nu", "sigma", ".epred")
    )

  fit <- arima(data_car1$.prediction, order = c(1, 0, 0))

  expect_equal(as.numeric(fit$coef[1]), phi, tolerance = .1)

})

test_that("add_car1_err() doesn't alter input.", {
  expect_equal(car1_input_test, car1_input)
})
