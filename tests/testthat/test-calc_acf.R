
library("dplyr")
library("readr")
library("withr")
library("tidyr")

data_acf1 <- tibble(
  .draw =  1,
  .residual = c(1:9, -1e2),
  location = "a"
)

data_acf_test1 <- data_acf1

test_that("calc_acf() yields the expected correlation, with and without filtering", {

  c1 <- calc_acf(data_acf1, gr_vars = c(".draw", "location"))
  c2 <- calc_acf(data_acf1, .residual > 0, gr_vars = c(".draw", "location"))

  expect_equal(c1$cor, .4)
  expect_equal(c2$cor, 1)

})

test_that("calc_acf() doesn't alter input", {
  expect_equal(data_acf1, data_acf_test1)
})

data_acf2 <- crossing(
  location = letters[1:2],
  .residual = as.numeric(1:10)
) %>%
  mutate(
    .residual = if_else(location == "b", .residual - 100, .residual),
    .draw = 1
  )

data_acf_test2 <- data_acf2

test_that("calc_acf() calculates lagged residuals correctly.", {

  expect_equal(calc_acf(data_acf2, gr_vars = c(".draw", "location"))$cor, 1)

})

test_that("calc_acf() doesn't alter input", {
  expect_equal(data_acf1, data_acf_test1)
})


data_acf3 <- crossing(
  series = letters[1:3],
  .draw = 1:3,
  .residual = as.numeric(1:10)
) %>%
  mutate(
    censoring = withr::with_seed(1245, {
      sample(c(-1, 1), length(series), replace = TRUE)
    }
    ),
    .residual = if_else(censoring == -1, -10, .residual)
  )

test_that("Filtering works for calc_acf()", {
  acf3 <- calc_acf(
    data_acf3,
    censoring == 1 & cens_lagged == 1,
    cen_var = "censoring"
  )
  expect_error(calc_acf(data_acf3, cens_lagged = 1))
  expect_equal(acf3$cor, rep(1, 3))
})
