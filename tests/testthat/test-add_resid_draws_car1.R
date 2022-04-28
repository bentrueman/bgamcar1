
library("brms")
library("readr")

seed <- 1

data <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
data_ar <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))
data_car1 <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_car1.csv"))

form_ar <- bf(y ~ ar(time = date, gr = series), sigma ~ series)
prior_ar <- prior(normal(0, 1), class = Intercept)

fit <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test"),
  seed,
  bf(y | cens(ycens, y2 = y2) ~ 1),
  data,
  prior(normal(0, 1), class = Intercept),
  car1 = FALSE,
  save_warmup = FALSE,
  chains = 3
)

fit_ar <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test_ar"),
  seed,
  form_ar,
  data_ar,
  prior_ar,
  save_warmup = FALSE,
  chains = 2
)

test_that("add_resid_draws_car1() yields the same result as tidybayes::add_residual_draws()", {
  r1 <- add_resid_draws_car1(data, fit, y, car1 = FALSE, draw_ids = 1:3000)
  r2 <- suppressWarnings(tidybayes::add_residual_draws(data, fit))
  r3 <- add_resid_draws_car1(data_ar, fit_ar, y, draw_ids = 1:2000)
  r4 <- suppressWarnings(tidybayes::add_residual_draws(data_ar, fit_ar)) %>%
    arrange(.draw, series, date)
  expect_equal(r1$.residual, r2$.residual)
  expect_equal(r3$.residual, r4$.residual)
})
