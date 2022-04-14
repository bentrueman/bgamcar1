
library("brms")
library("readr")

seed <- 1

data <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
data_ar <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))

fit <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test"),
  seed,
  bf(y | cens(ycens) ~ 1),
  data,
  prior(normal(0, 1), class = Intercept),
  car1 = FALSE,
  save_warmup = FALSE,
  chains = 3
)

form_ar <- bf(y ~ ar(time = date, gr = series), sigma ~ series)
prior_ar <- prior(normal(0, 1), class = Intercept)

fit_ar <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test_ar"),
  seed,
  form_ar,
  data_ar,
  prior_ar,
  save_warmup = FALSE,
  chains = 2
)

loo1a <- loo(fit_ar)
loo2a <- suppressWarnings(loo_cv(data_ar, fit_ar, censoring = FALSE, draw_ids = 1:2000))

loo1b <- loo(fit)
loo2b <- suppressWarnings(loo_cv(data, fit, draw_ids = 1:3000, car1 = FALSE))

test_that("brms::loo() yields the same results as loo_cv()", {
  expect_equal(loo1a$estimate, loo2a$estimate)
  expect_equal(loo1b$estimate, loo2b$estimate)
})
