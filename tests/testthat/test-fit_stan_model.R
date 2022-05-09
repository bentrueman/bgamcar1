
library("brms")
library("readr")
library("posterior")

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

form_car1 <- bf(y ~ ar(time = x))
phi_car1 <- .45

fit_car1 <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test_car1"),
  seed,
  form_car1,
  data_car1,
  prior_ar,
  save_warmup = FALSE,
  chains = 2
)

test_that("function loads the correct model", {
  expect_equal(class(fit), "brmsfit")
  expect_equal(as.character(fit$formula)[1], "y | cens(ycens, y2 = y2) ~ 1")
})

test_that(
  "fit_stan_model() fits a CAR(1) model that recovers the parameters used to generate the data.", {
    expect_equal(summarise_draws(as_draws_df(fit_car1, "ar[1]"))$median, phi_car1, tolerance = 5e-2)
  })

test_that("fit_stan_model() calls rstan correctly.", {
  data <- tibble(y = rnorm(10))
  testmod <- fit_stan_model(
    file = NULL,
    seed = seed,
    bform = bf(y ~ 1),
    bdata = data,
    car1 = FALSE,
    chains = 1
  )
  expect_equal(class(testmod), "brmsfit")
})
