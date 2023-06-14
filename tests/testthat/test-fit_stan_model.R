
test_that("fit_stan_model() loads the correct model and recovers params", {
  inputs <- load_test_models()
  expect_equal(class(inputs$fit), "brmsfit")
  expect_equal(as.character(inputs$fit$formula)[1], "y | cens(ycens, y2 = y2) ~ 1")
  # recovers params:
  expect_equal(
    posterior::summarise_draws(brms::as_draws_df(inputs$fit_car1, "ar[1]"))$median,
    inputs$phi_car1, tolerance = 5e-2
  )
})

test_that("fit_stan_model() calls rstan correctly.", {
  # this test passes locally, but fails on GHA with the following error:
  # "Boost not found; call install.packages('BH')"
  # skip_on_ci()
  # update: this test no longer passes locally,
  # due to an undiagnosed issue with rstan
  skip()
  data <- tibble::tibble(y = rnorm(10))
  testmod <- fit_stan_model(
    file = NULL,
    seed = 1,
    bform = brms::bf(y ~ 1),
    bdata = data,
    car1 = FALSE,
    chains = 1
  )
  expect_equal(class(testmod), "brmsfit")
})

test_that("fit_stan_model() calls cmdstanr correctly.", {
  skip_on_ci() # cmdstanr is suggested only
  data <- tibble::tibble(y = rnorm(10))
  testmod <- fit_stan_model(
    file = paste0(system.file("extdata", package = "bgamcar1"), "/model"),
    seed = 1,
    bform = brms::bf(y ~ 1),
    bdata = data,
    car1 = FALSE,
    chains = 1,
    backend = "cmdstanr",
    overwrite = TRUE
  )
  expect_equal(class(testmod), "brmsfit")
})

test_that("fit_stan_model() returns an error if d_x argument is missing", {

  seed <- 1
  inputs <- load_test_models()
  s <- inputs$data_car1$d_x
  inputs$data_car1$d_x <- NULL

  # d_x missing:
  expect_error(
    suppressWarnings(
      fit_stan_model(
        paste0(system.file("extdata", package = "bgamcar1"), "/test_car1"),
        seed,
        inputs$form_car1,
        inputs$data_car1,
        inputs$prior_ar,
        save_warmup = FALSE,
        chains = 2
      )
    )
  )

  # d_x as an argument:
  fit_car1_d_x <- fit_stan_model(
    paste0(system.file("extdata", package = "bgamcar1"), "/test_car1"),
    seed,
    inputs$form_car1,
    inputs$data_car1,
    inputs$prior_ar,
    save_warmup = FALSE,
    chains = 2,
    d_x = s
  )

  expect_equal(
    posterior::summarise_draws(brms::as_draws_df(fit_car1_d_x, "ar[1]"))$median,
    inputs$phi_car1, tolerance = 5e-2
  )
})
