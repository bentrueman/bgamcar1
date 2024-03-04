
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
  skip_on_ci()
  filepath <- paste0(system.file("extdata", package = "bgamcar1"), "/model")
  data <- tibble::tibble(y = rnorm(10))
  testmod <- fit_stan_model(
    file = filepath,
    seed = 1,
    bform = brms::bf(y ~ 1),
    bdata = data,
    car1 = FALSE,
    chains = 1
  )
  expect_equal(class(testmod), "brmsfit")
  file.remove(filepath)
  file.remove(paste0(filepath, ".rds"))
})

test_that("fit_stan_model() calls cmdstanr correctly.", {
  skip_on_ci() # cmdstanr is suggested only
  data <- tibble::tibble(y = rnorm(10))
  filepath <- paste0(system.file("extdata", package = "bgamcar1"), "/model")
  testmod <- fit_stan_model(
    file = filepath,
    seed = 1,
    bform = brms::bf(y ~ 1),
    bdata = data,
    car1 = FALSE,
    chains = 1,
    backend = "cmdstanr"
  )
  expect_equal(class(testmod), "brmsfit")
  file.remove(paste0(filepath, "-1.csv"))
  file.remove(paste0(filepath, ".rds"))
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
    ),
    regexp = "column d_x not found in data"
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

test_that("fit_stan_model() handles vector and scalar upper bounds on left-censored variables", {
  skip()
  multivariate_formula <- brms::bf(y ~ mi(x)) +
    brms::bf(x | mi() ~ 1) +
    brms::set_rescor(FALSE)
  data <- data.frame(x = rnorm(50))
  data$y <- 5 * data$x + rnorm(50)
  data$x_cens <- ifelse(data$x < -1, "left", "none")
  data$x <- pmax(data$x, -1)
  N_cens <- sum(data$x_cens == "left")
  fit_single_lcl <- fit_stan_model(
    file = tempfile(),
    seed = 125,
    bform = multivariate_formula,
    bdata = data,
    backend = "cmdstanr",
    car1 = FALSE,
    var_xcens = "x",
    cens_ind = "x_cens",
    lcl = -1,
    save_warmup = FALSE,
    lower_bound = -2,
    family = "gaussian"
  )
  lcl_limits <- list(runif(N_cens, -1.5, -0.5))
  fit_multiple_lcl <- fit_stan_model(
    file = tempfile(),
    seed = 125,
    bform = multivariate_formula,
    bdata = data,
    backend = "cmdstanr",
    car1 = FALSE,
    var_xcens = "x",
    cens_ind = "x_cens",
    lcl = lcl_limits,
    save_warmup = FALSE,
    lower_bound = -2,
    family = "gaussian"
  )
  draws_single <- brms::as_draws_df(fit_single_lcl)
  draws_single |>
    select(starts_with("Ycens_")) |>
    dplyr::reframe(across(everything(), range))
  draws_multiple <- brms::as_draws_df(fit_multiple_lcl)
  draws_multiple |>
    select(starts_with("Ycens_")) |>
    dplyr::reframe(across(everything(), range))
  lcl_limits
})

