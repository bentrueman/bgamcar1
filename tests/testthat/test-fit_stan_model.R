
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
        # backend = "cmdstanr"
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
    # backend = "cmdstanr"
  )

  expect_equal(
    posterior::summarise_draws(brms::as_draws_df(fit_car1_d_x, "ar[1]"))$median,
    inputs$phi_car1, tolerance = 5e-2
  )
})

test_that("fit_stan_model() handles left-censored variables without censored values", {
  data <- data.frame(x = rnorm(50))
  data$y <- 5 * data$x + rnorm(50)
  data$x_cens <- "none"
  model <- fit_stan_model(
    file = tempfile(),
    seed = 125,
    bform = y ~ x,
    bdata = data,
    # backend = "cmdstanr",
    car1 = FALSE,
    var_xcens = "x",
    cens_ind = "x_cens",
    lcl = -1e3,
    save_warmup = FALSE,
    lower_bound = -2e3,
    family = "gaussian"
  )
  expect_true(class(model) == "brmsfit")
})

test_that("fit_stan_model() handles vector and scalar upper and lower bounds on left-censored variables", {
  multivariate_formula <- brms::bf(y ~ mi(x)) +
    brms::bf(x | mi() ~ 1) +
    brms::set_rescor(FALSE)
  data <- data.frame(x = rnorm(50))
  data$y <- 5 * data$x + rnorm(50)
  data$x_cens <- ifelse(data$x < -1, "left", "none")
  data$x <- pmax(data$x, -1)
  N_cens <- sum(data$x_cens == "left")

  # --- scalar lcl, scalar lower bound ---
  fit_single_lcl <- fit_stan_model(
    file = tempfile(),
    seed = 125,
    bform = multivariate_formula,
    bdata = data,
    car1 = FALSE,
    var_xcens = "x",
    cens_ind = "x_cens",
    lcl = -1,
    save_warmup = FALSE,
    lower_bound = -2,
    family = "gaussian"
  )

  # --- vector lcl, vector lower bound ---
  lcl_limits <- list(runif(N_cens, -1.5, -0.5))
  lb_limits  <- list(runif(N_cens, -3, -2))
  fit_multiple_lcl <- fit_stan_model(
    file = tempfile(),
    seed = 125,
    bform = multivariate_formula,
    bdata = data,
    car1 = FALSE,
    var_xcens = "x",
    cens_ind = "x_cens",
    lcl = lcl_limits,
    save_warmup = FALSE,
    lower_bound = lb_limits,
    family = "gaussian"
  )

  # helper: for each censored parameter, apply a summary stat across all iterations

  get_draws_summary <- function(fit, fn) {
    withCallingHandlers(
      brms::as_draws_df(fit) |>
        dplyr::select(tidyselect::starts_with("Ycens_")) |>
        dplyr::reframe(dplyr::across(tidyselect::everything(), fn)) |>
        t() |>
        as.numeric(),
      warning = function(w) {
        if (grepl("Dropping 'draws_df' class", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    )
  }

  # scalar lcl: all draws must be below the scalar upper bound (-1)
  expect_true(all(get_draws_summary(fit_single_lcl, max) < -1))
  # scalar lower_bound: all draws must be above the scalar lower bound (-2)
  expect_true(all(get_draws_summary(fit_single_lcl, min) > -2))

  # vector lcl: each parameter's max draw must be below its individual upper bound
  expect_true(all(get_draws_summary(fit_multiple_lcl, max) < lcl_limits[[1]]))
  # vector lower_bound: each parameter's min draw must be above its individual lower bound
  expect_true(all(get_draws_summary(fit_multiple_lcl, min) > lb_limits[[1]]))
})

test_that("fit_stan_model() handles vector and scalar upper and lower bounds on left-censored variables in univariate models", {
  univariate_formula <- brms::bf(x | mi() ~ 1)
  data <- data.frame(x = rnorm(50))
  data$x_cens <- ifelse(data$x < -1, "left", "none")
  data$x <- pmax(data$x, -1)
  N_cens <- sum(data$x_cens == "left")

  # --- scalar lcl, scalar lower bound ---
  fit_single_lcl <- fit_stan_model(
    file = tempfile(),
    seed = 125,
    bform = univariate_formula,
    bdata = data,
    car1 = FALSE,
    var_xcens = "x",
    cens_ind = "x_cens",
    lcl = -1,
    save_warmup = FALSE,
    lower_bound = -2,
    family = "gaussian"
  )

  # --- vector lcl, vector lower bound ---
  lcl_limits <- list(runif(N_cens, -1.5, -0.5))
  lb_limits  <- list(runif(N_cens, -3,   -2))
  fit_multiple_lcl <- fit_stan_model(
    file = tempfile(),
    seed = 125,
    bform = univariate_formula,
    bdata = data,
    car1 = FALSE,
    var_xcens = "x",
    cens_ind = "x_cens",
    lcl = lcl_limits,
    save_warmup = FALSE,
    lower_bound = lb_limits,
    family = "gaussian"
  )

  # helper: for each censored parameter, apply a summary stat across all iterations

  get_draws_summary <- function(fit, fn) {
    withCallingHandlers(
      brms::as_draws_df(fit) |>
        dplyr::select(tidyselect::starts_with("Ycens_")) |>
        dplyr::reframe(dplyr::across(tidyselect::everything(), fn)) |>
        t() |>
        as.numeric(),
      warning = function(w) {
        if (grepl("Dropping 'draws_df' class", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    )
  }

  # scalar lcl: all draws must be below the scalar upper bound (-1)
  expect_true(all(get_draws_summary(fit_single_lcl, max) < -1))
  # scalar lower_bound: all draws must be above the scalar lower bound (-2)
  expect_true(all(get_draws_summary(fit_single_lcl, min) > -2))

  # vector lcl: each parameter's max draw must be below its individual upper bound
  expect_true(all(get_draws_summary(fit_multiple_lcl, max) < lcl_limits[[1]]))
  # vector lower_bound: each parameter's min draw must be above its individual lower bound
  expect_true(all(get_draws_summary(fit_multiple_lcl, min) > lb_limits[[1]]))
})

test_that("fit_stan_model() handles missings", {
  skip() # currently an empty test
  data <- data.frame(y = c(rnorm(10), NA), time = 1:11, d_x = c(0, rep(1, 10)))
  # only concerned about the error, not the warnings (remove suppressWarnings() to debug)
  fit <- suppressWarnings(fit_stan_model(
    file = tempfile(),
    seed = 125,
    # bform = y | mi() ~ ar(time), # this works
    bform = y ~ ar(time),
    bdata = data,
    backend = "cmdstanr",
    family = "gaussian"
  ))
})

