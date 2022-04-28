
library("brms")
library("dplyr")
library("tidybayes")
library("readr")
library("withr")
library("tidyr")
library("purrr")
library("ggdist")

calc_sigma <- function(x) {
  varnames <- extract_resp(x)
  distributional <- as.character(x$formula)[2] %>%
    str_detect("sigma ~ ")
  if (distributional) {
    groups <- unique(x$data[, varnames$gr_sigma])
  }
  draws <- x %>%
    as_draws_df("sigma", regex = TRUE) %>%
    as_tibble()
  if (distributional) {
    draws <- draws %>%
      pivot_longer(c(starts_with("b_sigma_"), -matches("^b_sigma_Intercept$"))) %>%
      mutate(value = b_sigma_Intercept + value) %>%
      pivot_wider(names_from = name, values_from = value) %>%
      pivot_longer(starts_with("b_sigma_"), values_to = "sigma") %>%
      select(c(name, sigma, starts_with("."))) %>%
      mutate(
        sigma = exp(sigma),
        group = str_remove(name, varnames$gr_sigma) %>%
          str_extract("(?<=b_sigma_).+$") %>%
          str_replace("Intercept", groups[1])
      )
  }
  draws
}

seed <- 1

data <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
data_ar <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))
data_car1 <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_car1.csv"))

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

test_that("add_pred_draws_car1() returns an error for incorrect 'type'", {
  expect_error(add_pred_draws_car1(data_ar, fit_ar, type = "wrong type"))
})

test_that(
  "add_pred_draws_car1() yields the same predictions as fitted.brmsfit()",
  {
    preds <- add_pred_draws_car1(
      input = data_ar, object = fit_ar,
      car1 = FALSE, draw_ids = 2000
    )

    fitted_vals <- fitted(
      fit_ar, incl_autocor = FALSE,
      robust = TRUE, draw_ids = 2000
    ) %>%
      as_tibble()

    expect_equal(preds$.epred, fitted_vals$Estimate)
  }
)

test_that(
  "add_pred_draws_car1() yields the same results as tidybayes::add_epred_draws()
  for a regular AR(1) fit.", {

    compare_preds <- function(x, ...) {
      x %>%
        select(date, series, y, d_x, .row, .epred, ...) %>%
        arrange(.draw) %>%
        ungroup()
    }

    preds1 <- tidybayes::add_epred_draws(data_ar, fit_ar) %>%
      compare_preds(.draw)

    preds2 <- add_pred_draws_car1(data_ar, fit_ar, draw_ids = 1:2000) %>%
      compare_preds(.draw = .index)

    expect_equal(preds1, preds2)
  })

test_that("add_pred_draws_car1() joins params correctly for nondistributional models.", {
  these_ids <- c(452, 1298)
  preds <- add_pred_draws_car1(data, fit, draw_ids = these_ids, car1 = FALSE, type = "prediction")
  draws <- as_draws_df(fit, c("nu", "sigma")) %>%
    as_tibble() %>%
    filter(.draw %in% these_ids)
  sig1 <- select(draws, sigma)
  sig2 <- preds %>%
    ungroup() %>%
    distinct(sigma)
  nu1 <- select(draws, nu)
  nu2 <- preds %>%
    ungroup() %>%
    distinct(nu)
  expect_equal(sig1, sig2)
  expect_equal(nu1, nu2)
})

test_that("add_pred_draws_car1() joins params correctly for test distributional model.", {
  these_ids <- c(452, 1298)
  preds <- add_pred_draws_car1(data_ar, fit_ar, draw_ids = these_ids, type = "prediction")
  draws <- as_draws_df(fit_ar, c("nu", "ar[1]")) %>%
    as_tibble() %>%
    filter(.draw %in% these_ids)
  sig1 <- calc_sigma(fit_ar) %>%
    filter(.draw %in% these_ids) %>%
    select(series = group, sigma)
  sig2 <- preds %>%
    ungroup() %>%
    distinct(series, sigma)
  nu1 <- draws %>%
    select(nu)
  nu2 <- preds %>%
    ungroup() %>%
    distinct(nu)
  ar1 <- draws %>%
    select(`ar[1]`)
  ar2 <- preds %>%
    ungroup() %>%
    distinct(`ar[1]`)
  expect_equal(sig1, sig2)
  expect_equal(nu1, nu2)
})


test_that(
  "add_pred_draws_car1() fits a CAR(1) model that accounts for the autocorrelation
  structure in an irregularly sampled AR(1).", {
    preds <- add_pred_draws_car1(data_car1, fit_car1, draw_ids = 1:2000) %>%
      ggdist::median_qi(.epred) %>%
      mutate(r = y - .epred)
    full <- tibble(
      x = seq_len(max(data_car1$x))
    ) %>%
      left_join(preds, by = "x")
    arima_car1_r <- arima(full$r, order = c(1, 0, 0))
    arima_car1 <- arima(full$y, order = c(1, 0, 0))
    d1 <- abs(coef(arima_car1_r)[1] - c(ar1 = 0))
    d2 <- abs(coef(arima_car1)[1] - c(ar1 = phi_car1))
    expect_lt(d1, .1)
    expect_lt(d2, .1)
  })

test_that(
  "add_pred_draws_car1() generates CAR(1) predictions (1).", {
    these_draws <- c(420, 1021, 1169, 782, 376)
    preds_car1 <- withr::with_seed(
      1249, {
        add_pred_draws_car1(data_car1, fit_car1, draw_ids = these_draws, type = "prediction")
      }
    )
    full <- crossing(
      .draw = these_draws,
      x = seq_len(max(data_car1$x))
    ) %>%
      left_join(preds_car1, by = c(".draw", "x")) %>%
      group_by(.draw) %>%
      summarize(
        ar1 = unique(na.omit(`ar[1]`)),
        armod = list(arima(.prediction, order = c(1, 0, 0))),
        ar1_est = map(armod, ~ coef(.x)[1])
      ) %>%
      unnest(ar1_est)
    estimates <- full$ar1_est
    names(estimates) <- NULL
    expect_equal(full$ar1, estimates, tolerance = .1)
  })

