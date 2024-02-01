

test_that("add_pred_draws_car1() returns an error for incorrect 'type'", {
  inputs <- load_test_models()
  expect_error(
    add_pred_draws_car1(inputs$data_ar, inputs$fit_ar, type = "wrong type"),
    regexp = "'type' must be either 'prediction' or 'epred'"
  )
})

test_that("add_pred_draws_car1() returns an error for multivariate model", {
  inputs <- load_test_models()
  expect_error(
    add_pred_draws_car1(inputs$data_car1_missing, inputs$fit_car1_missing),
    regexp = "postprocessing methods do not currently support multivariate models"
  )
})

test_that("add_pred_draws_car1() yields the same predictions as fitted.brmsfit()", {
  inputs <- load_test_models()
  preds <- add_pred_draws_car1(
    input = inputs$data_ar, object = inputs$fit_ar,
    car1 = FALSE, draw_ids = 2000
  )
  fitted_vals <- fitted(
    inputs$fit_ar, incl_autocor = FALSE,
    robust = TRUE, draw_ids = 2000
  ) |>
    tibble::as_tibble()
  expect_equal(preds$.epred, fitted_vals$Estimate)
  # also test that draw_ids = NULL works:
  preds_all <- add_pred_draws_car1(inputs$data, inputs$fit, car1 = FALSE)
  expect_equal(nrow(preds_all), nrow(inputs$data) * 3000)
})

test_that("add_pred_draws_car1() yields the same results as tidybayes::add_epred_draws() for a regular AR(1) fit.", {
  inputs <- load_test_models()
  preds1 <- tidybayes::add_epred_draws(inputs$data_ar, inputs$fit_ar) |>
    compare_preds(.draw)
  preds2 <- add_pred_draws_car1(inputs$data_ar, inputs$fit_ar, draw_ids = 1:2000) |>
    compare_preds(.draw = .index)
  expect_equal(preds1, preds2)
})

test_that("add_pred_draws_car1() joins params correctly for nondistributional models.", {
  inputs <- load_test_models()
  these_ids <- c(452, 1298)
  preds <- add_pred_draws_car1(inputs$data, inputs$fit, draw_ids = these_ids, car1 = FALSE, type = "prediction")
  draws <- as_draws_df(inputs$fit, c("nu", "sigma")) |>
    tibble::as_tibble() |>
    dplyr::filter(.draw %in% these_ids)
  sig1 <- dplyr::select(draws, sigma)
  sig2 <- preds |>
    dplyr::ungroup() |>
    dplyr::distinct(sigma)
  nu1 <- dplyr::select(draws, nu)
  nu2 <- preds |>
    dplyr::ungroup() |>
    dplyr::distinct(nu)
  expect_equal(sig1, sig2)
  expect_equal(nu1, nu2)
})

test_that("add_pred_draws_car1() joins params correctly for test distributional model.", {
  inputs <- load_test_models()
  these_ids <- c(452, 1298)
  preds <- add_pred_draws_car1(inputs$data_ar, inputs$fit_ar, draw_ids = these_ids, type = "prediction")
  draws <- as_draws_df(inputs$fit_ar, c("nu", "ar[1]")) |>
    tibble::as_tibble() |>
    dplyr::filter(.draw %in% these_ids)
  sig1 <- calc_sigma(inputs$fit_ar) |>
    dplyr::filter(.draw %in% these_ids) |>
    dplyr::select(series = group, sigma)
  sig2 <- preds |>
    dplyr::ungroup() |>
    dplyr::distinct(series, sigma)
  nu1 <- draws |>
    dplyr::select(nu)
  nu2 <- preds |>
    dplyr::ungroup() |>
    dplyr::distinct(nu)
  ar1 <- draws |>
    dplyr::select(`ar[1]`)
  ar2 <- preds |>
    dplyr::ungroup() |>
    dplyr::distinct(`ar[1]`)
  expect_equal(sig1, sig2)
  expect_equal(nu1, nu2)
})


test_that(
  "add_pred_draws_car1() fits a CAR(1) model that accounts for the autocorrelation
  structure in an irregularly sampled AR(1).", {
    inputs <- load_test_models()
    preds <- add_pred_draws_car1(inputs$data_car1, inputs$fit_car1, draw_ids = 1:2000) |>
      ggdist::median_qi(.epred) |>
      dplyr::mutate(r = y - .epred)
    full <- tibble::tibble(
      x = seq_len(max(inputs$data_car1$x))
    ) |>
      dplyr::left_join(preds, by = "x")
    arima_car1_r <- arima(full$r, order = c(1, 0, 0))
    arima_car1 <- arima(full$y, order = c(1, 0, 0))
    d1 <- abs(coef(arima_car1_r)[1] - c(ar1 = 0))
    d2 <- abs(coef(arima_car1)[1] - c(ar1 = inputs$phi_car1))
    expect_lt(d1, .1)
    expect_lt(d2, .1)
  })

test_that(
  "add_pred_draws_car1() generates CAR(1) predictions (1).", {
    inputs <- load_test_models()
    these_draws <- c(420, 1021, 1169, 782, 376)
    preds_car1 <- withr::with_seed(
      1249, {
        add_pred_draws_car1(inputs$data_car1, inputs$fit_car1, draw_ids = these_draws, type = "prediction")
      }
    )
    full <- tidyr::crossing(
      .draw = these_draws,
      x = seq_len(max(inputs$data_car1$x))
    ) |>
      dplyr::left_join(preds_car1, by = c(".draw", "x")) |>
      dplyr::group_by(.draw) |>
      dplyr::summarize(
        ar1 = unique(na.omit(`ar[1]`)),
        armod = list(arima(.prediction, order = c(1, 0, 0))),
        ar1_est = purrr::map(armod, ~ coef(.x)[1])
      ) |>
      tidyr::unnest(ar1_est)
    estimates <- full$ar1_est
    names(estimates) <- NULL
    expect_equal(full$ar1, estimates, tolerance = .1)
  })

test_that("add_pred_draws_car1() handles a missing response variable in the input (intentionally or not).", {
  inputs <- load_test_models()
  inputs_mod <- inputs$data_ar[, c("date", "series", "d_x")]
  expect_message(
    add_pred_draws_car1(inputs_mod, inputs$fit_ar, draw_ids = 1:2000),
    regexp = "not found in input. Setting car1 = FALSE."
  )
})

