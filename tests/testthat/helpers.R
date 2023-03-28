
# load models for testing:

load_test_models <- function() {

  seed <- 1

  data <- readr::read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
  data_ar <- readr::read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))
  data_car1 <- readr::read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_car1.csv"))

  fit <- fit_stan_model(
    paste0(system.file("extdata", package = "bgamcar1"), "/test"),
    seed,
    brms::bf(y | cens(ycens, y2 = y2) ~ 1),
    data,
    brms::prior(normal(0, 1), class = Intercept),
    car1 = FALSE,
    save_warmup = FALSE,
    chains = 3
  )

  form_ar <- brms::bf(y ~ ar(time = date, gr = series), sigma ~ series)
  prior_ar <- brms::prior(normal(0, 1), class = Intercept)

  fit_ar <- fit_stan_model(
    paste0(system.file("extdata", package = "bgamcar1"), "/test_ar"),
    seed,
    form_ar,
    data_ar,
    prior_ar,
    save_warmup = FALSE,
    chains = 2
  )

  form_car1 <- brms::bf(y ~ ar(time = x))
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

  list(
    # data:
    data = data,
    data_ar = data_ar,
    data_car1 = data_car1,
    # models:
    fit = fit,
    fit_ar = fit_ar,
    fit_car1 = fit_car1,
    # other inputs:
    form_ar = form_ar,
    form_car1 = form_car1,
    prior_ar = prior_ar,
    phi_car1 = phi_car1
  )

}

load_test_gams <- function() {

  seed <- 1

  # for local_slope.R

  data_gam <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_gam.csv"))
  data_gam2 <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_gam2.csv"))

  fit_gam <- fit_stan_model(
    paste0(system.file("extdata", package = "bgamcar1"), "/test_gam"),
    seed,
    brms::bf(y ~ s(x0) + s(x1) + s(x2) + s(x3)),
    data_gam,
    car1 = FALSE,
    chains = 2
  )

  fit_gam2 <- fit_stan_model(
    paste0(system.file("extdata", package = "bgamcar1"), "/test_gam2"),
    seed,
    brms::bf(y ~ s(x0, by = g) + s(x1, by = g) + s(x2, by = g) + s(x3, by = g)),
    data_gam2,
    save_warmup = FALSE,
    car1 = FALSE,
    chains = 2
  )

  fit_gam3 <- fit_stan_model(
    paste0(system.file("extdata", package = "bgamcar1"), "/test_gam2"),
    seed,
    brms::bf(y ~ s(x0, by = g) + s(x1, by = g) + s(x2, by = g) + s(x3, by = g) + ar(time = x1, gr = g)),
    data_gam2,
    save_warmup = FALSE,
    car1 = FALSE,
    chains = 2
  )

  list(
    # data:
    data_gam = data_gam,
    data_gam2 = data_gam2,
    # models:
    fit_gam = fit_gam,
    fit_gam2 = fit_gam2,
    fit_gam3 = fit_gam3
  )

}

# test-add_car1_err.R

help_add_car1_err <- function() {

  phi <- .7

  car1_input <- tidyr::crossing(.index = 1:2, location = letters[1:2], rep = 1:200) |>
    dplyr::mutate(
      `ar[1]` = phi,
      nu = 1e3,
      sigma = 1,
      .epred = 0
    )

  car1_input_test <- car1_input

  data_car1 <- withr::with_seed(32567, {
    car1_input |>
      dplyr::mutate(d_x = 1) |>
      add_car1_err(gr_vars = c(".index", "location")) |>
      dplyr::filter(.index == 1, location == "a")
  })

  fit <- arima(data_car1$.prediction, order = c(1, 0, 0))

  sub <- withr::with_seed(219, {
    car1_input |>
      dplyr::group_by(.index, location) |>
      dplyr::slice_sample(prop = .6) |>
      dplyr::ungroup() |>
      dplyr::arrange(.index, location, rep) |>
      dplyr::mutate(d_x = replace_na(rep - lag(rep), 0))
  })

  data2_car1 <- withr::with_seed(32567, {
    sub |>
      add_car1_err(gr_vars = c(".index", "location")) |>
      filter(.index == 1, location == "a")
  })

  fit2 <- arima(data2_car1$.prediction, order = c(1, 0, 0))

  list(
    phi = phi,
    fit = fit,
    fit2 = fit2,
    car1_input = car1_input,
    car1_input_test = car1_input_test
  )
}

# test-add_car1.R

help_add_car1 <- function(inputs) {

  add_car1_input <- inputs$data_ar |>
    dplyr::mutate(.index = 1, `ar[1]` = .7, .epred = 0)

  car1 <- add_car1(add_car1_input, "y", gr_vars = c(".index", "series")) |>
    dplyr::mutate(r = y - .epred)

  autocorr <- extract_acf(car1$r)

  autocorr2 <- arima(add_car1_input$y, order = c(1, 0, 0)) |>
    residuals() |>
    extract_acf()

  add_car1_sub <- withr::with_seed(3526, {
    dplyr::slice_sample(add_car1_input, prop = .7) |>
      dplyr::arrange(series, date) |>
      dplyr::group_by(series) |>
      dplyr::mutate(
        d_x = date - dplyr::lag(date),
        d_x = tidyr::replace_na(d_x, 0),
        d_x = as.numeric(d_x)
      ) |>
      dplyr::ungroup()
  })

  car1 <- add_car1(add_car1_sub, "y", gr_vars = c(".index", "series")) |>
    dplyr::mutate(r = y - .epred)

  car1_cor <- car1 |>
    dplyr::group_by(series) |>
    dplyr::mutate(
      r_lag = dplyr::lag(r),
      y_lag = dplyr::lag(y)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(d_x == 1) |>
    dplyr::summarize(
      cor_r = cor(r_lag, r),
      cor_y = cor(y_lag, y)
    )

  list(
    autocorr = autocorr,
    autocorr2 = autocorr2,
    car1_cor = car1_cor
  )

}

extract_acf <- function(x) {
  x |>
    acf(plot = FALSE) |>
    with(acf) |>
    as.numeric()
}

# test-add_pred_draws_car1.R

calc_sigma <- function(x) {
  varnames <- extract_resp(x)
  distributional <- as.character(x$formula)[2] |>
    str_detect("sigma ~ ")
  if (distributional) {
    groups <- unique(x$data[, varnames$gr_sigma])
  }
  draws <- x |>
    brms::as_draws_df("sigma", regex = TRUE) |>
    tibble::as_tibble()
  if (distributional) {
    draws <- draws |>
      tidyr::pivot_longer(c(
        tidyselect::starts_with("b_sigma_"),
        -tidyselect::matches("^b_sigma_Intercept$")
      )) |>
      dplyr::mutate(value = b_sigma_Intercept + value) |>
      tidyr::pivot_wider(names_from = name, values_from = value) |>
      tidyr::pivot_longer(starts_with("b_sigma_"), values_to = "sigma") |>
      dplyr::select(c(name, sigma, tidyselect::starts_with("."))) |>
      dplyr::mutate(
        sigma = exp(sigma),
        group = stringr::str_remove(name, varnames$gr_sigma) |>
          stringr::str_extract("(?<=b_sigma_).+$") |>
          stringr::str_replace("Intercept", groups[1])
      )
  }
  draws
}

compare_preds <- function(x, ...) {
  x |>
    dplyr::select(date, series, y, d_x, .row, .epred, ...) |>
    dplyr::arrange(.draw) |>
    dplyr::ungroup()
}

# test-calc_acf.R

help_calc_acf <- function() {

  data_acf1 <- tibble::tibble(
    .draw =  1,
    .residual = c(1:9, -1e2),
    location = "a"
  )

  data_acf_test1 <- data_acf1

  c1 <- calc_acf(data_acf1, gr_vars = c(".draw", "location"))
  c2 <- calc_acf(data_acf1, .residual > 0, gr_vars = c(".draw", "location"))

  data_acf2 <- tidyr::crossing(
    location = letters[1:2],
    .residual = as.numeric(1:10)
  ) |>
    dplyr::mutate(
      .residual = dplyr::if_else(location == "b", .residual - 100, .residual),
      .draw = 1
    )

  data_acf_test2 <- data_acf2

  c3 <- calc_acf(data_acf2, gr_vars = c(".draw", "location"))

  data_acf3 <- tidyr::crossing(
    series = letters[1:3],
    .draw = 1:3,
    .residual = as.numeric(1:10)
  ) |>
    dplyr::mutate(
      censoring = withr::with_seed(1245, {
        sample(c(-1, 1), length(series), replace = TRUE)
      }
      ),
      .residual = dplyr::if_else(censoring == -1, -10, .residual)
    )

  acf3 <- calc_acf(
    data_acf3,
    censoring == 1 & cens_lagged == 1,
    cen_var = "censoring",
    gr_vars = c(".draw", "series")
  )

  list(
    data_acf1 = data_acf1,
    data_acf_test1 = data_acf_test1,
    c1 = c1,
    c2 = c2,
    c3 = c3,
    data_acf2 = data_acf2,
    data_acf_test2 = data_acf_test2,
    data_acf3 = data_acf3,
    acf3 = acf3
  )
}

# test-calc_ll.R

help_calc_ll_preds <- function(inputs) {

  # fit:

  ll_brm1 <- brms::log_lik(inputs$fit)
  ll_myfn_in1 <- add_pred_draws_car1(inputs$data, inputs$fit, draw_ids = 1:3000, car1 = FALSE) |>
    dplyr::ungroup()
  ll_myfn1 <- ll_myfn_in1 |>
    calc_ll("y", censored = "ycens", upper = "y2") |>
    tidyr::pivot_wider(id_cols = c(.draw, .chain, .iteration), names_from = .row, values_from = log_lik) |>
    dplyr::select(tidyselect::matches("^\\d")) |>
    as.matrix()

  colnames(ll_myfn1) <- NULL

  # fit_ar

  ll_brm2 <- brms::log_lik(inputs$fit_ar)
  ll_myfn_in2 <- add_pred_draws_car1(inputs$data_ar, inputs$fit_ar, draw_ids = 1:2000) |>
    dplyr::ungroup()
  ll_myfn_in_test <- ll_myfn_in2
  ll_myfn2 <- ll_myfn_in2 |>
    calc_ll("y", cens = FALSE) |>
    tidyr::pivot_wider(id_cols = c(.draw, .chain, .iteration), names_from = .row, values_from = log_lik) |>
    dplyr::select(tidyselect::matches("^\\d")) |>
    as.matrix()

  colnames(ll_myfn2) <- NULL
}

help_calc_ll <- function(inputs) {

  # fit:

  ll_brm1 <- brms::log_lik(inputs$fit)
  ll_myfn_in1 <- add_pred_draws_car1(inputs$data, inputs$fit, draw_ids = 1:3000, car1 = FALSE) |>
    dplyr::ungroup()
  ll_myfn1 <- ll_myfn_in1 |>
    calc_ll("y", censored = "ycens", upper = "y2") |>
    tidyr::pivot_wider(id_cols = c(.draw, .chain, .iteration), names_from = .row, values_from = log_lik) |>
    dplyr::select(tidyselect::matches("^\\d")) |>
    as.matrix()

  colnames(ll_myfn1) <- NULL

  # fit_ar

  ll_brm2 <- brms::log_lik(inputs$fit_ar)
  ll_myfn_in2 <- add_pred_draws_car1(inputs$data_ar, inputs$fit_ar, draw_ids = 1:2000) |>
    dplyr::ungroup()
  ll_myfn_in_test <- ll_myfn_in2
  ll_myfn2 <- ll_myfn_in2 |>
    calc_ll("y", cens = FALSE) |>
    tidyr::pivot_wider(id_cols = c(.draw, .chain, .iteration), names_from = .row, values_from = log_lik) |>
    dplyr::select(tidyselect::matches("^\\d")) |>
    as.matrix()

  colnames(ll_myfn2) <- NULL

  list(
    ll_brm1 = ll_brm1,
    ll_brm2 = ll_brm2,
    ll_myfn1 = ll_myfn1,
    ll_myfn2 = ll_myfn2,
    ll_myfn_in2 = ll_myfn_in2,
    ll_myfn_in_test = ll_myfn_in_test
  )
}

# test-local_slope.R

help_local_slope <- function(inputs) {
  slopes <- local_slope(inputs$data_gam, inputs$fit_gam, "x2", smooth = "s(x2)", pts = 498)
  slopes2 <- local_slope(inputs$data_gam2, inputs$fit_gam2, "x2", smooth = "s(x2, by = g)", g_var = "g", pts = 459)
  slopes3 <- local_slope(
    inputs$data_gam2, inputs$fit_gam2, "x2", smooth = "s(x2, by = g)",
    add_vars = list(g = "b"), pts = 459
  )
  slopes4 <- local_slope(
    inputs$data_gam2, inputs$fit_gam3, "x2", smooth = "s(x2, by = g)",
    g_var = "g", add_vars = list(x1 = 1:(459 * 3)), pts = 459
  )
  list(
    slopes = slopes,
    slopes2 = slopes2,
    slopes3 = slopes3,
    slopes4 = slopes4
  )
}

# test-summarize_preds.R

help_summarize_preds <- function() {
  data_sumpred <- tidyr::crossing(
    g = letters[1:5],
    y = 1:10
  ) |>
    dplyr::mutate(.epred = scale(log(y))[,1]) |>
    dplyr::group_by(g, y)

  data_sumpred_test <- data_sumpred

  x_sum <- summarize_preds(data_sumpred, y_var = y) |>
    dplyr::filter(!dplyr::near(y, .epred_retrans)) |>
    nrow()

  x_sum2 <- withr::with_seed(1246, {
    data_sumpred |>
      ungroup() |>
      slice_sample(prop = .75) |>
      group_by(g, y)
  })

  s1 <- x_sum2 |>
    summarize_preds(y_var = y) |>
    filter(!near(y, .epred_retrans)) |>
    nrow()

  s2 <- x_sum2|>
    summarize_preds(y_var = data_sumpred$y) |>
    filter(!near(y, .epred_retrans)) |>
    nrow()

  list(
    data_sumpred = data_sumpred,
    data_sumpred_test = data_sumpred_test,
    x_sum = x_sum,
    s1 = s1,
    s2 = s2
  )
}
