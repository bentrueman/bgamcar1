
test_that("add_car1() models an AR(1).", {
  inputs <- load_test_models()
  inputs_car1 <- help_add_car1(inputs)
  expect_lt(max(abs(inputs_car1$autocorr - inputs_car1$autocorr2)), .025)
  expect_equal(inputs_car1$car1_cor$cor_r, 0, tolerance = .1) # irregularly-spaced AR(1)
})

test_that("add_car1() yields the same results as tidybayes::add_epred_draws() for an AR(1) model.", {
  inputs <- load_test_models()
  tbl1 <- tidybayes::add_epred_draws(inputs$data_ar, inputs$fit_ar) |>
    dplyr::rename(.index = .draw) |>
    dplyr::ungroup()
  ar1 <- brms::as_draws_df(inputs$fit_ar, "ar[1]") |>
    dplyr::as_tibble() |>
    dplyr::select(-c(.chain, .iteration))
  tbl2 <- tidybayes::add_epred_draws(inputs$data_ar, inputs$fit_ar, incl_autocor = FALSE) |>
    left_join(ar1, by = ".draw") |>
    rename(.index = .draw) |>
    add_car1("y", gr_vars = c(".index", "series")) |>
    select(-`ar[1]`)
  expect_equal(inputs$tbl1$.epred, inputs$tbl2$.epred)
})

test_that("add_car1() works with d_x declared as a separate argument in fit_stan_model()", {
  skip_on_ci()
  expect_true(FALSE)
})
