
test_that("add_resid_draws_car1() yields the same result as tidybayes::add_residual_draws()", {
  inputs <- load_test_models()
  r1 <- add_resid_draws_car1(inputs$data, inputs$fit, y, car1 = FALSE, draw_ids = 1:3000)
  r2 <- suppressWarnings(tidybayes::add_epred_draws(inputs$data, inputs$fit))
  r3 <- add_resid_draws_car1(inputs$data_ar, inputs$fit_ar, y, draw_ids = 1:2000)
  r4 <- suppressWarnings(tidybayes::add_epred_draws(inputs$data_ar, inputs$fit_ar)) %>%
    dplyr::arrange(.draw, series, date)
  expect_equal(r1$.residual, r2$y - r2$.epred)
  expect_equal(r3$.residual, r4$y - r4$.epred)
})
