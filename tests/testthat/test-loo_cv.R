
test_that("brms::loo() yields the same results as loo_cv()", {

  inputs <- load_test_models()

  loo1a <- loo(inputs$fit_ar)
  loo2a <- suppressWarnings(loo_cv(inputs$data_ar, inputs$fit_ar, censoring = FALSE, draw_ids = 1:2000))

  loo1b <- loo(inputs$fit)
  loo2b <- suppressWarnings(loo_cv(inputs$data, inputs$fit, draw_ids = 1:3000, car1 = FALSE))

  expect_equal(loo1a$estimate, loo2a$estimate)
  expect_equal(loo1b$estimate, loo2b$estimate)
})
