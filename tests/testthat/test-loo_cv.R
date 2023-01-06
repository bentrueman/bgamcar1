
test_that("brms::loo() yields the same results as loo_cv()", {

  inputs <- load_test_models()

  loo1a <- loo(inputs$fit_ar)
  loo2a <- suppressWarnings(loo_cv(inputs$data_ar, inputs$fit_ar, censoring = FALSE, draw_ids = 1:2000))

  loo1b <- loo(inputs$fit)
  loo2b <- suppressWarnings(loo_cv(inputs$data, inputs$fit, draw_ids = 1:3000, car1 = FALSE))

  expect_equal(loo1a$estimate, loo2a$estimate)
  expect_equal(loo1b$estimate, loo2b$estimate)
})

test_that("loo_cv() returns an error when the censoring variable is missing", {
  inputs <- load_test_models()
  expect_error(loo_cv(inputs$data_ar, inputs$fit_ar, censoring = TRUE, draw_ids = 1:2000))
})
