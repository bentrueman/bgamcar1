
test_that("extract_params() yields expected output.", {
  inputs <- load_test_models()
  fit_ar_pars <- extract_params(inputs$fit_ar, draw_ids = 400)
  fit_ar_pars_all <- extract_params(inputs$fit_ar)
  reference <- tibble::tribble(
       ~`ar[1]`, ~.chain, ~.iteration, ~.draw, ~.index,
       0.724271262,      1L,        400L,  400L,       1
     )
  expect_equal(fit_ar_pars, reference)
  expect_equal(nrow(fit_ar_pars_all), 500)
})

test_that("extract_params() returns an error for multivariate model.", {
  inputs <- load_test_models()
  expect_error(
    extract_params(inputs$fit_car1_missing),
    regexp = "postprocessing methods do not currently support multivariate models"
  )
})
