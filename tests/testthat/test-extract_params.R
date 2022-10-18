
test_that("extract_params() yields expected output.", {
  inputs <- load_test_models()
  fit_ar_pars <- extract_params(inputs$fit_ar, draw_ids = 1200)
  fit_ar_pars_all <- extract_params(inputs$fit_ar)
  reference <- tibble::tribble(
    ~`ar[1]`, ~.chain, ~.iteration, ~.draw, ~.index,
    0.745734,      2L,        200L,  1200L,       1
  )
  expect_equal(fit_ar_pars, reference)
  expect_equal(nrow(fit_ar_pars_all), 2e3)
})
