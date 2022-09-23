
test_that("summarize_preds() works on simulated data.", {
  inputs <- help_summarize_preds()
  expect_equal(inputs$x_sum, 0)
})

test_that("summarize_preds() accepts a vector for retransformation.", {
  inputs <- help_summarize_preds()
  expect_equal(inputs$s1, 37)
  expect_equal(inputs$s2, 0)

})

test_that("summarize_preds() doesn't alter input", {
  inputs <- help_summarize_preds()
  expect_equal(inputs$data_sumpred, inputs$data_sumpred_test)
})

test_that("summarize_preds() returns the same output as fitted.brmsfit()", {
  inputs <- load_test_models()
  s1 <- tidybayes::add_epred_draws(inputs$data_ar, inputs$fit_ar) |>
    dplyr::select(.epred) |>
    summarize_preds(y_var = y, log = FALSE)
  s2 <- fitted(inputs$fit_ar, robust = TRUE)
  expect_equal(s1$.epred, s2[,"Estimate"])
})
