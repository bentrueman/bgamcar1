test_that("add_stancode() works", {
  inputs <- load_test_models()
  # basic modifications:
  modification_1 <- "add new code here"
  scode <-inputs$fit$model
  scode_modified <- add_stancode(inputs$fit$model, new_code = modification_1, block = "generated quantities")
  expect_false(str_detect(scode, modification_1))
  expect_true(str_detect(scode_modified, modification_1))
  # test that an addition to the generated quantities block can yield the posterior predictions:
  posterior_predictions_1 <- inputs$fit_ar %>%
    posterior::as_draws_df() %>%
    as_tibble() %>%
    select(starts_with("mu2")) %>%
    as.matrix()
  colnames(posterior_predictions_1) <- NULL
  attributes(posterior_predictions_1) <- attributes(posterior_predictions_1)[1]
  posterior_predictions_2 <- brms::posterior_epred(inputs$fit_ar)
  expect_equal(posterior_predictions_1, posterior_predictions_2)
})
