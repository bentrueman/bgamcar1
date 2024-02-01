
test_that("local_slope() returns expected values", {

  inputs <- load_test_gams()
  slp <- help_local_slope(inputs)

  # add_vars argument generates the same output as g_var:
  sm2 <- slp$slopes2 |>
    dplyr::filter(g == "b")
  sm3 <- slp$slopes3 |>
    dplyr::relocate(g, .before = x2)
  expect_equal(sm2, sm3)

  # local_slope() argument add_var doesn't change results when added (arbitrary)
  # variable not included in smooth (and is instead added to satisfy brms::posterior_smooths() requirement
  # that variables used in brms::ar() also present in newdata)
  expect_equal(slp$slopes2$smooth, slp$slopes4$smooth)
  expect_equal(slp$slopes2$slope, slp$slopes4$slope)
  expect_snapshot(slp$slopes)
  expect_snapshot(slp$slopes2) # factor-smooth interaction

})

test_that("local_slope() returns an error when smooth term does not include CAR(1) variables", {
  skip() # argument to local_slope() add_vars no longer necessary for brms version 2.19.6 (and higher?)
  inputs <- load_test_gams()
  expect_error(
    local_slope(inputs$data_gam2, inputs$fit_gam3, "x2", smooth = "s(x2, by = g)", g_var = "g"),
    regexp = "One or more variables used to construct CAR\\(1\\) term"
  )
})

test_that("local_slope() returns an error for multivariate model.", {
  inputs <- load_test_models()
  expect_error(
    local_slope(inputs$data_car1_missing, inputs$fit_car1_missing),
    regexp = "postprocessing methods do not currently support multivariate models"
  )
})
