
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

  # the following expectations fail on GHA but pass locally
  # I think the problem might have to do with posterior_smooths()
  # (see https://github.com/paul-buerkner/brms/issues/1465)
  skip_on_ci()
  expect_snapshot(slp$slopes)
  expect_snapshot(slp$slopes2) # factor-smooth interaction

})

test_that("local_slope() returns an error when smooth term does not include CAR(1) variables", {
  inputs <- load_test_gams()
  expect_error(
    local_slope(inputs$data_gam2, inputs$fit_gam3, "x2", smooth = "s(x2, by = g)", g_var = "g"),
    regexp = "One or more variables used to construct CAR\\(1\\) term"
  )
})
