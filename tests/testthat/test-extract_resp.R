
test_that("extract_resp() returns error when input is wrong class.", {
  x <- tibble(formula = 1)
  expect_error(extract_resp(x), regexp = "'x' must be a brmsfit object")
})

test_that("extract_resp() extracts the correct model names.", {
  inputs <- load_test_models()
  fit_resp <- extract_resp(inputs$fit) |>
    unlist()
  fit_ar_resp <- extract_resp(inputs$fit_ar) |>
    unlist()
  expect_equal(fit_resp, c(resp = "y", cens = "ycens", y2 = "y2"))
  expect_equal(
    fit_ar_resp,
    c(resp = "y", cens = NA, y2 = NA, gr_sigma = "series", gr_ar = "series", time_ar = "date")
  )
})

test_that("extract_resp() does not return error in looking for y2", {
  inputs <- load_test_models()
  brmsmod <- brms::brm(
    brms::bf("y | cens(ycens) ~ 1"),
    data = dplyr::mutate(inputs$data, ycens = if_else(ycens == "interval", "none", ycens)),
    empty = TRUE
  )
  brmsmod$fit <- inputs$fit$fit
  t1 <- length(extract_resp(brmsmod))
  expect_equal(t1, 6)
})

test_that("extract_resp() works with a GAM even when the 'by' variable is converted to a character vector", {
  inputs <- load_test_gams()
  this_gam <- inputs$fit_gam3
  this_gam$data$g <- as.character(this_gam$data$g)
  expect_type(extract_resp(this_gam), "list")
})

test_that("extract_resp() returns an error for a multivariate model", {
  inputs <- load_test_models()
  expect_error(
    extract_resp(inputs$fit_car1_missing),
    regexp = "postprocessing methods do not currently support multivariate models"
  )
})
