
test_that("calc_acf() yields the expected correlation, with and without filtering", {
  inputs <- help_calc_acf()
  expect_equal(inputs$c1$cor, .4)
  expect_equal(inputs$c2$cor, 1)
})

test_that("calc_acf() doesn't alter input", {
  inputs <- help_calc_acf()
  expect_equal(inputs$data_acf1, inputs$data_acf_test1)
})

test_that("calc_acf() calculates lagged residuals correctly.", {
  inputs <- help_calc_acf()
  expect_equal(inputs$c3$cor, 1)
})

test_that("calc_acf() doesn't alter input", {
  inputs <- help_calc_acf()
  expect_equal(inputs$data_acf2, inputs$data_acf_test2)
})

test_that("Filtering works for calc_acf()", {
  inputs <- help_calc_acf()
  expect_error(calc_acf(inputs$data_acf3, cens_lagged = 1))
  expect_equal(inputs$acf3$cor, rep(1, 3))
})
