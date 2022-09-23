
test_that("add_car1_err() generates a regular AR(1) process.", {
  inputs <- help_add_car1_err()
  expect_equal(as.numeric(inputs$fit$coef[1]), inputs$phi, tolerance = .1)
})

test_that("add_car1_err() generates an irregular AR(1) process.", {
  inputs <- help_add_car1_err()
  expect_equal(as.numeric(inputs$fit2$coef[1]), inputs$phi, tolerance = .1)
})

test_that("add_car1_err() doesn't alter input.", {
  inputs <- help_add_car1_err()
  expect_equal(inputs$car1_input_test, inputs$car1_input)
})
