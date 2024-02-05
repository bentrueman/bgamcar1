
test_that("add_stancode() works", {
  inputs <- load_test_models()
  # modification_1 <- "\n  array[N] real y_predict;\n  y_predict = b_Intercept"
  modification_1 <- "add new code here"
  scode <-inputs$fit$model
  scode_modified <- add_stancode(inputs$fit$model, new_code = modification_1)
  expect_false(str_detect(scode, modification_1))
  expect_true(str_detect(scode_modified, modification_1))
})
