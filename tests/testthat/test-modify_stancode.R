
test_that("modify_stancode() adds expected number of characters to stan code.", {
  inputs <- load_test_models()
  n1 <- stringr::str_count(inputs$fit$model)
  n1_mod <- modify_stancode(inputs$fit$model) %>%
    stringr::str_count()
  d1 <- n1_mod - n1

  n2 <- stringr::str_count(inputs$fit_ar$model)
  n2_mod <- modify_stancode(inputs$fit_ar$model) %>%
    stringr::str_count()
  d2 <- n2_mod - n2
  expect_equal(d1, 35) # 35 chars to data block
  expect_equal(d2, 73) # 73 chars overall
})

test_that("modify_stancode() modifies appropriate code snippets.", {
  inputs <- load_test_models()
  s1 <- brms::make_stancode(
    inputs$form_ar,
    data = inputs$data_ar
  )
  s2 <- modify_stancode(s1)
  r1 <- "response variable\\\n  vector\\[N\\] s;  \\/\\/ CAR\\(1\\) exponent\\\n"
  r2 <- "vector<lower=0, upper=1>\\[Kar\\] ar;"
  r3 <- "mu\\[n\\] \\+= Err\\[n, 1\\] \\* pow\\(ar\\[1\\], s\\[n\\]\\); \\/\\/ CAR\\(1\\)"
  expect_false(stringr::str_detect(s1, r1))
  expect_false(stringr::str_detect(s1, r2))
  expect_false(stringr::str_detect(s1, r3))
  expect_true(stringr::str_detect(s2, r1))
  expect_true(stringr::str_detect(s2, r2))
  expect_true(stringr::str_detect(s2, r3))

})
