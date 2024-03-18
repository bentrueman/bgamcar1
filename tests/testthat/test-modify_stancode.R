
test_that("modify_stancode(modify = \"car1\") adds expected number of characters to stan code.", {
  inputs <- load_test_models()
  fit_stancode <- brms::make_stancode(
    inputs$fit$formula, inputs$data, prior = inputs$fit$prior, family = "student"
  )
  n1 <- stringr::str_count(fit_stancode)
  n1_mod <- modify_stancode(fit_stancode) %>%
    stringr::str_count()
  d1 <- n1_mod - n1

  fit_ar_stancode <- brms::make_stancode(
    inputs$fit_ar$formula, inputs$data_ar, prior = inputs$fit_ar$prior,
    family = "student"
  )
  n2 <- stringr::str_count(fit_ar_stancode)
  n2_mod <- modify_stancode(fit_ar_stancode) %>%
    stringr::str_count()
  d2 <- n2_mod - n2
  expect_equal(d1, 35) # 35 chars to data block
  expect_equal(d2, 73) # 73 chars overall
})

test_that("modify_stancode(modify = \"car1\") modifies appropriate code snippets.", {
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

test_that("modify_stancode() works when both methods are used in sequence.", {
  inputs <- load_test_models()
  stancode_original <- brms::stancode(inputs$fit_car1_missing)
  stancode_modified <- stancode_original %>%
    modify_stancode(modify = "car1", var_car1 = "y") %>%
    modify_stancode(modify = "xcens", var_xcens = "xmissing", lower_bound = 0)
  expect_message(
    modify_stancode(stancode_original, modify = "xcens", var_xcens = "xmissing", lower_bound = 0),
    regexp = "for multivariate models, remember to specify which response gets CAR\\(1\\) errors."
  )
  # left-censoring modifications:
  modification_1 <- "Yl_xmissing\\[Jcens_xmissing\\] = Ycens_xmissing; // add imputed left-censored values"
  modification_2 <- "vector<lower=0, upper=U_xmissing>\\[Ncens_xmissing\\] Ycens_xmissing;  // estimated left-censored"
  modification_3 <- "array\\[Ncens_xmissing\\] int<lower=1> Jcens_xmissing;  // positions of left-censored"
  expect_false(stringr::str_detect(stancode_original, modification_1))
  expect_true(stringr::str_detect(stancode_modified, modification_1))
  expect_false(stringr::str_detect(stancode_original, modification_2))
  expect_true(stringr::str_detect(stancode_modified, modification_2))
  expect_false(stringr::str_detect(stancode_original, modification_3))
  expect_true(stringr::str_detect(stancode_modified, modification_3))
  # CAR(1) modifications:
  modification_4 <- "mu_y\\[n\\] \\+= Err_y\\[n, 1\\] \\* pow\\(ar_y\\[1\\], s\\[n\\]\\); // CAR\\(1\\)"
  expect_false(stringr::str_detect(stancode_original, modification_4))
  expect_true(stringr::str_detect(stancode_modified, modification_4))
})
