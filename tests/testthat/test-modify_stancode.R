test_that("modify_stancode(modify = \"car1\") adds expected number of characters to stan code.", {
  inputs <- load_test_models()
  fit_stancode <- brms::make_stancode(
    inputs$fit$formula, inputs$data,
    prior = inputs$fit$prior, family = "student"
  )
  n1 <- stringr::str_count(fit_stancode)
  n1_mod <- modify_stancode(fit_stancode) %>%
    stringr::str_count()
  d1 <- n1_mod - n1

  fit_ar_stancode <- brms::make_stancode(
    inputs$fit_ar$formula, inputs$data_ar,
    prior = inputs$fit_ar$prior,
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
    modify_stancode(modify = "xcens", var_xcens = "xmissing", lower_bound = 0, lcl = 5)
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

test_that("modify_stancode_censored() inserts correct scalar lower bound in parameters block", {
  inputs <- load_test_models()
  scode_raw <- brms::stancode(inputs$fit_car1_missing)
  scode <- modify_stancode_censored(
    scode_raw = scode_raw,
    var_xcens = "xcens",
    lower_bound = -2,
    lcl = -1
  )
  expect_true(grepl("vector<lower=-2, upper=U_xcens>", scode, fixed = TRUE))
})

test_that("modify_stancode_censored() inserts correct vector lower bound in data and parameters blocks", {
  inputs <- load_test_models()
  scode_raw <- brms::stancode(inputs$fit_car1_missing)
  scode <- modify_stancode_censored(
    scode_raw = scode_raw,
    var_xcens = "xcens",
    lower_bound = list(rep(-2, 5)),
    lcl = list(rep(-1, 5))
  )
  expect_true(grepl("vector[Ncens_xcens] L_xcens;", scode, fixed = TRUE))
  expect_true(grepl("vector<lower=L_xcens, upper=U_xcens>", scode, fixed = TRUE))
})

test_that("modify_stancode_censored() inserts correct Stan code for univariate model", {
  univariate_formula <- brms::bf(x | mi() ~ 1)
  data <- data.frame(x = rnorm(50))
  data$x_cens <- ifelse(data$x < -1, "left", "none")
  data$x <- pmax(data$x, -1)
  scode_univariate <- brms::make_stancode(univariate_formula, data)
  scode <- modify_stancode_censored(
    scode_raw = scode_univariate, # a raw univariate brms Stan program
    var_xcens = "xcens",
    lower_bound = -2,
    lcl = -1
  )
  expect_true(grepl("vector<lower=-2, upper=U_xcens>", scode, fixed = TRUE))
  expect_true(grepl("Yl[Jcens_xcens] = Ycens_xcens;", scode, fixed = TRUE))
})

test_that("modify_stancode_censored() inserts correct Stan code for multivariate model", {
  inputs <- load_test_models()
  scode_multivariate <- brms::stancode(inputs$fit_car1_missing)
  scode <- modify_stancode_censored(
    scode_raw = scode_multivariate, # a raw multivariate brms Stan program
    var_xcens = "xcens",
    lower_bound = -2,
    lcl = -1
  )
  expect_true(grepl("Yl_xcens[Jcens_xcens] = Ycens_xcens;", scode, fixed = TRUE))
})

test_that("modify_stancode_censored() with NULL lower_bound produces no lower constraint", {
  inputs <- load_test_models()
  scode_raw <- brms::stancode(inputs$fit_car1_missing)
  scode <- modify_stancode_censored(
    scode_raw = scode_raw,
    var_xcens = "xcens",
    lower_bound = NULL,
    lcl = -1
  )
  expect_false(grepl("vector<lower=", scode, fixed = TRUE))
  expect_true(grepl("vector<upper=U_xcens>", scode, fixed = TRUE))
})

test_that("modify_stancode_censored() handles multiple censored variables", {
  scode_multivariate_2cens <- brms::make_stancode(y ~ x1 + x1, data = data.frame(y = 1:10, x1 = 1:10, x2 = 1:10))
  scode <- modify_stancode_censored(
    scode_raw = scode_multivariate_2cens, # model with two censored predictors
    var_xcens = c("x1", "x2"),
    lower_bound = c(-2, -3),
    lcl = c(-1, -1.5)
  )
  expect_true(grepl("vector<lower=-2, upper=U_x1>", scode, fixed = TRUE))
  expect_true(grepl("vector<lower=-3, upper=U_x2>", scode, fixed = TRUE))
})

test_that("L_ vector dimension in sdata matches Stan code expectation", {
  N_cens <- 9L
  lb <- list(runif(N_cens, -3, -2))
  lcl <- list(runif(N_cens, -1.5, -0.5))
  x_cens <- rep("none", 50)
  x_cens[seq(N_cens)] <- "left"
  sdata <- modify_standata(
    sdata = list(),
    data = data.frame(x = rnorm(50), x_cens = x_cens),
    lcl = lcl,
    lower_bound = lb,
    var_xcens = "x",
    cens_ind = "x_cens"
  )
  inputs <- load_test_models()
  scode_raw <- brms::stancode(inputs$fit_car1_missing)
  scode <- modify_stancode_censored(
    scode_raw = scode_raw,
    var_xcens = "x",
    lower_bound = lb,
    lcl = lcl
  )
  # Stan declares vector[Ncens_x] L_x, so sdata$L_x must have length == sdata$Ncens_x
  expect_equal(length(sdata$L_x), sdata$Ncens_x)
  expect_true(grepl("vector[Ncens_x] L_x;", scode, fixed = TRUE))
})

# not implemented yet

test_that("modify_stancode_censored() errors or warns when lower_bound >= lcl", {
  skip()
  inputs <- load_test_models()
  scode_raw <- brms::stancode(inputs$fit_car1_missing)
  expect_error(
    modify_stancode_censored(
      scode_raw = scode_raw,
      var_xcens = "x",
      lower_bound = -1, # equal to lcl — invalid
      lcl = -1
    )
  )
})

test_that("modify_stancode_censored() handles NA in lower_bound vector gracefully", {
  skip()
  inputs <- load_test_models()
  scode_raw <- brms::stancode(inputs$fit_car1_missing)
  expect_error(
    modify_stancode_censored(
      scode_raw = scode_raw,
      var_xcens = "x",
      lower_bound = list(c(-2, NA, -2)),
      lcl = list(c(-1, -1, -1))
    )
  )
})
