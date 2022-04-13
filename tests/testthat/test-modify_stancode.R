
library("stringr")
library("brms")
library("readr")

seed <- 1

data <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
data_ar <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))
data_car1 <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_car1.csv"))

form_ar <- bf(y ~ ar(time = date, gr = series), sigma ~ series)
prior_ar <- prior(normal(0, 1), class = Intercept)

fit <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test"),
  seed,
  bf(y | cens(ycens) ~ 1),
  data,
  prior(normal(0, 1), class = Intercept),
  car1 = FALSE,
  save_warmup = FALSE,
  chains = 3
)

fit_ar <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test_ar"),
  seed,
  form_ar,
  data_ar,
  prior_ar,
  save_warmup = FALSE,
  chains = 2
)

test_that("modify_stancode() adds expected number of characters to stan code.", {
  n1 <- str_count(fit$model)
  n1_mod <- modify_stancode(fit$model) %>%
    str_count()
  d1 <- n1_mod - n1

  n2 <- str_count(fit_ar$model)
  n2_mod <- modify_stancode(fit_ar$model) %>%
    str_count()
  d2 <- n2_mod - n2
  expect_equal(d1, 35) # 35 chars to data block
  expect_equal(d2, 73) # 73 chars overall
})

test_that("modify_stancode() sets lower bound on priors correctly.", {
  s1 <- make_stancode(
    form_ar,
    data = data_ar
  )
  s2 <- make_stancode(
    form_ar,
    data = data_ar,
    prior = prior(normal(0, .5), ar)
  )
  s3 <- make_stancode(
    form_ar,
    data = data_ar,
    prior = prior(student_t(0, .5), ar)
  )
  s4 <- make_stancode(
    form_ar,
    data = data_ar,
    prior = prior(cauchy(0, .5), ar)
  )
  expect_false(str_detect(s1, "normal_lcdf\\(-1 \\|"))
  expect_true(str_detect(s2, "normal_lcdf\\(-1 \\| 0, 0.5\\)"))
  expect_true(str_detect(modify_stancode(s2), "normal_lcdf\\(0 \\| 0, 0.5\\)"))
  expect_true(str_detect(modify_stancode(s3), "student_t_lcdf\\(0 \\| 0, 0.5\\)"))
  expect_true(str_detect(modify_stancode(s4), "cauchy_lcdf\\(0 \\| 0, 0.5\\)"))
})
