
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
  bf(y | cens(ycens, y2 = y2) ~ 1),
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

test_that("modify_stancode() modifies appropriate code snippets.", {
  s1 <- make_stancode(
    form_ar,
    data = data_ar
  )
  s2 <- modify_stancode(s1)
  r1 <- "response variable\\\n  vector\\[N\\] s;  \\/\\/ CAR\\(1\\) exponent\\\n"
  r2 <- "vector<lower=0, upper=1>\\[Kar\\] ar;"
  r3 <- "mu\\[n\\] \\+= Err\\[n, 1\\] \\* pow\\(ar\\[1\\], s\\[n\\]\\); \\/\\/ CAR\\(1\\)"
  expect_false(str_detect(s1, r1))
  expect_false(str_detect(s1, r2))
  expect_false(str_detect(s1, r3))
  expect_true(str_detect(s2, r1))
  expect_true(str_detect(s2, r2))
  expect_true(str_detect(s2, r3))

})
