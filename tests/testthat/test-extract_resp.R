
library("dplyr")
library("brms")
library("readr")

seed <- 1

data <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
data_ar <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))

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

test_that("extract_resp() returns error when input is wrong class.", {
  x <- tibble(formula = 1)
  expect_error(extract_resp(x))
})

test_that("extract_resp() extracts the correct model names.", {
  fit_resp <- extract_resp(fit) %>%
    unlist()
  fit_ar_resp <- extract_resp(fit_ar) %>%
    unlist()
  expect_equal(fit_resp, c(resp = "y", cens = "ycens", gr_sigma = NA))
  expect_equal(
    fit_ar_resp,
    c(resp = "y", cens = NA, gr_sigma = "series", gr_ar = "series", time_ar = "date")
  )
})