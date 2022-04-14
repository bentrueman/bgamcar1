
library("tibble")
library("readr")

seed <- 1

data_ar <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))

form_ar <- bf(y ~ ar(time = date, gr = series), sigma ~ series)
prior_ar <- prior(normal(0, 1), class = Intercept)

fit_ar <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test_ar"),
  seed,
  form_ar,
  data_ar,
  prior_ar,
  save_warmup = FALSE,
  chains = 2
)

test_that("extract_params() yields expected output.", {
  fit_ar_pars <- extract_params(fit_ar, draw_ids = 1200)
  reference <- tibble::tribble(
    ~`ar[1]`, ~.chain, ~.iteration, ~.draw, ~.index,
    0.745734,      2L,        200L,  1200L,       1
  )
  expect_equal(fit_ar_pars, reference)
})
