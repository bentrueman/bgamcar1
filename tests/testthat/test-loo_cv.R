
library("brms")
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

loo1 <- loo(fit_ar)
loo2 <- loo_cv(data_ar, fit_ar, censoring = FALSE, draw_ids = 1:2000)

test_that("brms::loo() yields the same results as loo_cv()", {
  expect_equal(loo1$estimate, loo2$estimate)
})
