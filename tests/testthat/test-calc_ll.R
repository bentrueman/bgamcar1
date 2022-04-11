
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

ll_brm <- brms::log_lik(fit_ar)
ll_myfn_in <- add_pred_draws_car1(data_ar, fit_ar, draw_ids = 1:2000) %>%
  ungroup()
ll_myfn_in_test <- ll_myfn_in
ll_myfn <- ll_myfn_in %>%
  calc_ll("y", cens = FALSE) %>%
  pivot_wider(c(.draw, .chain, .iteration), names_from = .row, values_from = log_lik) %>%
  select(matches("^\\d")) %>%
  as.matrix()

colnames(ll_myfn) <- NULL

test_that("calc_ll() returns the same likelihoods as brms::log_lik()", {
  expect_equal(ll_brm, ll_myfn)
})

test_that("calc_ll() doesn't alter input", {
  expect_equal(ll_myfn_in, ll_myfn_in_test)
})
