
library("brms")
library("readr")
library("tidyr")

seed <- 1

data <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
data_ar <- read_csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))

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

# fit:

ll_brm1 <- brms::log_lik(fit)
ll_myfn_in1 <- add_pred_draws_car1(data, fit, draw_ids = 1:3000, car1 = FALSE) %>%
  ungroup()
ll_myfn1 <- ll_myfn_in1 %>%
  calc_ll("y", censored = "ycens", upper = "y2") %>%
  pivot_wider(c(.draw, .chain, .iteration), names_from = .row, values_from = log_lik) %>%
  select(matches("^\\d")) %>%
  as.matrix()

colnames(ll_myfn1) <- NULL

# fit_ar

ll_brm2 <- brms::log_lik(fit_ar)
ll_myfn_in2 <- add_pred_draws_car1(data_ar, fit_ar, draw_ids = 1:2000) %>%
  ungroup()
ll_myfn_in_test <- ll_myfn_in2
ll_myfn2 <- ll_myfn_in2 %>%
  calc_ll("y", cens = FALSE) %>%
  pivot_wider(c(.draw, .chain, .iteration), names_from = .row, values_from = log_lik) %>%
  select(matches("^\\d")) %>%
  as.matrix()

colnames(ll_myfn2) <- NULL

test_that("calc_ll() returns the same likelihoods as brms::log_lik()", {
  expect_equal(ll_brm1, ll_myfn1)
  expect_equal(ll_brm2, ll_myfn2)
})

test_that("calc_ll() doesn't alter input", {
  expect_equal(ll_myfn_in2, ll_myfn_in_test)
})
