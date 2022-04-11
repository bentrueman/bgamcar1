
library("dplyr")
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

data_sumpred <- crossing(
  g = letters[1:5],
  y = 1:10
) %>%
  mutate(.epred = scale(log(y))[,1]) %>%
  group_by(g, y)

data_sumpred_test <- data_sumpred

test_that("summarize_preds() works on simulated data.", {
  x_sum <- summarize_preds(data_sumpred, y_var = y) %>%
    filter(!near(y, .epred_retrans)) %>%
    nrow()
  expect_equal(x_sum, 0)
})

test_that("summarize_preds() accepts a vector for retransformation.", {

  x_sum <- withr::with_seed(1246, {
    data_sumpred %>%
      ungroup() %>%
      slice_sample(prop = .75) %>%
      group_by(g, y)
  })

  s1 <- x_sum %>%
    summarize_preds(y_var = y) %>%
    filter(!near(y, .epred_retrans)) %>%
    nrow()

  s2 <- x_sum%>%
    summarize_preds(y_var = data_sumpred$y) %>%
    filter(!near(y, .epred_retrans)) %>%
    nrow()

  expect_equal(s1, 37)
  expect_equal(s2, 0)

})

test_that("summarize_preds() doesn't alter input", {
  expect_equal(data_sumpred, data_sumpred_test)
})

test_that("summarize_preds() returns the same output as fitted.brmsfit()", {
  s1 <- tidybayes::add_epred_draws(data_ar, fit_ar) %>%
    select(.epred) %>%
    summarize_preds(y_var = y, log = FALSE)
  s2 <- fitted(fit_ar, robust = TRUE)
  expect_equal(s1$.epred, s2[,"Estimate"])
})
