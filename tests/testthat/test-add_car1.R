
library("dplyr")
library("readr")
library("withr")
library("tidyr")
library("brms")

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

extract_acf <- function(x) {
  x %>%
    acf(plot = FALSE) %>%
    with(acf) %>%
    as.numeric()
}

add_car1_input <- data_ar %>%
  mutate(.index = 1, `ar[1]` = .7, .epred = 0)

test_that(
  "add_car1() models an AR(1).", {
    car1 <- add_car1(add_car1_input, "y", gr_vars = c(".index", "series")) %>%
      mutate(r = y - .epred)
    autocorr <- extract_acf(car1$r)
    autocorr2 <- arima(add_car1_input$y, order = c(1, 0, 0)) %>%
      residuals() %>%
      extract_acf()
    expect_lt(max(abs(autocorr - autocorr2)), .025)
  })

test_that(
  "add_car1() models an irregularly-spaced AR(1).", {

    add_car1_sub <- with_seed(3526, {
      slice_sample(add_car1_input, prop = .7) %>%
        arrange(series, date) %>%
        group_by(series) %>%
        mutate(
          d_x = date - lag(date),
          d_x = replace_na(d_x, 0),
          d_x = as.numeric(d_x)
        ) %>%
        ungroup()
    })

    car1 <- add_car1(add_car1_sub, "y", gr_vars = c(".index", "series")) %>%
      mutate(r = y - .epred)

    car1_cor <- car1 %>%
      group_by(series) %>%
      mutate(
        r_lag = lag(r),
        y_lag = lag(y)
      ) %>%
      ungroup() %>%
      filter(d_x == 1) %>%
      summarize(
        cor_r = cor(r_lag, r),
        cor_y = cor(y_lag, y)
      )
    expect_equal(car1_cor$cor_r, 0, tolerance = .1)
  })

test_that(
  "add_car1() yields the same results as tidybayes::add_epred_draws() for an AR(1) model.", {
    tbl1 <- tidybayes::add_epred_draws(data_ar, fit_ar) %>%
      rename(.index = .draw) %>%
      ungroup()
    ar1 <- as_draws_df(fit_ar, "ar[1]") %>%
      as_tibble() %>%
      select(-c(.chain, .iteration))
    tbl2 <- tidybayes::add_epred_draws(data_ar, fit_ar, incl_autocor = FALSE) %>%
      left_join(ar1, by = ".draw") %>%
      rename(.index = .draw) %>%
      add_car1("y", gr_vars = c(".index", "series")) %>%
      select(-`ar[1]`)
    expect_equal(tbl1$.epred, tbl2$.epred)
  })
