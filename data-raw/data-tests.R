
# generate datasets/models for tests:

library("brms")
library("withr")
library("tibble")
library("dplyr")
library("tidyr")
library("readr")
# library("bgamcar1")
library("devtools")
load_all()

#------------------ test models ------------------

seed <- 1

# data:

data <- withr::with_seed(seed, {
  tibble(
    y = rnorm(100),
    ycens = if_else(y <= -1, "left", "none")
  ) %>%
    mutate(y = pmax(y, -1))
})

data_ar <- withr::with_seed(seed, {
  tibble(
    date = seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "1 day"),
    A = arima.sim(list(ar = .7), length(date)),
    B = arima.sim(list(ar = .7), length(date))
  ) %>%
    mutate(
      date = as.numeric(date),
      date = date - min(date) + 1
    ) %>%
    pivot_longer(
      c(A, B),
      names_to = "series",
      values_to = "y"
    ) %>%
    arrange(series, date) %>%
    group_by(series) %>%
    mutate(
      d_x = date - lag(date),
      d_x = replace_na(d_x, 0),
      d_x = as.numeric(d_x)
    ) %>%
    ungroup()
})

phi_car1 <- .45
p_ret <- .6 # proportion retained

data_car1 <- withr::with_seed(seed, {
  tibble(
    x = 1:200,
    y = arima.sim(list(ar = phi_car1), length(x))
  ) %>%
    slice_sample(prop = p_ret) %>%
    arrange(x) %>%
    mutate(
      x_lag = lag(x),
      d_x = replace_na(x - x_lag, 0) # spacing of observations
    )
})

# fitted models:

fit <- fit_stan_model(
  "inst/extdata/test",
  seed,
  bf(y | cens(ycens) ~ 1),
  data,
  prior(normal(0, 1), class = Intercept),
  car1 = FALSE,
  save_warmup = FALSE,
  chains = 3
)

form_ar <- bf(y ~ ar(time = date, gr = series), sigma ~ series)
prior_ar <- prior(normal(0, 1), class = Intercept)

fit_ar <- fit_stan_model(
  "inst/extdata/test_ar",
  seed,
  form_ar,
  data_ar,
  prior_ar,
  save_warmup = FALSE,
  chains = 2
)

fit_ar2 <- brm(
  form_ar,
  data = data_ar,
  family = student(),
  seed = seed,
  chains = 2,
  file = "inst/extdata/test_ar_brm"
)

form_car1 <- bf(y ~ ar(time = x))

fit_car1 <- fit_stan_model(
  "inst/extdata/test_car1",
  seed,
  form_car1,
  data_car1,
  prior_ar,
  save_warmup = FALSE,
  chains = 2
)

write_csv(data, "inst/extdata/data.csv")
write_csv(data_ar, "inst/extdata/data_ar.csv")
write_csv(data_car1, "inst/extdata/data_car1.csv")

