
# generate datasets/models for tests:

# n.b., a dummy file "badtest_1.csv" has been added to inst/extdata to test that fit_stan_model() ignores it

library("brms")
library("withr")
library("tibble")
library("dplyr")
library("tidyr")
library("readr")
# library("bgamcar1")
library("mgcv")
load_all()

#------------------ test models ------------------

seed <- 1

# data:

data <- withr::with_seed(seed, {
  tibble(
    y = rnorm(100),
    ycens = case_when(
      y <= -1 ~ "left",
      y >= 1 ~ "right",
      y >= 0 & y <= 0.5 ~ "interval",
      TRUE ~ "none"
    )
  ) %>%
    mutate(
      y = case_when(
        ycens == "none" ~ y,
        ycens == "left" ~ -1,
        ycens == "right" ~ 1,
        ycens == "interval" ~ 0,
      ),
      y2 = if_else(ycens == "interval", 0.5, y)
    )
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

data_car1_missing <- data_car1 %>%
  transmute(
    time = x,
    d_x,
    x = withr::with_seed(seed, rnorm(length(data_car1$x))),
    # censor x at -1:
    x_cens = if_else(x < -1, "left", "none"),
    x = pmax(x, -1),
    y = x + y
  )

data_car1_missing$x[59] <- NA

data_gam <- withr::with_seed(seed, mgcv::gamSim(1, n = 200, scale = 2))

data_gam2 <- withr::with_seed(seed, {
  data_gam %>%
    slice_sample(prop = 1/3) %>%
    crossing(g = as.factor(letters[1:3]))
})

# fitted models:

fit <- fit_stan_model(
  "inst/extdata/test",
  seed,
  bf(y | cens(ycens, y2 = y2) ~ 1),
  data,
  prior(normal(0, 1), class = Intercept),
  car1 = FALSE,
  save_warmup = FALSE,
  chains = 3,
  backend = "cmdstanr",
  overwrite = TRUE
)

form_ar <- bf(y ~ ar(time = date, gr = series), sigma ~ series)
prior_ar <- prior(normal(0, 1), class = Intercept)
generate_mu <- c(
  "generated quantities" =
  "// matrix storing lagged residuals
  matrix[N, max_lag] Err2 = rep_matrix(0, N, max_lag);
  vector[N] err2;  // actual residuals
  // initialize linear predictor term
  vector[N] mu2 = rep_vector(0.0, N);
  mu2 += Intercept;
  // include ARMA terms
  for (n in 1:N) {
    err2[n] = Y[n] - mu2[n];
    for (i in 1:J_lag[n]) {
      Err2[n + 1, i] = err2[n + 1 - i];
    }
    mu2[n] += Err2[n, 1] * pow(ar[1], s[n]); // CAR(1)
  }"
)

fit_ar <- fit_stan_model(
  "inst/extdata/test_ar",
  seed,
  form_ar,
  data_ar,
  prior_ar,
  save_warmup = FALSE,
  chains = 1,
  iter_sampling = 500,
  backend = "cmdstanr",
  overwrite = TRUE,
  stancode = generate_mu,
  sig_figs = 18
)

form_car1 <- bf(y ~ ar(time = x))

fit_car1 <- fit_stan_model(
  "inst/extdata/test_car1",
  seed,
  form_car1,
  data_car1,
  prior_ar,
  save_warmup = FALSE,
  chains = 2,
  backend = "cmdstanr",
  overwrite = TRUE
)

form_car1_missing <- bf(y ~ mi(x) + ar(time = time)) +
  bf(x | mi() ~ 1, family = "gaussian") +
  set_rescor(FALSE)

fit_car1_missing <- fit_stan_model(
  "inst/extdata/test_car1_missing",
  seed,
  form_car1_missing,
  data_car1_missing,
  prior_ar,
  save_warmup = FALSE,
  chains = 2,
  backend = "cmdstanr",
  overwrite = TRUE,
  var_car1 = "y",
  var_xcens = "x",
  cens_ind = "x_cens",
  lcl = -1
)

fit_gam <- fit_stan_model(
  "inst/extdata/test_gam",
  seed,
  bf(y ~ s(x0) + s(x1) + s(x2) + s(x3)),
  data_gam,
  save_warmup = FALSE,
  car1 = FALSE,
  chains = 2,
  backend = "cmdstanr",
  overwrite = TRUE
)

fit_gam2 <- fit_stan_model(
  "inst/extdata/test_gam2",
  seed,
  bf(y ~ s(x0, by = g) + s(x1, by = g) + s(x2, by = g) + s(x3, by = g)),
  data_gam2,
  save_warmup = FALSE,
  car1 = FALSE,
  chains = 2,
  backend = "cmdstanr",
  overwrite = TRUE
)

write_csv(data, "inst/extdata/data.csv")
write_csv(data_ar, "inst/extdata/data_ar.csv")
write_csv(data_car1, "inst/extdata/data_car1.csv")
write_csv(data_car1_missing, "inst/extdata/data_car1_missing.csv")
write_csv(data_gam, "inst/extdata/data_gam.csv")
write_csv(data_gam2, "inst/extdata/data_gam2.csv")
