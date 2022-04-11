
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

test_that("ppc_km_nada() throws an error when the AR(1) term is missing", {
  expect_error(ppc_km_nada(data, fit, seed = seed))
})

test_that("ppc_km_nada() throws an error when the censoring term is missing", {
  expect_error(ppc_km_nada(data_ar, fit_ar, seed = seed))
})

test_that("ppc_km_nada() yields the same ecdf as NADA::cenfit()", {
  x <- ppc_km_nada(data, fit, draw_ids = 34, seed = seed, car1 = FALSE) %>%
    filter(type == "Observations") %>%
    select(-c(type, .draw))
  y = cenfit(obs = data$y, censored = data$ycens == "left") %>%
    summary() %>%
    as_tibble()
  expect_equal(x, y)
})
