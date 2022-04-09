
# generate datasets for tests:

library("withr")
library("tibble")
library("dplyr")
library("tidyr")
library("readr")

seed <- 1

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

write_csv(data_ar, "tests/testthat/data_ar.csv")
