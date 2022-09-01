
library("brms")

seed <- 1

data_gam <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_gam.csv"))
data_gam2 <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_gam2.csv"))

fit <- fit_stan_model(
   paste0(system.file("extdata", package = "bgamcar1"), "/test_gam"),
   seed,
   bf(y ~ s(x0) + s(x1) + s(x2) + s(x3)),
   data_gam,
   car1 = FALSE,
   chains = 2
 )

fit2 <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test_gam2"),
  seed,
  bf(y ~ s(x0, by = g) + s(x1, by = g) + s(x2, by = g) + s(x3, by = g)),
  data_gam2,
  save_warmup = FALSE,
  car1 = FALSE,
  chains = 2
)

slopes <- local_slope(data_gam, fit, "x2", smooth = "s(x2)")
slopes2 <- local_slope(data_gam2, fit2, "x2", smooth = "s(x2, by = g)", g_var = "g")
smooths <- map_post_smooth(data_gam2, object = fit2, smooth = "s(x2, by = g)", g_var = "g")

test_that("local_slope() returns expected values", {
  expect_snapshot(slopes)
})

test_that("local_slope() returns expected values for factor-smooth interaction", {
  expect_snapshot(slopes2)
})

test_that("unexported function map_post_smooth() returns expected values for factor-smooth interaction", {
  expect_snapshot(smooths)
})

test_that("unexported function map_post_smooth() matches brms::posterior_smooths()", {
  smooths2 <- posterior_smooths(fit2, smooth = "s(x2, by = g)", newdata = data_gam2)
  expect_equal(smooths, smooths2)
})





