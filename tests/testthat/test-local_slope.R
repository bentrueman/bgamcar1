
library("brms")

seed <- 1

data_gam <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_gam.csv"))

fit <- fit_stan_model(
   paste0(system.file("extdata", package = "bgamcar1"), "/test_gam"),
   seed,
   bf(y ~ s(x0) + s(x1) + s(x2) + s(x3)),
   data_gam,
   car1 = FALSE,
   chains = 2
 )

slopes <- local_slope(data_gam, fit, "x2", smooth = "s(x2)")

test_that("local_slope() returns expected values", {
  expect_snapshot(slopes)
})
