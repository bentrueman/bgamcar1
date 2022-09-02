
library("brms")
library("dplyr")

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

fit3 <- fit_stan_model(
  paste0(system.file("extdata", package = "bgamcar1"), "/test_gam2"),
  seed,
  bf(y ~ s(x0, by = g) + s(x1, by = g) + s(x2, by = g) + s(x3, by = g) + ar(time = x1, gr = g)),
  data_gam2,
  save_warmup = FALSE,
  car1 = FALSE,
  chains = 2
)

slopes <- local_slope(data_gam, fit, "x2", smooth = "s(x2)")
slopes2 <- local_slope(data_gam2, fit2, "x2", smooth = "s(x2, by = g)", g_var = "g")
slopes3 <- local_slope(data_gam2, fit2, "x2", smooth = "s(x2, by = g)", add_vars = list(g = "b"))
slopes4 <- local_slope(
  data_gam2, fit3, "x2", smooth = "s(x2, by = g)", g_var = "g", add_vars = list(x1 = 1:(459 * 3))
)

test_that("local_slope() returns expected values", {
  expect_snapshot(slopes)
})

test_that("local_slope() returns expected values for factor-smooth interaction", {
  expect_snapshot(slopes2)
})

test_that("add_vars argument generates the same output as g_var", {
  sm2 <- slopes2 %>%
    filter(g == "b")
  sm3 <- slopes3 %>%
    relocate(g, .before = x2)
  expect_equal(sm2, sm3)
})

test_that("local_slope() add_vars argument returns same result as g_var argument", {
  sm2 <- slopes2 %>%
    filter(g == "b")
  sm3 <- slopes3 %>%
    relocate(g, .before = x2)
  expect_equal(sm2, sm3)
})

test_that("local_slope() returns an error when smooth term does not include CAR(1) variables", {
  expect_error(
    local_slope(data_gam2, fit3, "x2", smooth = "s(x2, by = g)", g_var = "g"),
    regexp = "One or more variables used to construct CAR\\(1\\) term"
  )
})

test_that("local_slope() argument add_var doesn't change results when added (arbitrary)
variable not included in smooth (and is instead added to satisfy brms::posterior_smooths() requirement
          that variables used in brms::ar() also present in newdata)", {
  expect_equal(slopes2$smooth, slopes4$smooth)
  expect_equal(slopes2$slope, slopes4$slope)
})


