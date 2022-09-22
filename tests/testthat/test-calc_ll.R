
test_that("calc_ll() returns the same likelihoods as brms::log_lik()", {
  inputs <- load_test_models()
  ll <- help_calc_ll(inputs)
  t1 <- ll$ll_myfn_in2 %>%
    dplyr::mutate(ycens = "none") %>%
    calc_ll("y", censored = "ycens") %>%
    class()
  expect_equal(ll$ll_brm1, ll$ll_myfn1)
  expect_equal(ll$ll_brm2, ll$ll_myfn2)
  expect_equal(ll$ll_myfn_in2, ll$ll_myfn_in_test) # doesn't alter input
  expect_type(t1, "character") #  handles missing 'upper' arg when cens=TRUE
})

