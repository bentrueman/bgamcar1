
test_that("ppc_km_nada() throws an error when terms are missing", {
  inputs <- load_test_models()
  expect_error(ppc_km_nada(inputs$data, inputs$fit, seed = seed)) # AR
  expect_error(ppc_km_nada(inputs$data_ar, inputs$fit_ar, seed = seed)) # censoring
})

test_that("ppc_km_nada() yields the same ecdf as NADA::cenfit()", {
  inputs <- load_test_models()
  x <- ppc_km_nada(inputs$data, inputs$fit, draw_ids = 34, seed = 1, car1 = FALSE) %>%
    dplyr::filter(type == "Observations") %>%
    dplyr::select(-c(type, .draw))
  y = NADA::cenfit(obs = inputs$data$y, censored = inputs$data$ycens == "left") %>%
    summary() %>%
    tibble::as_tibble()
  expect_equal(x, y)
})
