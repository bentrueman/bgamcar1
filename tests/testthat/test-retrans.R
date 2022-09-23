
library("dplyr")

test_that("retrans() works as expected.", {
  x <- rnorm(10, 10)
  x_trans <- scale(x)[,1]
  x_ltrans <- scale(log(x))[,1]
  x_retrans <- retrans(x_trans, x, log = FALSE)
  x_retrans_recens <- retrans(x_trans, x, log = FALSE, recensor = TRUE, lcl = 9)
  x_reltrans <- retrans(x_ltrans, x)
  # in a dataframe:
  data <- tibble::tibble(
    orig = x,
    trans = x_trans,
    ltrans = x_ltrans
  ) %>%
    dplyr::mutate(
      re_trans = retrans(trans, orig, log = FALSE),
      re_ltrans = retrans(ltrans, orig),
    )
  expect_equal(x, x_retrans)
  expect_equal(pmax(x, 9), x_retrans_recens)
  expect_equal(x, x_reltrans)
  expect_equal(data$orig, data$re_trans)
  expect_equal(data$orig, data$re_ltrans)
})
