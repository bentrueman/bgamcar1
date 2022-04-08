
x <- rnorm(10, 10)
x_trans <- scale(x)[,1]
x_ltrans <- scale(log(x))[,1]
x_retrans <- retrans(x_trans, x, log = FALSE)
x_reltrans <- retrans(x_ltrans, x)

rtdata <- data.frame(
  orig = x,
  trans = x_trans,
  ltrans = x_ltrans
)

rtdata$re_trans <- retrans(rtdata$trans, rtdata$orig, log = FALSE)
rtdata$re_ltrans <- retrans(rtdata$ltrans, rtdata$orig)

test_that("retrans() works as expected.", {
  expect_equal(x, x_retrans)
  expect_equal(x, x_reltrans)
  expect_equal(rtdata$orig, rtdata$re_trans)
  expect_equal(rtdata$orig, rtdata$re_ltrans)
})
