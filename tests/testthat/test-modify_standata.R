
test_that("modify_standata() creates the expected list", {
  data <- data.frame(x = rep(NA, 11), cens_x = c(rep("none", 10), "left"))
  out <- modify_standata(sdata = list(), data, 999, "x", "cens_x")
  expect_equal(list(Ncens_x = 1, Jcens_x = as.array(11), U_x = 999), out)
})
