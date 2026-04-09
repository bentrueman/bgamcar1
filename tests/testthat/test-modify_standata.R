
test_that("modify_standata() creates the expected list", {
  # single left-censoring limit per variable:
  data <- data.frame(x = rep(NA, 11), cens_x = c(rep("none", 10), "left"))
  out <- modify_standata(sdata = list(), data, lcl = 999, var_xcens = "x", lower_bound = 0, cens_ind = "cens_x")
  expect_equal(list(Ncens_x = 1, Jcens_x = as.array(11), U_x = 999, L_x = 0), out)
  # multiple left-censoring limits per variable:
  data <- data.frame(x = rep(NA, 11), cens_x = c(rep("none", 9), "left", "left"))
  out <- modify_standata(sdata = list(), data, list(c(998, 999)), lower_bound = 100, var_xcens = "x", cens_ind = "cens_x")
  expect_equal(list(Ncens_x = 2, Jcens_x = as.array(10:11), U_x = c(998, 999), L_x = 100), out)
})

