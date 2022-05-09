
#' Calculate the local slope of a smooth term using finite differences
#'
#' @param input A dataframe for which to generate model predictions.
#' @param object A `brms` model object.
#' @param x_var The predictor associated with the selected smooth term.
#' @param epsilon The increment on which to generate differences.
#' @param smooth The smooth term for which to calculate local slopes. Passed on to `brms::posterior_smooths()`.
#'
#' @return A `tibble` containing the (tidy) output of `brms::posterior_smooth()` and the calculated slopes.
#' @importFrom brms posterior_smooths
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library("brms")
#' seed <- 1
#' data_gam <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_gam.csv"))
#' fit <- fit_stan_model(
#'    paste0(system.file("extdata", package = "bgamcar1"), "/test_gam"),
#'    seed,
#'    bf(y ~ s(x0) + s(x1) + s(x2) + s(x3)),
#'    data_gam,
#'    car1 = FALSE,
#'    chains = 2
#'  )
#' local_slope(data_gam, fit, "x2", smooth = "s(x2)")

local_slope <- function(input, object, x_var, epsilon = .001, smooth) {

  grid_1 <- data.frame(
    seq(
      min(input[, x_var]),
      max(input[, x_var]),
      by = 2 * epsilon
    )
  )

  names(grid_1) <- x_var

  grid_2 <- grid_1
  grid_2[, x_var] <- grid_2[, x_var] + epsilon

  grid_avg <- (grid_1 + grid_2) / 2

  smooth_1 <- posterior_smooths(object, smooth = smooth, newdata = grid_1)
  smooth_2 <- posterior_smooths(object, smooth = smooth, newdata = grid_2)
  smooth_avg <- posterior_smooths(object, smooth = smooth, newdata = grid_avg)

  derivs <- (smooth_2 - smooth_1) / epsilon

  derivs_df <- as.data.frame(t(derivs))
  smooths_df <- as.data.frame(t(smooth_avg))

  derivs_df[, x_var] <- grid_avg
  smooths_df[, x_var] <- grid_avg

  derivs_df <- pivot_longer(derivs_df, -.data[[x_var]], names_to = ".draw", values_to = "slope")
  smooths_df <- pivot_longer(smooths_df, -.data[[x_var]], names_to = ".draw", values_to = "smooth")

  derivs_df$.draw <- as.numeric(gsub("V", "", derivs_df$.draw))
  derivs_df$smooth <- smooths_df$smooth

  derivs_df

}
