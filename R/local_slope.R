
#' Calculate the local slope of a smooth term using finite differences
#'
#' @param input A dataframe for which to generate model predictions.
#' @param object A `brms` model object.
#' @param x_var The predictor associated with the selected smooth term.
#' @param epsilon The increment on which to generate differences.
#' @param smooth The smooth term for which to calculate local slopes. Passed on to `brms::posterior_smooths()`.
#' @param ... Additional arguments passed to `brms::posterior_smooths()`.
#' @param g_var An optional grouping variable for factor-smooth interactions.
#' @param add_vars A named list containing variables to add as columns to the dataframe passed to `brms::posterior_smooths()` as `newdata`.
#' Useful when the smooth specified by `smooth` does not include the variables used to construct the CAR(1) term
#' (causing an error in `posterior_smooths()`).
#'
#' @return A `tibble` containing the (tidy) output of `brms::posterior_smooth()` and the calculated slopes.
#' @importFrom brms posterior_smooths
#' @importFrom tidyr pivot_longer crossing
#' @importFrom tidyselect everything starts_with
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library("brms")
#' seed <- 1
#' data_gam <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_gam.csv"))
#' fit <- fit_stan_model(
#'   paste0(system.file("extdata", package = "bgamcar1"), "/test_gam"),
#'   seed,
#'   bf(y ~ s(x0) + s(x1) + s(x2) + s(x3)),
#'   data_gam,
#'   car1 = FALSE,
#'   chains = 2
#' )
#' local_slope(data_gam, fit, "x2", smooth = "s(x2)")
#'
local_slope <- function(input, object, x_var, epsilon = .001, smooth, g_var = NULL, add_vars = NULL, ...) {

  grid_1 <- data.frame(
    seq(
      min(input[, x_var]),
      max(input[, x_var]),
      by = 2 * epsilon
    )
  )

  message(paste0(paste0("Evaluating at ", dim(grid_1)[1]), " points (per group)"))

  names(grid_1) <- x_var

  if (!is.null(g_var)) {
    g <- unique(input[, g_var])
    grid_1 <- crossing(g, grid_1)
  }

  # add variables to grid to satisfy brms checks on ts models:
  if (!is.null(add_vars)) {
    for (i in 1:length(add_vars)) {
      grid_1[, names(add_vars)[i]] <- add_vars[[i]]
    }
  }

  grid_2 <- grid_1
  grid_avg <- grid_1

  grid_2[, x_var] <- grid_1[, x_var] + epsilon
  grid_avg[, x_var] <- (grid_1[, x_var] + grid_2[, x_var]) / 2

  smooth_1 <- try(posterior_smooths(object, smooth = smooth, newdata = grid_1, ...))
  if (!is.matrix(smooth_1)) {
    if (smooth_1 == "Error : Time points within groups must be unique.\n") {
      stop("One or more variables used to construct CAR(1) term not used in the smooth term.
           Add them via `add_vars` to satisfy brms::posterior_smooths()")
    }
  }
  smooth_2 <- posterior_smooths(object, smooth = smooth, newdata = grid_2, ...)
  smooth_avg <- posterior_smooths(object, smooth = smooth, newdata = grid_avg, ...)

  derivs <- (smooth_2 - smooth_1) / epsilon

  derivs_df <- as.data.frame(t(derivs))
  smooths_df <- as.data.frame(t(smooth_avg))

  derivs_df <- cbind(grid_avg, derivs_df)
  smooths_df <- cbind(grid_avg, smooths_df)

  derivs_df <- pivot_longer(derivs_df, starts_with("V"), names_to = ".draw", values_to = "slope")
  smooths_df <- pivot_longer(smooths_df, starts_with("V"), names_to = ".draw", values_to = "smooth")

  derivs_df$.draw <- as.numeric(gsub("V", "", derivs_df$.draw))
  derivs_df$smooth <- smooths_df$smooth

  derivs_df
}
