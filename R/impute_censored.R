#' Impute left-censored observations via posterior prediction.
#'
#' @param x A dataframe of observations and posterior predictions.
#' @param input The input dataframe used to generate the model.
#' @param yvar Name of the column containing left-censored observations.
#' @param ycens Name of the column containing the censoring indicator.
#'
#' @return A dataframe containing the (possibly imputed) observations.
#' @importFrom dplyr %>%  ungroup if_else
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
#' @importFrom tidyselect matches
#' @export
#'
#' @examples
#' x <- data.frame(
#'    x = 1:10,
#'    y = rnorm(10),
#'    .prediction = rnorm(10),
#'    cens = "left"
#' )
#' impute_censored(x, x, "y", "cens")
impute_censored <- function(x, input, yvar, ycens) {
  x <- ungroup(x) %>%
    data.frame()
  x[, yvar] <- if_else(
    x[, ycens] != "none",
    x$.prediction,
    x[, yvar]
  )
  x %>%
    as_tibble() %>%
    select(matches(paste(names(input), collapse = "|")))
}
