#' Extract parameters from a `brms` model
#'
#' @param object A `brms` model object.
#' @param car1 Logical. Extract CAR(1) parameter?
#' @param draw_ids Numeric draw ids from the `brms` model. If NULL (the default), all draws are used.
#'
#' @return A dataframe of draws.
#' @importFrom brms as_draws_df
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library("brms")
#' seed <- 1
#' data <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
#' fit <- fit_stan_model(
#'    paste0(system.file("extdata", package = "bgamcar1"), "/test"),
#'    seed,
#'    bf(y | cens(ycens, y2 = y2) ~ 1),
#'    data,
#'    prior(normal(0, 1), class = Intercept),
#'    car1 = FALSE,
#'    save_warmup = FALSE,
#'    chains = 3
#'  )
#'  extract_params(fit, car1 = FALSE, draw_ids = 1)
extract_params <- function(object, car1 = TRUE, draw_ids = NULL) {

  response <- object$formula$resp
  stopifnot(
    "postprocessing methods do not currently support multivariate models" = length(response) == 1
  )

  Intercept <- NULL

  this_var <- if (car1) {
    "ar[1]"
  } else {
    "Intercept"
  }

  out_raw <- as_draws_df(object, variable = this_var) %>%
    as_tibble()

  if (is.null(draw_ids)) {
    draw_ids <- seq_len(nrow(out_raw))
  }

  out <- out_raw %>%
    filter(.data$.draw %in% draw_ids) %>%
    mutate(.index = as.numeric(factor(.data$.draw)))

  if (!car1) {
    # this adds .chain and .iteration to draws object, which would otherwise be missing
    # (but is needed for loo::loo())
    select(out, -Intercept)
  } else {
    out
  }
}
