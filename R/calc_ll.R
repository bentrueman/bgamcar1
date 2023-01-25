#' Calculate the log-likelihood of the posterior predictive distribution
#'
#' @param x A dataframe of posterior predictions.
#' @param response The name of the response variable.
#' @param censored The name of the censoring indicator.
#' @param cens Logical. Is the response censored?
#' @param upper For interval censoring, the upper bound (`y2` argument to `brms::cens()`).
#' Extracted from the model object by `loo_cv()`, but defaults to the response variable if not supplied.
#'
#' @return A matrix of log-likelihoods.
#' @importFrom data.table as.data.table `:=`
#' @importFrom brms pstudent_t dstudent_t
#' @importFrom dplyr case_when
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
#'  ll_in <- add_pred_draws_car1(data, fit, draw_ids = 1:10, car1 = FALSE)
#'  calc_ll(ll_in, "y", "y2", "ycens")
calc_ll <- function(x, response, upper = NA, censored = NULL, cens = TRUE) {

  nu <- NULL
  .epred <- NULL
  sigma <- NULL
  log_lik <- NULL
  ..response <- NULL
  if(is.na(upper)) {
    upper <- response
  }

  x <- as.data.table(x)
  if (cens) {
    x[, log_lik := case_when(
      get(..censored) == "none" ~ dstudent_t(get(..response), nu, .epred, sigma, log = TRUE),
      get(..censored) == "left" ~ pstudent_t(get(..response), nu, .epred, sigma, log.p = TRUE),
      get(..censored) == "right" ~ pstudent_t(get(..response), nu, .epred, sigma, log.p = TRUE, lower.tail = FALSE),
      get(..censored) == "interval" ~ log(
        exp(pstudent_t(get(..upper), nu, .epred, sigma, log.p = TRUE)) -
          exp(pstudent_t(get(..response), nu, .epred, sigma, log.p = TRUE))
      )

    )]
  } else {
    x[, log_lik := dstudent_t(
      get(..response), nu, .epred, sigma,
      log = TRUE
    )]
  }
}
