#' Extract formula variables from a `brmsfit` object
#'
#' @param x A `brmsfit` object
#'
#' @return A list of extracted formula variables for use by other `bgamcar1` functions
#' @importFrom brms prepare_predictions brmsterms
#' @importFrom dplyr if_else
#' @importFrom stats as.formula terms
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
#' extract_resp(fit)
extract_resp <- function(x) {
  stopifnot("'x' must be a brmsfit object" = class(x)[1] == "brmsfit")
  prep <- prepare_predictions(x)
  bterms <- brmsterms(x$formula)
  response <- x$formula$resp
  stopifnot(
    "postprocessing methods do not currently support multivariate models" = length(response) == 1
  )
  censoring_terms <- brms:::get_ad_vars(bterms, "cens")
  grouping_factor_sigma <- bterms$dpars$sigma$formula
  grouping_factor_sigma <- if (is.null(grouping_factor_sigma)) grouping_factor_sigma else
    labels(terms(as.formula(grouping_factor_sigma)))
  grouping_factor_ar <- prep$dpars$mu$ac$acef$gr
  # convert "NA" to NA_character_:
  grouping_factor_ar <- if (is.null(grouping_factor_ar)) grouping_factor_ar else
    if (grouping_factor_ar == "NA") NA_character_ else grouping_factor_ar
  time_variable_ar <- prep$dpars$mu$ac$acef$time
  list(
    "resp" = response,
    "cens" = censoring_terms[1],
    "y2" = censoring_terms[2],
    "gr_sigma" = grouping_factor_sigma,
    "gr_ar" = grouping_factor_ar,
    "time_ar" = time_variable_ar
  )
}
