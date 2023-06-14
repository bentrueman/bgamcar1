#' Approximate leave-one-out cross-validation (LOO) using `loo::loo()`.
#'
#' @param input The dataframe of observations used to generate the model.
#' @param object A `brms` model object.
#' @param censoring Logical. Is the response left-censored?
#' @param ... Arguments passed on to `add_pred_draws_car1()`.
#'
#' @return An object of class `loo`.
#' @importFrom dplyr %>%
#' @importFrom data.table dcast
#' @importFrom loo relative_eff loo
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
#'  loo_cv(data, fit, draw_ids = 1:3000, car1 = FALSE)
loo_cv <- function(input, object, censoring = TRUE, ...) {

  .draw <- NULL
  .chain <- NULL
  .iteration <- NULL
  .row <- NULL

  varnames <- extract_resp(object) # extract responses from model formula

  if (censoring == TRUE && is.na(varnames$cens)) {
    stop("No censoring variable found in model formula.")
  }

  n <- as.character(seq_len(nrow(input)))

  ll <- add_pred_draws_car1(input, object, ...) %>%
    calc_ll(varnames$resp, varnames$y2, varnames$cens, cens = censoring) %>%
    dcast(.draw + .chain + .iteration ~ .row, value.var = "log_lik")

  ll_mat <- ll[, c(mget(n))] %>%
    as.matrix()

  rel_eff <- loo::relative_eff(x = exp(ll_mat), chain_id = ll$.chain)

  loo(ll_mat, r_eff = rel_eff)
}
