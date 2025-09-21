#' Summarize model predictions
#'
#' @param x A dataframe of model predictions.
#' @param y_var The untransformed response, used to backtransform predictions.
#' @param retrans Logical. Backtransform predictions?
#' @param pred_var The column name containing predictions to summarize.
#' @param ... Arguments passed on to `retrans()`.
#'
#' @return A dataframe containing the summarized predictions.
#' @importFrom data.table as.data.table
#' @importFrom tidyselect all_of
#' @importFrom dplyr  %>% select mutate rename
#' @importFrom tibble as_tibble
#' @importFrom stats median quantile
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
#'    chains = 3,
#'    backend = "cmdstanr"
#'  )
#' preds <- add_pred_draws_car1(data, fit, car1 = FALSE, draw_ids = 1234)
#' summarize_preds(preds, y_var = y, log = FALSE)
summarize_preds <- function(x, y_var = lead, retrans = TRUE, pred_var = ".epred", ...) {

  .epred <- NULL
  lead <- NULL

  grps <- x %>%
    select() %>%
    names()

  x <- as.data.table(x)

  x <- x[, list(
    .epred = median(get(pred_var)),
    .lower = quantile(get(pred_var), .025),
    .upper = quantile(get(pred_var), .975)
  ), by = grps] %>%
    as_tibble() %>%
    rename("{pred_var}" := .epred)

  if(retrans) {
    x <- x %>%
      mutate(
        across(
          all_of(c(pred_var, ".lower", ".upper")),
          ~ retrans(.x, {{ y_var }}, ...),
          .names = "{.col}_retrans"
        )
      )
  }
  return(x)
}
