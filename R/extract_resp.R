#' Extract formula variables from a `brmsfit` object
#'
#' @param x A `brmsfit` object
#'
#' @return A list of extracted formula variables for use by other `bgamcar1` functions
#' @importFrom brms prepare_predictions
#' @importFrom dplyr if_else
#' @export
#'
#' @examples
#' library("brms")
#' seed <- 1
#' data <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data.csv"))
#' fit <- fit_stan_model(
#'    paste0(system.file("extdata", package = "bgamcar1"), "/test"),
#'    seed,
#'    bf(y | cens(ycens) ~ 1),
#'    data,
#'    prior(normal(0, 1), class = Intercept),
#'    car1 = FALSE,
#'    save_warmup = FALSE,
#'    chains = 3
#'  )
#' extract_resp(fit)
extract_resp <- function(x) {
  if (class(x)[1] != "brmsfit") stop("'x' must be a brmsfit object")

  prep <- prepare_predictions(x)

  as_char <- as.character(x$formula)
  response <- str_extract(as_char[1], "\\w+")
  cenvar <- str_extract(as_char[1], "(?<=cens\\()\\w+")
  gr_sig <- str_extract(as_char[2], "(?<=sigma ~ ).+(?=\\)$)")
  gr_ar <- prep$dpars$mu$ac$acef$gr
  time_ar <- prep$dpars$mu$ac$acef$time
  list(
    "resp" = response,
    "cens" = cenvar,
    "gr_sigma" = gr_sig,
    "gr_ar" = if_else(gr_ar == "NA", NA_character_, gr_ar),
    "time_ar" = time_ar
  )
}
