
#' Add the CAR(1) autocorrelation structure.
#'
#' @param x A dataframe.
#' @param response The column of `x` containing the resonse variable.
#' @param gr_vars Grouping variables withing which to calculate lagged residuals.
#'
#' @return The input dataframe with the fitted values transformed to have the CAR(1)
#' autocorrelation structure.
#' @importFrom dplyr ungroup `%>%` lag
#' @importFrom data.table as.data.table `:=`
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples
#' data_ar <- read.csv(paste0(system.file("extdata", package = "bgamcar1"), "/data_ar.csv"))
#' data_ar$`ar[1]` <- .7
#' data_ar$.epred <- 0
#' add_car1(data_ar, "y", gr_vars = "series")
add_car1 <- function(x, response, gr_vars = c(".index", "series")) {

  r_lag <- NULL
  .epred <- NULL
  `ar[1]` <- NULL
  d_x <- NULL

  x <- x %>%
    ungroup() %>%
    as.data.table()

  x[,
    r_lag := replace_na(lag(get(response) - .epred), 0),
    by = c(gr_vars)
  ][,
    .epred := .epred + r_lag * `ar[1]`^d_x,
    by = c(gr_vars)
  ][
    ,
    r_lag := NULL
  ]

  as_tibble(x)
}

