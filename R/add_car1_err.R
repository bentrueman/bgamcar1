#' Add simulated CAR(1) error to model
#'
#' @param x A dataframe of model predictions.
#' @param car1 Logical. Apply CAR(1) filter?
#' @param gr_vars Grouping variables defining groups within which to apply the CAR(1) filter.
#'
#' @return A dataframe of model predictions with CAR(1) errors.
#' @importFrom tibble rowid_to_column as_tibble
#' @importFrom data.table as.data.table `:=`
#' @importFrom brms rstudent_t
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
#' car1_input <- expand.grid(.index = 1:2, location = letters[1:2], rep = 1:200)
#' car1_input$`ar[1]` <- .7
#' car1_input$nu <- 1e3
#' car1_input$sigma <- 1
#' car1_input$.epred <- 0
#' car1_input$d_x <- 1
#' add_car1_err(car1_input, gr_vars = c(".index", "location"))
add_car1_err <- function(x,
                         car1 = TRUE,
                         gr_vars = c(".index", "series")) {

  nu <- NULL
  mu <- NULL
  sigma <- NULL
  .err <- NULL
  `ar[1]` <- NULL
  d_x <- NULL
  rowid <- NULL

  x <- x %>%
    rowid_to_column() %>%
    as.data.table()

  x[,
    .err := rstudent_t(n = 1, df = nu, mu = 0, sigma = sigma),
    by = rowid
  ][
    ,
    rowid := NULL
  ]

  if (car1) {
    x[,
      .err := filter_car1(.err, phi = unique(`ar[1]`), s = d_x),
      by = c(gr_vars)
    ]
  }

  x %>%
    as_tibble() %>%
    mutate(.prediction = .data$.epred + .data$.err)
}
