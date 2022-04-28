
#' Calculate residual autocorrelation
#'
#' @param x A `data.frame`
#' @param ... Arguments passed to a `data.table` chain for subsetting the data.frame after calculating the lagged
#' residual/censoring vectors.
#' @param gr_vars Calculate lagged residual/censoring vector within these groups.
#' @param cen_var Name of the censoring variable to be (optionally) lagged.
#'
#' @return A `tibble` with a Spearman rank correlation coefficient for each residual draw.
#' @importFrom data.table as.data.table shift `:=`
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#'
#' res <- data.frame(
#'   .draw = 1,
#'   .residual = c(1:9, -1e2),
#'   location = "a"
#' )
#' calc_acf(res, gr_vars = c(".draw", "location"))
#'
calc_acf <- function(x,
                     ...,
                     gr_vars = NULL,
                     cen_var = NULL) {
  .residual_lagged <- NULL
  .residual <- NULL
  .draw <- NULL

  x <- as.data.table(x)

  if (is.null(cen_var)) {
    x[,
      .residual_lagged := shift(.residual),
      by = gr_vars
    ]
  } else {
    x[,
      `:=`(
        .residual_lagged = shift(.residual),
        cens_lagged = shift(get(cen_var))
      ),
      by = gr_vars
    ]
  }

  x_cor <- x[
    ...
  ][,
    list(cor = possibly_cor(
      .residual, .residual_lagged,
      method = "spearman", use = "complete"
    )),
    by = .draw
  ]

  as_tibble(x_cor)
}

possibly_cor <- purrr::possibly(cor, otherwise = NA_real_)
