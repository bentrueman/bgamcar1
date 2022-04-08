#' Back-tranform scaled, logged model predictions
#'
#' @param x A vector.
#' @param scale_var A vector by which to rescale `x`.
#' @param log Logical. Back-transform predictions using `exp()`?
#' @param recensor Logical. Recensor (left-censor) after back-transforming?
#' @param lcl Left-censoring limit for recensor = TRUE.
#'
#' @return A vector of back-transformed predictions.
#' @importFrom stats sd
#' @export
#'
#' @examples
#' x <- rnorm(10, 10)
#' x_trans <- scale(x)[,1]
#' retrans(x_trans, x, log = FALSE)
retrans <- function(x,
                    scale_var,
                    log = TRUE,
                    recensor = FALSE,
                    lcl = 2e-4) {
  rt <- if (log) {
    exp(
      x * sd(log(scale_var), na.rm = TRUE) +
        mean(log(scale_var), na.rm = TRUE)
    )
  } else {
    x * sd(scale_var, na.rm = TRUE) +
      mean(scale_var, na.rm = TRUE)
  }

  if (recensor) {
    rt <- ifelse(rt < lcl, lcl, rt)
  }

  rt
}
