#' CAR(1) filter
#'
#' @param x A numeric vector to be filtered.
#' @param phi The CAR(1) coefficient.
#' @param s A numeric vector defining the time spacing between observations.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' d_x = rep(1, length(x))
#' filter_car1(x, .5, s = d_x)
filter_car1 <- function(x, phi, s) {
  if (sum(is.na(x)) > 0) stop("input series contains NAs")
  n <- length(x)
  xf <- rep(NA, n)
  xf[1] <- x[1]
  for (i in 2:n) {
    xf[i] <- phi^s[i] * xf[i - 1] + x[i]
  }
  xf
}
