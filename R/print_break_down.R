#' Print Generic for Break Down Objects
#'
#' @param x the model model of `break_down` class.
#' @param ... other parameters.
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument.
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{\link{signif}} which keeps a specified number of significant digits.
#' Or the default \code{\link{round}} to have the same precision for all components.
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @return a data frame
#'
#' @export
print.break_down <- function(x, ..., digits = 3, rounding_function = round) {
  class(x) = "data.frame"
  x$contribution <- rounding_function(x$contribution, digits)
  rownames(x) <- make.unique(paste0(x$label, ": ", x$variable), sep = "_")
  print(x[, "contribution", drop = FALSE])
  invisible(NULL)
}
