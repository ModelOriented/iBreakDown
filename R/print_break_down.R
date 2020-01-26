#' Print Generic for Break Down Objects
#'
#' @param x an explanation created with \code{\link{break_down}}
#' @param ... other parameters.
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument.
#' @param rounding_function a function to be used for rounding numbers.
#' This should be \code{\link{signif}} which keeps a specified number of significant digits or \code{\link{round}} (which is default) to have the same precision for all components.
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://pbiecek.github.io/ema}
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
