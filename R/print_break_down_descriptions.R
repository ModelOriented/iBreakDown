#' Print Generic for Break Down Objects
#'
#' @param x a description of \code{break_down_description} class.
#' @param ... other parameters.
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://pbiecek.github.io/ema}
#'
#' @return a character
#'
#' @export

print.break_down_description <- function(x, ...) {
  for (element in x) {
    cat(element, "\n")
  }

  return(invisible(NULL))
}
