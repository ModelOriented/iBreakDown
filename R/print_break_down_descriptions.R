#' Print Generic for Break Down Objects
#'
#' @param x description of `break_down_description` class.
#' @param ... other parameters.
#'
#' @return a string
#'
#' @export

print.break_down_description <- function(x, ...) {
  for (element in x) {
    cat(element, "\n")
  }

  return(invisible(NULL))
}
