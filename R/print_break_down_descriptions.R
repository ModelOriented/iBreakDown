#' Print Generic for Break Down Objects
#'
#' @param x description of `descriptions` class.
#' @param ... other parameters.
#'
#' @return a string
#'
#' @export

print.descriptions <- function(x, ...) {
  for (element in x) {
    cat(element, "\n")
  }

  return(invisible(NULL))
}
