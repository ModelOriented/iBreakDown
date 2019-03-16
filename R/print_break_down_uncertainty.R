#' Print Generic for Break Down Uncertainty Objects
#'
#' @param x object of `break_down_uncertainty` class.
#' @param ... other parameters.
#'
#' @return a data frame.
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#' ## Not run:
#' library("DALEX")
#' library("iBreakDown")
#' library("randomForest")
#' set.seed(1313)
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HR_test[1,]
#'
#' explainer_rf <- explain(model,
#'                         data = HR[1:1000,1:5],
#'                         y = HR$status[1:1000])
#'
#' bd_rf <- local_attributions_uncertainty(explainer_rf,
#'                            new_observation)
#' bd_rf
#'
#' # example for regression - apartment prices
#' # here we do not have intreactions
#' model <- randomForest(m2.price ~ . , data = apartments)
#' explainer_rf <- explain(model,
#'                         data = apartments_test[1:1000,2:6],
#'                         y = apartments_test$m2.price[1:1000])
#'
#' bd_rf <- local_attributions_uncertainty(explainer_rf, apartments_test[1,])
#' bd_rf
#' }
#' @export
print.break_down_uncertainty <- function(x, ...) {

  result <- data.frame(
    min = tapply(x$contribution, paste(x$label, x$variable, sep = "-"), min, na.rm = TRUE),
    q1 = tapply(x$contribution, paste(x$label, x$variable, sep = "-"), quantile, 0.25, na.rm = TRUE),
    median = tapply(x$contribution, paste(x$label, x$variable, sep = "-"), mean, na.rm = TRUE),
    q3 = tapply(x$contribution, paste(x$label, x$variable, sep = "-"), quantile, 0.75, na.rm = TRUE),
    max = tapply(x$contribution, paste(x$label, x$variable, sep = "-"), max, na.rm = TRUE)
  )

  print.data.frame(result)
}
