#' Print Generic for Break Down Uncertainty Objects
#'
#' @param x an explanation created with \code{\link{break_down_uncertainty}}
#' @param ... other parameters.
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{http://ema.drwhy.ai}
#'
#' @return a data frame.
#' @importFrom stats quantile median
#'
#' @examples
#' library("DALEX")
#' library("iBreakDown")
#' set.seed(1313)
#' model_titanic_glm <- glm(survived ~ gender + age + fare,
#'                        data = titanic_imputed, family = "binomial")
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic_imputed,
#'                            y = titanic_imputed$survived,
#'                            label = "glm")
#'
#' bd_glm <- break_down_uncertainty(explain_titanic_glm, titanic_imputed[1, ])
#' bd_glm
#' plot(bd_glm)
#'
#' \dontrun{
#' ## Not run:
#' library("randomForest")
#' set.seed(1313)
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HR_test[1,]
#'
#' explainer_rf <- explain(model,
#'                         data = HR[1:1000,1:5],
#'                         y = HR$status[1:1000],
#'                         verbose = FALSE)
#'
#' bd_rf <- break_down_uncertainty(explainer_rf,
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
#' bd_rf <- break_down_uncertainty(explainer_rf, apartments_test[1,])
#' bd_rf
#' }
#' @export
print.break_down_uncertainty <- function(x, ...) {

  result <- data.frame(
    min = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), min, na.rm = TRUE),
    q1 = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), quantile, 0.25, na.rm = TRUE),
    median = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), median, na.rm = TRUE),
    mean = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), mean, na.rm = TRUE),
    q3 = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), quantile, 0.75, na.rm = TRUE),
    max = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), max, na.rm = TRUE)
  )

  print.data.frame(result)
}
