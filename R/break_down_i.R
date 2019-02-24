#' Model Agnostic Sequential Variable Attributions
#'
#' This function finds Variable Attributions via Sequential Variable Conditioning
#' It calls either `local_attributions` or `local_interactions`.
#'
#' @param ... parameters passed to `local_*` functions
#' @param interactions shall interactions be included
#'
#' @return an object of the `break_down` class
#'
#' @examples
#' \dontrun{
#' ## Not run:
#' library("DALEX")
#' library("iBreakDown")
#' library("randomForest")
#' set.seed(1313)
#' # example with interaction
#' # classification for HR data
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HR_test[1,]
#'
#' explainer_rf <- explain(model,
#'                         data = HR[1:1000,1:5],
#'                         y = HR$status[1:1000])
#'
#' bd_rf <- break_down(explainer_rf,
#'                            new_observation)
#' bd_rf
#' plot(bd_rf, start_baseline = TRUE)
#' }
#' @export
#' @rdname break_down
break_down <- function(..., interactions = FALSE) {
  if (interactions) {
    res <- local_interactions(...)
  } else {
    res <- local_attributions(...)
  }
  res
}
