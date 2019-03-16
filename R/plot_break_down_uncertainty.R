#' Plot Generic for Break Down Uncertainty Objects
#'
#' @param x the model model of `break_down_uncertainty` class.
#' @param ... other parameters.
#' @param vcolors named vector with colors.
#'
#' @return a `ggplot2` object.
#' @importFrom stats reorder
#'
#' @examples
#' \dontrun{
#' ## Not run:
#' library("DALEX")
#' library("iBreakDown")
#' library("randomForest")
#' set.seed(1313)
#'
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HR_test[1,]
#'
#' explainer_rf <- explain(model,
#'                         data = HR[1:1000,1:5],
#'                         y = HR$status[1:1000])
#'
#' bd_rf <- local_attributions_uncertainty(explainer_rf,
#'                            new_observation,
#'                            path = c(3,2,4,1,5))
#' bd_rf
#' plot(bd_rf)
#'
#' # example for regression - apartment prices
#' # here we do not have intreactions
#' model <- randomForest(m2.price ~ . , data = apartments)
#' explainer_rf <- explain(model,
#'                         data = apartments_test[1:1000,2:6],
#'                         y = apartments_test$m2.price[1:1000])
#'
#' bd_rf <- local_attributions_uncertainty(explainer_rf,
#'                                      apartments_test[1,],
#'                                      path = c("floor", "no.rooms", "district",
#'                                          "construction.year", "surface"))
#' bd_rf
#' plot(bd_rf)
#' }
#' @export
plot.break_down_uncertainty <- function(x, ...,
                                        vcolors = DALEX::theme_drwhy_colors_break_down()) {

  variable <- contribution <- NULL
  x$variable <- reorder(x$variable, abs(x$contribution), mean)

  # base plot
  if (any(x$B == 0)) {
    x_bars <- x[x$B == 0,]
    pl <- ggplot(x, aes(x = variable, y = contribution)) +
      geom_col(data = x_bars, aes(x = variable, y = contribution, fill = factor(sign(contribution)))) +
      geom_boxplot(coef = 100, fill = "#371ea3", color = "#371ea3", width = 0.2) +
      scale_fill_manual(values = vcolors)
  } else {
    pl <- ggplot(x, aes(x = variable, y = contribution)) +
      geom_boxplot(coef = 100, fill = "#371ea3", color = "#371ea3", width = 0.3)
  }

  pl +
    facet_wrap(~label, ncol = 1) +
    coord_flip() + theme_drwhy_vertical() +
    theme(legend.position = "none") + xlab("")
}

