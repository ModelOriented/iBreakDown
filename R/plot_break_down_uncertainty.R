#' Plot Generic for Break Down Uncertainty Objects
#'
#' @param x the model model of `break_down_uncertainty` class.
#' @param ... other parameters.
#' @param show_boxplots logical if `TRUE` (default) boxplot will be plotted to show uncertanity of attributions
#' @param vcolors named vector with colors.
#'
#' @return a `ggplot2` object.
#' @importFrom stats reorder
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @examples
#' library("DALEX")
#' library("iBreakDown")
#' # Toy examples, because CRAN angels ask for them
#' titanic <- na.omit(titanic)
#' set.seed(1313)
#' titanic_small <- titanic[sample(1:nrow(titanic), 500), c(1,2,6,9)]
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic_small, family = "binomial")
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic_small[,-9],
#'                            y = titanic_small$survived == "yes")
#' bd_rf <- shap(explain_titanic_glm, titanic_small[1, ])
#' bd_rf
#' plot(bd_rf)
#'
#' \donttest{
#' ## Not run:
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
#' bd_rf <- break_down_uncertainty(explainer_rf,
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
#' bd_rf <- break_down_uncertainty(explainer_rf,
#'                                      apartments_test[1,],
#'                                      path = c("floor", "no.rooms", "district",
#'                                          "construction.year", "surface"))
#' bd_rf
#' plot(bd_rf)
#'
#' bd_rf <- shap(explainer_rf,
#'               apartments_test[1,])
#' bd_rf
#' plot(bd_rf)
#' plot(bd_rf, show_boxplots = FALSE)
#' }
#' @export
plot.break_down_uncertainty <- function(x, ...,
                  vcolors = DALEX::theme_drwhy_colors_break_down(),
                  show_boxplots = TRUE) {

  variable <- contribution <- NULL
  x$variable <- reorder(x$variable, abs(x$contribution), mean)

  # base plot
  pl <- ggplot(x, aes(x = variable, y = contribution))
  if (any(x$B == 0)) {
    x_bars <- x[x$B == 0,]
    pl <- pl +
      geom_col(data = x_bars, aes(x = variable, y = contribution, fill = factor(sign(contribution)))) +
      scale_fill_manual(values = vcolors)
  }

  if (show_boxplots) {
    pl <- pl +
      geom_boxplot(coef = 100, fill = "#371ea3", color = "#371ea3", width = 0.25)
  }

  pl +
    facet_wrap(~label, ncol = 1) +
    coord_flip() + theme_drwhy_vertical() +
    theme(legend.position = "none") + xlab("")
}

