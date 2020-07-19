#' Plot Generic for Break Down Uncertainty Objects
#'
#' @param x an explanation created with \code{\link{break_down_uncertainty}}
#' @param ... other parameters.
#' @param show_boxplots logical if \code{TRUE} (default) boxplot will be plotted to show uncertanity of attributions
#' @param vcolors If \code{NA} (default), DrWhy colors are used.
#' @param max_features maximal number of features to be included in the plot. By default it's \code{10}.
#' @param max_vars alias for the \code{max_features} parameter.
#'
#' @return a \code{ggplot2} object.
#' @importFrom stats reorder
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://pbiecek.github.io/ema}
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
#' sh_glm <- shap(explain_titanic_glm, titanic_imputed[1, ])
#'
#' sh_glm
#' plot(sh_glm)
#'
#' \dontrun{
#' ## Not run:
#' library("randomForest")
#' set.seed(1313)
#'
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HR_test[1,]
#'
#' explainer_rf <- explain(model,
#'                         data = HR[1:1000,1:5])
#'
#' bd_rf <- break_down_uncertainty(explainer_rf,
#'                            new_observation,
#'                            path = c(3,2,4,1,5),
#'                            show_boxplots = FALSE)
#' bd_rf
#' plot(bd_rf, max_features = 3)
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
                  vcolors = DALEX::colors_breakdown_drwhy(),
                  show_boxplots = TRUE,
                  max_features = 10,
                  max_vars = NULL) {

  # aliases
  if (!is.null(max_vars)) {
    max_features <- max_vars
  }

  variable <- contribution <- NULL
  df <- as.data.frame(x)

  df$variable <- reorder(df$variable, df$contribution, function(x) mean(abs(x)))

  vnames <- tail(levels(df$variable), max_features)
  df <- df[df$variable %in% vnames, ]

  # base plot
  pl <- ggplot(df, aes(x = variable, y = contribution))
  if (any(df$B == 0)) {
    x_bars <- df[df$B == 0,]
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
    coord_flip() + DALEX::theme_drwhy_vertical() +
    theme(legend.position = "none") + xlab("")
}

