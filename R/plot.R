#' Plot Generic for Break Down Objects
#'
#' @param x the model model of 'break_down' class
#' @param ... other parameters
#' @param add_contributions shall variable contributions to be added on plot?
#' @param vcolors named vector with colors
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{signif()} which keeps a specified number of significant digits.
#' Or the default \code{round()} to have the same precision for all components
#' @param plot_distributions if TRUE then distributions of conditional propotions will be plotted. This requires keep_distributions=TRUE in the broken.default().
#' @param baseline if numeric then veritical line will start in baseline
#'
#' @return a ggplot2 object
#' @import ggplot2
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
#' bd_rf <- local_attributions(explainer_rf,
#'                            new_observation)
#' bd_rf
#' plot(bd_rf)
#' plot(bd_rf, baseline = 0)
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            new_observation,
#'                            keep_distributions = TRUE)
#' bd_rf
#' plot(bd_rf, plot_distributions = TRUE)
#'
#' bd_rf <- local_interactions(explainer_rf,
#'                  new_observation,
#'                  keep_distributions = TRUE)
#'
#' bd_rf
#' plot(bd_rf)
#' plot(bd_rf, plot_distributions = TRUE)
#'
#' # example for regression - apartment prices
#' # here we do not have intreactions
#' model <- randomForest(m2.price ~ . , data = apartments)
#' explainer_rf <- explain(model,
#'                         data = apartments_test[1:1000,2:6],
#'                         y = apartments_test$m2.price[1:1000])
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            apartments_test[1,])
#' bd_rf
#' plot(bd_rf, digits = 1)
#' plot(bd_rf, digits = 1, baseline = 0)
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            apartments_test[1,],
#'                            keep_distributions = TRUE)
#' plot(bd_rf, plot_distributions = TRUE)
#'
#' bd_rf <- local_interactions(explainer_rf,
#'                  new_observation = apartments_test[1,],
#'                  keep_distributions = TRUE)
#'
#' bd_rf
#' plot(bd_rf)
#' plot(bd_rf, plot_distributions = TRUE)
#' }
#' @export
plot.break_down <- function(x, ..., add_contributions = TRUE, baseline = NA,
                        vcolors = c("-1" = "#f05a71", "0" = "#371ea3", "1" = "#8bdcbe", "X" = "#371ea3"),
                        digits = 3, rounding_function = round, plot_distributions = FALSE) {
  position <- cummulative <- prev <- variable <- pretty_text <- right_side <- contribution <- trans_contribution <- prediction <- label <- id <- NULL

  if (plot_distributions) {
    df <- attr(x, "yhats_distribution")
    if (is.null(df))
      stop("You need to use keep_distributions=TRUE in the broken.default() ")

    pl <- ggplot(df, aes(variable, prediction, group = factor(variable))) +
      geom_line(aes(group = id), alpha = 0.01) +
      geom_violin(scale = "width", adjust = 3) +
      stat_summary(fun.y = "mean", colour = "red", size = 4, geom = "point") +
      xlab("") + ylab("") +
      facet_wrap(~label, scales = "free_y", ncol = 1)
  } else {
    broken_cumm <- x
    class(broken_cumm) = "data.frame"
    broken_cumm$sign[broken_cumm$variable_name == ""] <- "X"
    broken_cumm$prev <- broken_cumm$cummulative - broken_cumm$contribution
    broken_cumm$cummulative <- broken_cumm$cummulative
    broken_baseline <- broken_cumm[broken_cumm$variable_name == "intercept",]
    broken_cumm$text <- broken_cumm$prev
    if (is.na(baseline)) {
      for (lab in broken_baseline$label) {
        broken_cumm[broken_cumm$label == lab &
                      broken_cumm$variable == "prediction", "prev"] <- broken_baseline[broken_baseline$label == lab,"contribution"]
        broken_cumm[broken_cumm$label == lab &
                      broken_cumm$variable == "intercept", "prev"] <- broken_baseline[broken_baseline$label == lab,"contribution"]
      }
    } else {
      broken_baseline$contribution <- baseline
      broken_cumm[broken_cumm$variable == "prediction", "prev"] <- baseline
      broken_cumm[broken_cumm$variable == "intercept", "prev"] <- baseline
    }

    broken_cumm$trans_contribution <- broken_cumm$cummulative - broken_cumm$text
    broken_cumm$right_side <- pmax(broken_cumm$cummulative,
                                   broken_cumm$cummulative - broken_cumm$contribution )
#    broken_cumm <- droplevels(broken_cumm)
      ctmp <- as.character(rounding_function(broken_cumm$trans_contribution, digits))
      broken_cumm$pretty_text <-
        paste0(ifelse((substr(ctmp, 1, 1) == "-") |
                      (broken_cumm$variable == "prediction") |
                      (broken_cumm$variable == "intercept"), "", "+"), ctmp)

    pl <- ggplot(broken_cumm, aes(x = position + 0.5,
                                  y = pmax(cummulative, prev),
                                  xmin = position + 0.15, xmax = position + 0.85,
                                  ymin = cummulative, ymax = prev,
                                  fill = sign,
                                  label = pretty_text))

    if (add_contributions) {
      drange <- diff(range(broken_cumm$cummulative))
#      maxposition <- max(c(broken_cumm$cummulative, 0)) + 0.03*diff(range(broken_cumm$cummulative))
      pl <- pl + geom_text(aes(y = right_side), vjust = 0.5, nudge_y = drange/20, hjust = 0, color = "#371ea3")
    }

    pl <- pl +
      geom_errorbarh(data = broken_cumm[broken_cumm$variable_name != "", ],
                     aes(xmax = position - 0.85,
                         xmin = position + 0.85,
                         y = cummulative), height = 0,
                     color = "#371ea3") +
      geom_rect(alpha = 0.9) +
      geom_hline(data = broken_baseline, aes(yintercept = contribution), lty = 3, alpha = 0.5, color = "#371ea3") +
      facet_wrap(~label, scales = "free_y", ncol = 1)

    pl <- pl +
      scale_y_continuous(expand = c(0.05,0.15), name = "") +
      scale_x_continuous(labels = broken_cumm$variable, breaks = broken_cumm$position + 0.5, name = "") +
      scale_fill_manual(values = vcolors)
  }

   pl + coord_flip() + theme_drwhy_vertical() +
     theme(legend.position = "none")
}
