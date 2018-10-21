#' Break Down Plot
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
#' @param start_baseline if TRUE then veritical line will start in baselines
#'
#' @return a ggplot2 object
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' ## Not run:
#' library("DALEX2")
#' library("breakDown2")
#' library("randomForest")
#' set.seed(1313)
#' # example with interaction
#' # classification for HR data
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HRTest[1,]
#'
#' explainer_rf <- explain(model,
#'                         data = HR[1:1000,1:5],
#'                         y = HR$status[1:1000])
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            new_observation)
#' bd_rf
#' plot(bd_rf)
#' plot(bd_rf, start_baseline = TRUE)
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            new_observation,
#'                            keep_distributions = TRUE)
#' bd_rf
#' plot(bd_rf, plot_distributions = TRUE)
#'
#' # example for regression - apartment prices
#' # here we do not have intreactions
#' model <- randomForest(m2.price ~ . , data = apartments)
#' explainer_rf <- explain(model,
#'                         data = apartmentsTest[1:1000,2:6],
#'                         y = apartmentsTest$m2.price[1:1000])
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            apartmentsTest[1,])
#' bd_rf
#' plot(bd_rf, digits = 1)
#' plot(bd_rf, digits = 1, start_baseline = TRUE)
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            apartmentsTest[1,],
#'                            keep_distributions = TRUE)
#' plot(bd_rf, plot_distributions = TRUE)
#'}
#' @export
plot.break_down <- function(x, ..., add_contributions = TRUE, start_baseline = FALSE,
                        vcolors = c("-1" = "#d8b365", "0" = "#f5f5f5", "1" = "#5ab4ac", "X" = "darkgrey"),
                        digits = 3, rounding_function = round, plot_distributions = FALSE) {
  position <- cummulative <- prev <- variable <- contribution <- trans_contribution <- prediction <- label <- id <- NULL

  if (plot_distributions) {
    df <- attr(x, "yhats_distribution")
    if (is.null(df))
      stop("You need to use keep_distributions=TRUE in the broken.default() ")

    pl <- ggplot(df, aes(variable, prediction, group = factor(variable))) +
      geom_line(aes(group = id), alpha = 0.01) +
      geom_violin(scale = "width", adjust = 3) +
      stat_summary(fun.y = "mean", colour = "red", size = 4, geom = "point") +
      xlab("") + ylab("") +
      facet_wrap(~label, ncol = 1)
  } else {
    broken_cumm <- x
    class(broken_cumm) = "data.frame"
    broken_cumm$sign[broken_cumm$variable_name == ""] <- "X"
    broken_cumm$prev <- broken_cumm$cummulative - broken_cumm$contribution
    broken_cumm$cummulative <- broken_cumm$cummulative
    broken_baseline <- broken_cumm[broken_cumm$variable_name == "baseline",]
    broken_cumm$text <- broken_cumm$prev
    for (lab in broken_baseline$label) {
      broken_cumm[broken_cumm$label == lab &
                    broken_cumm$variable == "prediction", "prev"] <- broken_baseline[broken_baseline$label == lab,"contribution"]
      broken_cumm[broken_cumm$label == lab &
                    broken_cumm$variable == "baseline", "prev"] <- broken_baseline[broken_baseline$label == lab,"contribution"]
    }

    broken_cumm$trans_contribution <- broken_cumm$cummulative - broken_cumm$text
    broken_cumm <- droplevels(broken_cumm)
    pl <- ggplot(broken_cumm, aes(x = position + 0.5,
                                  y = pmax(cummulative, prev),
                                  xmin = position, xmax = position + 0.95,
                                  ymin = cummulative, ymax = prev,
                                  fill = sign,
                                  label = sapply(trans_contribution, function(tmp) as.character(rounding_function(tmp, digits)))))

    if (add_contributions) {
      maxposition <- max(c(broken_cumm$cummulative, 0)) + 0.03*diff(range(broken_cumm$cummulative))
      pl <- pl + geom_text(aes(y = maxposition), vjust = 0.5, hjust = 0)
    }

    pl <- pl +
      geom_errorbarh(data = broken_cumm[broken_cumm$variable_name != "", ],
                     aes(xmax = position,
                         xmin = position + 2,
                         y = cummulative), height = 0,
                     lty = "F2") +
      geom_rect(alpha = 0.9) +
      geom_hline(data = broken_baseline, aes(yintercept = contribution)) +
      facet_wrap(~label, ncol = 1)

    pl <- pl +
      scale_y_continuous(expand = c(0.05,0.05), name = "") +
      scale_x_continuous(labels = broken_cumm$variable, breaks = broken_cumm$position + 0.5, name = "") +
      scale_fill_manual(values = vcolors)
  }

   pl + coord_flip() + theme_classic() +
     theme(legend.position = "none", panel.border = element_blank(),
           axis.line.y = element_line(color = "white"),
           axis.ticks.y = element_line(color = "white"),
           axis.text = element_text(size = 10))
}
