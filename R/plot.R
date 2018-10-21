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
#'
#' @return a ggplot2 object
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' library("breakDown")
#' library("randomForest")
#' library("ggplot2")
#' set.seed(1313)
#' model <- randomForest(factor(left)~., data = HR_data, family = "binomial", maxnodes = 5)
#' predict.function <- function(model, new_observation)
#'       predict(model, new_observation, type="prob")[,2]
#' predict.function(model, HR_data[11,-7])
#' explain_1 <- broken(model, HR_data[11,-7], data = HR_data[,-7],
#' predict.function = predict.function, direction = "down")
#' explain_1
#' plot(explain_1) + ggtitle("breakDown plot (direction=down) for randomForest model")
#'
#' explain_2 <- broken(model, HR_data[11,-7], data = HR_data[,-7],
#' predict.function = predict.function, direction = "down", keep_distributions = TRUE)
#' plot(explain_2, plot_distributions = TRUE) +
#'          ggtitle("breakDown distributions (direction=down) for randomForest model")
#'
#' explain_3 <- broken(model, HR_data[11,-7], data = HR_data[,-7],
#' predict.function = predict.function, direction = "up", keep_distributions = TRUE)
#' plot(explain_3, plot_distributions = TRUE) +
#'          ggtitle("breakDown distributions (direction=up) for randomForest model")
#'
#' model <- lm(quality~., data=wine)
#' new_observation <- wine[1,]
#' br <- broken(model, new_observation)
#' plot(br)
#' plot(br, top_features = 2)
#' plot(br, top_features = 2, min_delta = 0.01)
#'}
#' @export
plot.break_down <- function(x, ..., add_contributions = TRUE,
                        vcolors = c("-1" = "#d8b365", "0" = "#f5f5f5", "1" = "#5ab4ac", "X" = "darkgrey"),
                        digits = 3, rounding_function = round, plot_distributions = FALSE) {
  position <- cummulative <- prev <- trans_contribution <- prediction <- label <- id <- NULL

  if (plot_distributions) {
    df <- attr(x, "yhats_distribution")
    if (is.null(df))
      stop("You need to use keep_distributions=TRUE in the broken.default() ")

    pl <- ggplot(df, aes(factor(label), prediction, group=factor(label))) +
      geom_line(aes(group = id), alpha = 0.01) +
      geom_violin(scale = "width", adjust = 3) +
      stat_summary(fun.y = "mean", colour = "red", size = 4, geom = "point") +
      xlab("") + ylab("")
  } else {
    broken_cumm <- x
    broken_cumm$sign[broken_cumm$variable_name == ""] <- "X"
    constant <- attr(broken_cumm, "baseline")
    broken_cumm$prev <- constant + broken_cumm$cummulative - broken_cumm$contribution
    broken_cumm$cummulative <- constant + broken_cumm$cummulative
    class(broken_cumm) = "data.frame"

    broken_cumm$trans_contribution <- broken_cumm$cummulative - broken_cumm$prev
    broken_cumm <- droplevels(broken_cumm)
    pl <- ggplot(broken_cumm, aes(x = position + 0.5,
                                  y = pmax(cummulative, prev),
                                  xmin = position, xmax = position + 0.95,
                                  ymin = cummulative, ymax = prev,
                                  fill = sign,
                                  label = sapply(trans_contribution, function(tmp) as.character(rounding_function(tmp, digits))))) +
      geom_errorbarh(data = broken_cumm[broken_cumm$variable_name != "", ],
                     aes(xmax = position,
                         xmin = position + 2,
                         y = cummulative), height = 0,
                     lty = "F2") +
      geom_rect(alpha = 0.9) +
      geom_hline(yintercept = constant) +
      facet_wrap(~label, ncol = 1)

    if (add_contributions) {
      minposition <- min(c(broken_cumm$cummulative, 0)) - 0.05*diff(range(broken_cumm$cummulative))
      pl <- pl + geom_text(aes(y = minposition), vjust = 0.5, hjust = 1)
    }

    pl <- pl +
      scale_y_continuous(expand = c(0.1,0.1), name = "") +
      scale_x_continuous(labels = broken_cumm$variable, breaks = broken_cumm$position + 0.5, name = "") +
      scale_fill_manual(values = vcolors)
  }

   pl + coord_flip() + theme_classic() +
     theme(legend.position = "none", panel.border = element_blank(),
           axis.line.y = element_line(color = "white"),
           axis.ticks.y = element_line(color = "white"),
           axis.text = element_text(size = 10))
}
