#' Plot Generic for Break Down Objects
#'
#' Displays a waterfall break down plot for objects of \code{break_down} class.
#'
#' @param x an explanation created with \code{\link{break_down}}
#' @param ... other parameters.
#' @param max_features maximal number of features to be included in the plot. default value is \code{10}.
#' @param min_max a range of OX axis. By default \code{NA}, therefore it will be extracted from the contributions of \code{x}. But it can be set to some constants, useful if these plots are to be used for comparisons.
#' @param add_contributions if \code{TRUE}, variable contributions will be added to the plot
#' @param shift_contributions number describing how much labels should be shifted to the right, as a fraction of range. By default equal to \code{0.05}.
#' @param vcolors If \code{NA} (default), DrWhy colors are used.
#' @param vnames a character vector, if specified then will be used as labels on OY axis. By default NULL
#' @param digits number of decimal places (\code{\link{round}}) or significant digits (\code{\link{signif}}) to be used.
#' See the \code{rounding_function} argument.
#' @param rounding_function a function to be used for rounding numbers.
#' This should be \code{\link{signif}} which keeps a specified number of significant digits or \code{\link{round}} (which is default) to have the same precision for all components.
#' @param plot_distributions if \code{TRUE} then distributions of conditional propotions will be plotted. This requires \code{keep_distributions=TRUE} in the
#' \code{\link{break_down}}, \code{\link{local_attributions}}, or \code{\link{local_interactions}}.
#' @param baseline if numeric then veritical line starts in \code{baseline}.
#' @param title a character. Plot title. By default \code{"Break Down profile"}.
#' @param subtitle a character. Plot subtitle. By default \code{""}.
#' @param max_vars alias for the \code{max_features} parameter.
#'
#' @return a \code{ggplot2} object.
#'
#' @import ggplot2
#' @importFrom utils tail
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://ema.drwhy.ai}
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
#' bd_glm <- break_down(explain_titanic_glm, titanic_imputed[1, ])
#' bd_glm
#' plot(bd_glm, max_features = 3)
#' plot(bd_glm, max_features = 3,
#'      vnames = c("average","+ male","+ young","+ cheap ticket", "+ other factors", "final"))
#'
#' \dontrun{
#' ## Not run:
#' library("randomForest")
#' set.seed(1313)
#' # example with interaction
#' # classification for HR data
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HR_test[1,]
#'
#' explainer_rf <- explain(model,
#'                         data = HR[1:1000,1:5])
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            new_observation)
#' bd_rf
#' plot(bd_rf)
#' plot(bd_rf, baseline = 0)
#' plot(bd_rf, min_max = c(0,1))
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
plot.break_down <- function(x, ...,
                            baseline = NA,
                            max_features = 10,
                            min_max = NA,
                            vcolors = DALEX::colors_breakdown_drwhy(),
                            digits = 3, rounding_function = round,
                            add_contributions = TRUE, shift_contributions = 0.05,
                            plot_distributions = FALSE,
                            vnames = NULL,
                            title = "Break Down profile",
                            subtitle = "",
                            max_vars = NULL) {
  position <- cumulative <- prev <- pretty_text <- right_side <- contribution <- NULL
  # fix for https://github.com/ModelOriented/iBreakDown/issues/77
  colnames(x) <- gsub(colnames(x), pattern = "cummulative", replacement = "cumulative")

  # aliases
  if (!is.null(max_vars)) {
    max_features <- max_vars
  }

  if (plot_distributions) {
    vorder <- c(as.character(x$variable)[order(x$position)], "all data")
    df <- attr(x, "yhats_distribution")
    if (is.null(df))
      stop("You need to use keep_distributions=TRUE in the break_down() ")
    pl <- plot_break_down_distributions(df, vorder)
  } else {
    # how many features shall we plot
    x <- select_only_k_features(x, max_features)
    # enrich dataframe with additional features
    tmp <- prepare_data_for_break_down_plot(x, baseline, rounding_function, digits)
    broken_baseline <- tmp$broken_baseline
    x <- tmp$x

    # fix for https://github.com/ModelOriented/iBreakDown/issues/85
    # check if correction is needed
    if (any(x[x$variable == "prediction", "right_side"] < broken_baseline$contribution)) {
      # put there max val
      x[x$variable == "prediction", "right_side"] <- pmax(x[x$variable == "prediction", "right_side"], broken_baseline$contribution)
    }
    if (any(x[x$variable == "intercept", "right_side"] < broken_baseline$contribution)) {
      # put there max val
      x[x$variable == "intercept", "right_side"] <- pmax(x[x$variable == "intercept", "right_side"], broken_baseline$contribution)
    }


    # base plot
    pl <- ggplot(x, aes(x = position + 0.5,
                        y = pmax(cumulative, prev),
                        xmin = position + 0.15, xmax = position + 0.85,
                        ymin = cumulative, ymax = prev,
                        fill = sign,
                        label = pretty_text))
    # add rectangles and hline
    pl <- pl +
      geom_errorbarh(data = x[x$variable_name != "", ],
                     aes(xmax = position - 0.85,
                         xmin = position + 0.85,
                         y = cumulative), height = 0,
                     color = "#371ea3") +
      geom_rect(alpha = 0.9) +
      geom_hline(data = broken_baseline, aes(yintercept = contribution), lty = 3, alpha = 0.5, color = "#371ea3") +
      facet_wrap(~label, scales = "free_y", ncol = 1)

    # add addnotations
    if (add_contributions) {
      drange <- diff(range(x$cumulative))
      pl <- pl + geom_text(aes(y = right_side),
                           vjust = 0.5,
                           nudge_y = drange*shift_contributions,
                           hjust = 0,
                           color = "#371ea3")
    }

    # set limits for contributions
    if (any(is.na(min_max))) {
      x_limits <- scale_y_continuous(expand = c(0.05,0.15), name = "")
    } else {
      x_limits <- scale_y_continuous(expand = c(0.05,0.15), name = "", limits = min_max)
    }

    if (is.null(vnames)) vnames <- x$variable

    pl <- pl + x_limits +
      scale_x_continuous(labels = vnames, breaks = x$position + 0.5, name = "") +
      scale_fill_manual(values = vcolors)
  }

  # add theme
   pl + coord_flip() + DALEX::theme_vertical_default_dalex() +
     theme(legend.position = "none") +
     labs(title = title, subtitle = subtitle)
}

# break down plot with distributions
plot_break_down_distributions <- function(df, vorder = NULL) {
  variable  <- prediction <- id <- NULL
  if (!is.null(vorder)) {
    df$variable <- factor(df$variable, levels = unique(vorder))
  }

  ggplot(df, aes(variable, prediction, group = factor(variable))) +
    geom_line(aes(group = id), alpha = 0.01) +
    geom_violin(scale = "width", adjust = 3) +
    stat_summary(fun.y = "mean", colour = "red", size = 4, geom = "point") +
    xlab("") + ylab("") +
    facet_wrap(~label, scales = "free_y", ncol = 1)
}

# prepare data for plot
prepare_data_for_break_down_plot <- function(x, baseline, rounding_function, digits) {
  x$sign[x$variable_name == ""] <- "X"
  x$sign[x$variable == "intercept"] <- "X"
  x$prev <- x$cumulative - x$contribution
  broken_baseline <- x[x$variable_name == "intercept",]
  x$text <- x$prev
  if (is.na(baseline)) {
    for (lab in broken_baseline$label) {
      x[x$label == lab & x$variable == "prediction", "prev"] <-
        broken_baseline[broken_baseline$label == lab, "contribution"]
      x[x$label == lab & x$variable == "intercept", "prev"] <-
        broken_baseline[broken_baseline$label == lab, "contribution"]
    }
  } else {
    broken_baseline$contribution <- baseline
    x[x$variable == "prediction", "prev"] <- baseline
    x[x$variable == "intercept", "prev"] <- baseline
  }

  x$trans_contribution <- x$cumulative - x$text
  x$right_side <- pmax(x$cumulative,  x$cumulative - x$contribution)

  pretty_trans_contribution <- as.character(rounding_function(x$trans_contribution, digits))
  x$pretty_text <-
    paste0(ifelse((substr(pretty_trans_contribution, 1, 1) == "-") |
                    (x$variable == "prediction") |
                    (x$variable == "intercept"), "", "+"), pretty_trans_contribution)

  list(x = x, broken_baseline = broken_baseline)
}


select_only_k_features <- function(x, k = 10) {
  # filter-out redundant rows
  contribution_sum <- tapply(x$contribution, x$variable_name, function(contribution) sum(abs(contribution), na.rm = TRUE))
  contribution_ordered_vars <- names(sort(contribution_sum[!(names(contribution_sum) %in% c("", "intercept"))]))
  variables_keep <- tail(contribution_ordered_vars, k)
  variables_remove <- setdiff(contribution_ordered_vars, variables_keep)

  if (length(variables_remove) > 0) {
    x_remove   <- x[x$variable_name %in% variables_remove,]
    x_keep     <- x[!(x$variable_name %in% c(variables_remove, "")),]
    x_prediction <- x[x$variable == "prediction",]
    row.names(x_prediction) <- x_prediction$label
    remainings <- tapply(x_remove$contribution, x_remove$label, sum, na.rm=TRUE)
    # fix position and cumulative in x_keep
    x_keep$position <- as.numeric(as.factor(x_keep$position)) + 2
    for (i in 1:nrow(x_keep)) {
      if (x_keep[i,"variable_name"] == "intercept") {
        x_keep[i,"cumulative"] <- x_keep[i,"contribution"]
      } else {
        x_keep[i,"cumulative"] <- x_keep[i - 1,"cumulative"] + x_keep[i,"contribution"]
      }
    }
    # for each model we shall calculate the others statistic
    x_others <- data.frame(variable = "+ all other factors",
               contribution = remainings,
               variable_name = "+ all other factors",
               variable_value = "",
               cumulative = x_prediction[names(remainings),"cumulative"],
               sign = sign(remainings),
               position = 2,
               label = names(remainings))
    #
    x <- rbind(x_keep, x_others, x_prediction)
  }

  x
}

