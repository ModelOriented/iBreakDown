#' Model Agnostic Sequential Variable attributions
#'
#' This function finds Variable attributions via Sequential Variable Conditioning.
#' The complexity of this function is O(2*p).
#' This function works in a similar way to step-up and step-down greedy approximations in function \code{\link{break_down}}.
#' The main difference is that in the first step the order of variables is determined.
#' And in the second step the impact is calculated.
#'
#' @param x an explainer created with function \code{\link[DALEX]{explain}} or a model.
#' @param data validation dataset, will be extracted from \code{x} if it is an explainer.
#' @param predict_function predict function, will be extracted from \code{x} if it is an explainer.
#' @param new_observation a new observation with columns that correspond to variables used in the model.
#' @param keep_distributions if \code{TRUE}, then distribution of partial predictions is stored and can be plotted with the generic \code{plot()}.
#' @param order if not \code{NULL}, then it will be a fixed order of variables. It can be a numeric vector or vector with names of variables.
#' @param ... other parameters.
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model.
#'
#' @return an object of the \code{break_down} class.
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://pbiecek.github.io/ema}
#'
#' @seealso \code{\link{break_down}}, \code{\link{local_interactions}}
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
#' bd_glm <- local_attributions(explain_titanic_glm, titanic_imputed[1, ])
#' bd_glm
#' plot(bd_glm, max_features = 3)
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
#'
#' # example for regression - apartment prices
#' # here we do not have interactions
#' model <- randomForest(m2.price ~ . , data = apartments)
#' explainer_rf <- explain(model,
#'                         data = apartments_test[1:1000,2:6],
#'                         y = apartments_test$m2.price[1:1000])
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            apartments_test[1,])
#' bd_rf
#' plot(bd_rf, digits = 1)
#'
#' bd_rf <- local_attributions(explainer_rf,
#'                            apartments_test[1,],
#'                            keep_distributions = TRUE)
#' plot(bd_rf, plot_distributions = TRUE)
#' }
#' @export
#' @rdname local_attributions
local_attributions <- function(x, ...)
  UseMethod("local_attributions")

#' @export
#' @rdname local_attributions
local_attributions.explainer <- function(x, new_observation,
                                         keep_distributions = FALSE, ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  local_attributions.default(model, data, predict_function,
                             new_observation = new_observation,
                             label = label,
                             keep_distributions = keep_distributions,
                             ...)
}

#' @export
#' @rdname local_attributions
local_attributions.default <- function(x, data, predict_function = predict,
                                       new_observation,
                                       label = class(x)[1],
                                       keep_distributions = FALSE,
                                       order = NULL,
                                       ...) {
  # here one can add model and data and new observation
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[1, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }

  #
  # just in case the return has more columns
  # set target
  target_yhat <- predict_function(x, new_observation)
  yhatpred <- as.data.frame(predict_function(x, data))
  baseline_yhat <- colMeans(yhatpred)
  # 1d changes
  # how the average would change if single variable is changed
  average_yhats <- calculate_1d_changes(x, new_observation, data, predict_function)
  diffs_1d <- sapply(seq_along(average_yhats), function(i) {
    sqrt(mean((average_yhats[[i]] - baseline_yhat)^2))
  })

  # impact summary for 1d variables
  # prepare ordered path of features
  feature_path <- create_ordered_path(diffs_1d, order, names(average_yhats))


  # Now we know the path, so we can calculate contributions
  # set variable indicators
  tmp <- calculate_contributions_along_path(x, data, new_observation, feature_path, predict_function, keep_distributions, label, baseline_yhat, target_yhat)
  contribution <- tmp$contribution
  variable_name <- tmp$variable_name
  variable_value <- tmp$variable_value
  variable <- tmp$variable
  yhats <- tmp$yhats
  cumulative <- tmp$cumulative

  # setup labels
  label_class <- label
  if (ncol(as.data.frame(target_yhat)) > 1) {
    label_class <- paste0(label, ".",
                          rep(colnames(as.data.frame(target_yhat)),
                              each = length(variable)))
  }

  result <- data.frame(variable = variable,
                       contribution = c(contribution),
                       variable_name = variable_name,
                       variable_value = variable_value,
                       cumulative = c(cumulative),
                       sign = factor(c(as.character(sign(contribution)[-length(contribution)]), "X"), levels = c("-1", "0", "1", "X")),
                       position = length(variable):1,
                       label = label_class)

  class(result) <- c("break_down", "data.frame")
  if (keep_distributions) {
    yhats_distribution <- calculate_yhats_distribution(x, data, predict_function, label, yhats)

    attr(result, "yhats_distribution") <- yhats_distribution
  }

  result
}


# helper functions

# if we need to add contributions
calculate_yhats_distribution <- function(x, data, predict_function, label, yhats) {
  allpredictions <- as.data.frame(predict_function(x, data))
  predictions_for_batch <- lapply(1:ncol(allpredictions), function(j) {
    data.frame(variable_name = "all data",
               variable = "all data",
               id = 1:nrow(data),
               prediction = allpredictions[,j],
               label = ifelse(ncol(allpredictions) > 1,
                              paste0(label, ".", colnames(allpredictions)[j]),
                              label)
    )
  })
  yhats0 <- do.call(rbind, predictions_for_batch)

  rbind(yhats0, do.call(rbind, yhats))
}

# Now we know the path, so we can calculate contributions
# set variable indicators
calculate_contributions_along_path <- function(x,
                                               data,
                                               new_observation,
                                               feature_path,
                                               predict_function,
                                               keep_distributions,
                                               label,
                                               baseline_yhat,
                                               target_yhat) {
  p <- ncol(data)
  open_variables <- 1:p
  current_data <- data

  step <- 0
  yhats <- NULL
  yhats_mean <- list()
  selected_rows <- c()
  for (i in 1:nrow(feature_path)) {
    candidates <- feature_path$ind1[i]
    if (all(candidates %in% open_variables)) {
      # we can add this effect to our path
      current_data[,candidates] <- new_observation[,candidates]
      step <- step + 1
      yhats_pred <- data.frame(predict_function(x, current_data))
      if (keep_distributions) {
        distribution_for_batch <- lapply(1:ncol(yhats_pred), function(j){
          data.frame(variable_name = paste(colnames(data)[candidates], collapse = ":"),
                     variable = paste0(paste(colnames(data)[candidates], collapse = ":"),
                                       " = ", nice_pair(new_observation, candidates[1], NA )),
                     id = 1:nrow(data),
                     prediction = yhats_pred[,j],
                     label = ifelse(ncol(yhats_pred) > 1,
                                    paste0(label, ".", colnames(yhats_pred)[j]),
                                    label)
                     )
        })
        # setup labels

        yhats[[step]] <- do.call(rbind, distribution_for_batch)
      }
      yhats_mean[[step]] <- colMeans(as.data.frame(yhats_pred))
      selected_rows[step] <- i
      open_variables <- setdiff(open_variables, candidates)
    }
  }
  selected <- feature_path[selected_rows,]

  # extract values
  selected_values <- sapply(1:nrow(selected), function(i) {
    nice_pair(new_observation, selected$ind1[i], NA )
  })

  # prepare values
  variable_name  <- c("intercept", colnames(current_data)[selected$ind1], "")
  variable_value <- c("1", selected_values, "")
  variable       <- c("intercept",
                      paste0(colnames(current_data)[selected$ind1], " = ",  selected_values) ,
                      "prediction")
  cumulative <- do.call(rbind, c(list(baseline_yhat), yhats_mean, list(target_yhat)))
  contribution <- rbind(0,apply(cumulative, 2, diff))
  contribution[1,] <- cumulative[1,]
  contribution[nrow(contribution),] <- cumulative[nrow(contribution),]

  list(variable_name = variable_name,
       variable_value = variable_value,
       variable = variable,
       cumulative = cumulative,
       contribution = contribution,
       yhats = yhats)
}

# created ordered path of features
create_ordered_path <- function(diffs_1d, order, average_yhats_names = NULL) {
  feature_path <- data.frame(diff = diffs_1d,
                             ind1 = seq_along(diffs_1d))
  # how variables shall be ordered in the BD plot?
  if (is.null(order)) {
    # sort impacts and look for most importants elements
    feature_path <- feature_path[order(feature_path$diff, decreasing = TRUE),]
  } else {
    # order is defined by indexes
    if (is.numeric(order)) {
      feature_path <- feature_path[order,]
    }
    # order is defined by names
    if (is.character(order)) {
      rownames(feature_path) <- average_yhats_names
      feature_path <- feature_path[order,]
    }
  }

  feature_path
}

create_ordered_path_2d <- function(feature_path, order, average_yhats_names) {
  if (is.null(order)) {
    # sort impacts and look for most importants elements
    feature_path <- feature_path[order(feature_path$adiff_norm, decreasing = TRUE),]
  } else {
    if (is.numeric(order)) {
      feature_path <- feature_path[order,]
    }
    if (is.character(order)) {
      feature_path <- feature_path[order,]
    }
  }
  feature_path
}


# this formats numbers and factors
# note that in previoiuse version there was signif(x, 2)
# with unexpected side effect for dates signif(1999, 4) = 2000
# signif(x, 4) shall work for dates and also shall be readable in case of ,,angular'' numbers (like pi)
nice_format <- function(x) {
  if (is.numeric(x)) {
    as.character(signif(x, 4))
  } else {
    as.character(x)
  }
}

# this formats pairs of values
nice_pair <- function(x, ind1, ind2) {
  if (is.na(ind2)) {
    nice_format(x[1,ind1])
  } else {
    paste(nice_format(x[1,ind1]), nice_format(x[1,ind2]), sep=":")
  }
}

# 1d changes
# how the average would change if single variable is changed
calculate_1d_changes <- function(model, new_observation, data, predict_function) {
  p <- ncol(data)
  average_yhats <- list()
  for (i in 1:p) {
    current_data <- data
    current_data[,i] <- new_observation[,i]
    yhats <- predict_function(model, current_data)
    average_yhats[[i]] <- colMeans(as.data.frame(yhats))
  }
  names(average_yhats) <- colnames(data)
  average_yhats
}

# 2d changes
# how the average would change if two variables are changed
calculate_2d_changes <- function(model, new_observation, data, predict_function, inds, diffs_1d) {
  average_yhats <- numeric(nrow(inds))
  average_yhats_norm <- numeric(nrow(inds))
  for (i in 1:nrow(inds)) {
    current_data <- data
    current_data[,inds[i, 1]] <- new_observation[,inds[i, 1]]
    current_data[,inds[i, 2]] <- new_observation[,inds[i, 2]]
    yhats <- predict_function(model, current_data)
    average_yhats[i] <- mean(yhats)
    average_yhats_norm[i] <- mean(yhats) - diffs_1d[inds[i, 1]] - diffs_1d[inds[i, 2]]
  }
  names(average_yhats) <- paste(colnames(data)[inds[,1]],
                                colnames(data)[inds[,2]],
                                sep = ":")
  list(average_yhats = average_yhats, average_yhats_norm = average_yhats_norm)
}
