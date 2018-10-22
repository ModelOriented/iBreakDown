#' Model Agnostic Sequential Variable attributionss
#'
#' This function finds Variable attributionss via Sequential Variable Conditioning
#' The complexity of this function is O(2*p).
#' This function first determines the order for conditionings and then calculate variable effects via sequence of conditionings.
#'
#' @param x a model to be explained, or an explaienr created with function `DALEX::explain()`.
#' @param data validation dataset, will be extracted from `x` if it's an explainer
#' @param predict_function predict function, will be extracted from `x` if it's an explainer
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param keep_distributions if `TRUE`, then distributions of partial predictions is stored and can be plotted with the generic `plot()`
#' @param ... other parameters
#'
#' @return an object of the `break_down` class
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
                     keep_distributions = keep_distributions,
                     label = label,
                     ...)
}

#' @export
#' @rdname local_attributions
local_attributions.default <- function(x, data, predict_function = predict,
                               new_observation,
                               keep_distributions = FALSE,
                               label = class(x)[1], ...) {
  # here one can add model and data and new observation
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }
  p <- ncol(data)

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
    mean((average_yhats[[i]] - baseline_yhat)^2)
  })
  # impact summary for 1d variables
  tmp <- data.frame(diff = diffs_1d,
                    ind1 = 1:p)
  # sort impacts and look for most importants elements
  tmp <- tmp[order(tmp$diff, decreasing = TRUE),]

  # Now we know the path, so we can calculate contributions
  # set variable indicators
  open_variables <- 1:p
  current_data <- data

  step <- 0
  yhats <- NULL
  yhats_mean <- list()
  selected_rows <- c()
  for (i in 1:nrow(tmp)) {
    candidates <- tmp$ind1[i]
    if (all(candidates %in% open_variables)) {
      # we can add this effect to our path
      current_data[,candidates] <- new_observation[,candidates]
      step <- step + 1
      yhats_pred <- data.frame(predict_function(x, current_data))
      if (keep_distributions) {
        tmpj <- lapply(1:ncol(yhats_pred), function(j){
          data.frame(variable_name = paste(colnames(data)[candidates], collapse = ":"),
                     variable = paste("*",
                                   paste(colnames(data)[candidates], collapse = ":"),
                                   "=",
                                   nice_pair(new_observation, candidates[1], NA )),
                     id = 1:nrow(data),
                     prediction = yhats_pred[,j],
                     label = ifelse(ncol(yhats_pred) > 1, paste0(label, ".", colnames(yhats_pred)[j]), label) )
        })
        # setup labels

        yhats[[step]] <- do.call(rbind, tmpj)
      }
      yhats_mean[[step]] <- colMeans(as.data.frame(yhats_pred))
      selected_rows[step] <- i
      open_variables <- setdiff(open_variables, candidates)
    }
  }
  selected <- tmp[selected_rows,]


  # extract values
  selected_values <- sapply(1:nrow(selected), function(i) {
    nice_pair(new_observation, selected$ind1[i], NA )
  })

  # prepare values
  variable_name  <- c("baseline", colnames(current_data)[selected$ind1], "")
  variable_value <- c("1", selected_values, "")
  variable       <- c("baseline",
                      paste("*", colnames(current_data)[selected$ind1], "=",  selected_values) ,
                      "prediction")
  cummulative <- do.call(rbind, c(list(baseline_yhat), yhats_mean, list(target_yhat)))
  contribution <- rbind(0,apply(cummulative, 2, diff))
  contribution[1,] <- cummulative[1,]
  contribution[nrow(contribution),] <- cummulative[nrow(contribution),]

  # setup labels
  label_class <- label
  if (ncol(as.data.frame(target_yhat)) > 1) {
    label_class <- paste0(label, ".",rep(colnames(as.data.frame(target_yhat)), each = length(variable)))
  }

  result <- data.frame(variable = variable,
                       contribution = c(contribution),
                       variable_name = variable_name,
                       variable_value = variable_value,
                       cummulative = c(cummulative),
                       sign = factor(c(as.character(sign(contribution)[-length(contribution)]), "X"), levels = c("-1", "0", "1", "X")),
                       position = 1:(step + 2),
                       label = label_class)

  class(result) <- "break_down"
  attr(result, "baseline") <- 0
  if (keep_distributions) {
    allpredictions <- as.data.frame(predict_function(x, data))
    tmp <- lapply(1:ncol(allpredictions), function(j) {
      data.frame(variable_name = "all data",
                 variable = "all data",
                 id = 1:nrow(data),
                 prediction = allpredictions[,j],
                 label = ifelse(ncol(allpredictions) > 1, paste0(label, ".", colnames(allpredictions)[j]), label) )
    })
    yhats0 <- do.call(rbind, tmp)

    yhats_distribution <- rbind(yhats0, do.call(rbind, yhats))
    attr(result, "yhats_distribution") = yhats_distribution
  }

  result
}


# helper functions
nice_format <- function(x) {
  if (is.numeric(x)) {
    as.character(signif(x, 2))
  } else {
    as.character(x)
  }
}

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
