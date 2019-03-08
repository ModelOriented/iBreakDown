#' Model Agnostic Sequential Variable Attributions with Interactions
#'
#' This function implements decomposition of model predictions with identification
#' of interactions.
#' The complexity of this function is O(2*p) for additive models and O(2*p^2) for interactions.
#' This function works in similar way to step-up and step-down greedy approximations,
#' the main difference is that in the first step the order of variables is determined.
#' And in the second step the impact is calculated.
#'
#' @param x a model to be explained, or an explainer created with function `DALEX::explain()`.
#' @param data validation dataset, will be extracted from `x` if it's an explainer
#' @param predict_function predict function, will be extracted from `x` if it's an explainer
#' @param ... other parameters
#' @param interaction_preference a constant that set the preference for interactions. By default `1`. The larger the more frequently intereactions will be presented in explanations
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param keep_distributions if TRUE, then the distribution of partial predictions is stored in addition to the average.
#' @param label character - the name of the model. By default it's extracted from the 'class' attribute of the model
#' @importFrom stats predict
#' @return an object of the `break_down` class
#'
#' @examples
#' \dontrun{
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
#'                  data = HR[1:1000,1:5],
#'                  y = HR$status[1:1000])
#'
#' bd_rf <- local_interactions(explainer_rf,
#'                  new_observation)
#'
#' bd_rf
#' plot(bd_rf)
#'
#' # example for regression - apartment prices
#' # here we do not have intreactions
#' model <- randomForest(m2.price ~ . , data = apartments)
#' explainer_rf <- explain(model,
#'          data = apartments_test[1:1000,2:6],
#'          y = apartments_test$m2.price[1:1000])
#'
#' new_observation <- apartments_test[1,]
#'
#' bd_rf <- local_interactions(explainer_rf,
#'                  new_observation,
#'                  keep_distributions = TRUE)
#'
#' bd_rf
#' plot(bd_rf)
#' plot(bd_rf, plot_distributions = TRUE)
#' }
#' @export
#' @rdname local_interactions
local_interactions <- function(x, ...)
  UseMethod("local_interactions")

#' @export
#' @rdname local_interactions
local_interactions.explainer <- function(x, new_observation,
                                        keep_distributions = FALSE, ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data  <- x$data
  predict_function <- x$predict_function
  label <- x$label

  local_interactions.default(model, data, predict_function,
                            new_observation = new_observation,
                            keep_distributions = keep_distributions,
                            label = label,
                            ...)
}

#' @export
#' @rdname local_interactions
local_interactions.default <- function(x, data, predict_function = predict,
                                       new_observation,
                                       keep_distributions = FALSE,
                                       interaction_preference = 1,
                                       label = class(x)[1], ...) {
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
  target_yhat_all <- predict_function(x, new_observation)

  # how long in the model output
  results_list <- lapply(1:length(target_yhat_all), function(ii) {
    # find decomposition for ith's variable
    single_predict_function <- function(...) {
      tmp <- predict_function(...)
      if (!is.null(dim(tmp))) {
        return(tmp[,ii])
      }
      tmp
    }

    # set target
    target_yhat <- single_predict_function(x, new_observation)
    baseline_yhat <- mean(single_predict_function(x, data))

    # 1d changes
    # how the average would change if single variable is changed
    average_yhats <- unlist(calculate_1d_changes(x, new_observation, data, single_predict_function))
    diffs_1d <- average_yhats - baseline_yhat

    # impact summary for 1d variables
    tmp <- data.frame(diff = diffs_1d,
                      adiff = abs(diffs_1d),
                      diff_norm = diffs_1d,
                      adiff_norm = abs(diffs_1d),
                      ind1 = 1:p,
                      ind2 = NA)
    rownames(tmp) <- gsub(rownames(tmp), pattern = ".yhats", replacement = "")

    inds <- data.frame(ind1 = unlist(lapply(2:p, function(i) i:p)),
                       ind2 = unlist(lapply(2:p, function(i) rep(i - 1, p - i + 1))))

    # 2d changes
    # how the average would change if two variables are changed
    changes <- calculate_2d_changes(x, new_observation, data, single_predict_function, inds, diffs_1d)

    diffs_2d <- changes$average_yhats - baseline_yhat
    diffs_2d_norm <- changes$average_yhats_norm - baseline_yhat

    # impact summary for 2d variables
    # large interaction_preference force to use interactions
    tmp2 <- data.frame(diff = diffs_2d,
                       adiff = abs(diffs_2d) * interaction_preference,
                       diff_norm = diffs_2d_norm,
                       adiff_norm = abs(diffs_2d_norm) * interaction_preference,
                       ind1 = inds$ind1,
                       ind2 = inds$ind2)
    tmp <- rbind(tmp, tmp2)

    # sort impacts and look for most importants elements
    tmp <- tmp[order(tmp$adiff_norm, decreasing = TRUE),]

    # Now we know the path, so we can calculate contributions
    # set variable indicators
    open_variables <- 1:p
    current_data <- data

    step <- 0
    yhats <- NULL
    yhats_mean <- c()
    selected_rows <- c()
    for (i in 1:nrow(tmp)) {
      candidates <- tmp$ind1[i]
      if (!is.na(tmp$ind2[i]))
        candidates[2] <- tmp$ind2[i]
      if (all(candidates %in% open_variables)) {
        # we can add this effect to out path
        current_data[,candidates] <- new_observation[,candidates]
        step <- step + 1
        yhats_pred <- single_predict_function(x, current_data)
        if (keep_distributions) {
          yhats[[step]] <- data.frame(variable_name = paste(colnames(data)[candidates], collapse = ":"),
                                      variable = paste(
                                                    paste(colnames(data)[candidates], collapse = ":"),
                                                    "=",
                                                    nice_pair(new_observation, candidates[1], candidates[2] )),
                                      id = 1:nrow(data),
                                      prediction = yhats_pred)
        }
        yhats_mean[step] <- mean(yhats_pred)
        selected_rows[step] <- i
        open_variables <- setdiff(open_variables, candidates)
      }
    }
    selected <- tmp[selected_rows,]

    # extract values
    selected_values <- sapply(1:nrow(selected), function(i) {
      nice_pair(new_observation, selected$ind1[i], selected$ind2[i] )
    })

    # prepare values
    variable_name  <- c("intercept", rownames(selected), "")
    variable_value <- c("1", selected_values, "")
    variable       <- c("intercept",
                        paste(rownames(selected), "=",  selected_values) ,
                        "prediction")
    cummulative <- c(baseline_yhat, yhats_mean, target_yhat)
    contribution <- c(0, diff(cummulative))
    contribution[1] <- cummulative[1]
    contribution[length(contribution)] <- cummulative[length(contribution)]

    nlabel <- ifelse(length(unlist(target_yhat_all)) > 1, paste0(label, ".", colnames(as.data.frame(target_yhat_all))[ii]), label)

    result <- data.frame(variable = variable,
                         contribution = contribution,
                         variable_name = variable_name,
                         variable_value = variable_value,
                         cummulative = cummulative,
                         sign = factor(c(as.character(sign(contribution)[-length(contribution)]), "X"), levels = c("-1", "0", "1", "X")),
                         position = (step + 2):1,
                         label = nlabel)

    yhats_distribution <- NULL
    if (keep_distributions) {
      yhats0 <- data.frame(variable_name = "all data",
                           variable = "all data",
                           id = 1:nrow(data),
                           prediction = single_predict_function(x, data)
      )

      yhats_distribution <- cbind(rbind(yhats0, do.call(rbind, yhats)), label = nlabel)
    }

    list(result, yhats_distribution)
  })

  # merge results for all classess
  results <- do.call(rbind, lapply(results_list, function(x) x[[1]]))
  results$position <- seq_along(results$position)
  class(results) <- "break_down"
  attr(results, "baseline") <- 0

  if (keep_distributions) {
    yhats_distribution <- do.call(rbind, lapply(results_list, function(x) x[[2]]))
    attr(results, "yhats_distribution") = yhats_distribution
  }

  results
}


