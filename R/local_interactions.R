#' Model Agnostic Sequential Variable Attributions with Interactions
#'
#' This function implements decomposition of model predictions with identification
#' of interactions.
#' The complexity of this function is O(2*p) for additive models and O(2*p^2) for interactions.
#' This function works in a similar way to step-up and step-down greedy approximations in function \code{break_down()}.
#' The main difference is that in the first step the order of variables and interactions is determined.
#' And in the second step the impact is calculated.
#'
#' @param x an explainer created with function \code{\link[DALEX]{explain}} or a model.
#' @param data validation dataset, will be extracted from \code{x} if it's an explainer.
#' @param predict_function predict function, will be extracted from \code{x} if it's an explainer.
#' @param ... other parameters.
#' @param interaction_preference an integer specifying which interactions will be present in an explanation. The larger the integer, the more frequently interactions will be presented.
#' @param new_observation a new observation with columns that correspond to variables used in the model.
#' @param keep_distributions if \code{TRUE}, then the distribution of partial predictions is stored in addition to the average.
#' @param order if not \code{NULL}, then it will be a fixed order of variables. It can be a numeric vector or vector with names of variables/interactions.
#' @param label character - the name of the model. By default it's extracted from the 'class' attribute of the model.
#'
#' @return an object of the \code{break_down} class.
#'
#' @seealso \code{\link{break_down}}, \code{\link{local_attributions}}
#'
#' @importFrom stats predict
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
#' bd_glm <- local_interactions(explain_titanic_glm, titanic_imputed[1, ],
#'        interaction_preference = 500)
#' bd_glm
#' plot(bd_glm, max_features = 2)
#'
#' \dontrun{
#' library("randomForest")
#' # example with interaction
#' # classification for HR data
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HR_test[1,]
#'
#' explainer_rf <- explain(model,
#'                  data = HR[1:1000,1:5])
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
                            label = label,
                            keep_distributions = keep_distributions,
                            ...)
}

#' @export
#' @rdname local_interactions
local_interactions.default <- function(x, data, predict_function = predict,
                                       new_observation,
                                       label = class(x)[1],
                                       keep_distributions = FALSE,
                                       order = NULL,
                                       interaction_preference = 1,
                                       ...) {
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[1, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }
  p <- ncol(data)

  #
  # just in case the return has more columns
  # set target
  target_yhat_all <- predict_function(x, new_observation)

  # how long is the model output
  # iterate over all targets
  results_list <- lapply(1:length(target_yhat_all), function(selected_target) {
    # find decomposition for ith's variable
    single_predict_function <- function(...) {
      predictions <- predict_function(...)
      if (!is.null(dim(predictions))) {
        return(predictions[selected_target])
      }
      predictions
    }

    # set target
    target_yhat <- single_predict_function(x, new_observation)
    baseline_yhat <- mean(single_predict_function(x, data))

    # 1d changes
    # how the average would change if single variable is changed
    average_yhats <- unlist(calculate_1d_changes(x, new_observation, data, single_predict_function))
    diffs_1d <- average_yhats - baseline_yhat

    # impact summary for 1d variables
    feature_path_1d <- data.frame(diff = diffs_1d,
                      adiff = abs(diffs_1d),
                      diff_norm = diffs_1d,
                      adiff_norm = abs(diffs_1d),
                      ind1 = 1:p,
                      ind2 = NA)
    rownames(feature_path_1d) <- gsub(rownames(feature_path_1d), pattern = ".yhats", replacement = "")

    inds <- data.frame(ind1 = unlist(lapply(2:p, function(i) i:p)),
                       ind2 = unlist(lapply(2:p, function(i) rep(i - 1, p - i + 1))))

    # 2d changes
    # how the average would change if two variables are changed
    changes <- calculate_2d_changes(x, new_observation, data, single_predict_function, inds, diffs_1d)

    diffs_2d <- changes$average_yhats - baseline_yhat
    diffs_2d_norm <- changes$average_yhats_norm - baseline_yhat

    # impact summary for 2d variables
    # large interaction_preference force to use interactions
    feature_path_2d <- data.frame(diff = diffs_2d,
                       adiff = abs(diffs_2d) * interaction_preference,
                       diff_norm = diffs_2d_norm,
                       adiff_norm = abs(diffs_2d_norm) * interaction_preference,
                       ind1 = inds$ind1,
                       ind2 = inds$ind2)
    feature_path <- rbind(feature_path_1d, feature_path_2d)

    # how variables shall be ordered in the BD plot?
    feature_path <- create_ordered_path_2d(feature_path, order, names(average_yhats))


    # Now we know the path, so we can calculate contributions
    # set variable indicators

    tmp <- calculate_contributions_along_path_2d(x, data, new_observation, feature_path, single_predict_function, keep_distributions, label, baseline_yhat, target_yhat)
    contribution <- tmp$contribution
    variable_name <- tmp$variable_name
    variable_value <- tmp$variable_value
    variable <- tmp$variable
    yhats <- tmp$yhats
    cumulative <- tmp$cumulative


    nlabel <- ifelse(length(unlist(target_yhat_all)) > 1,
                     paste0(label, ".", colnames(as.data.frame(target_yhat_all))[selected_target]),
                     label)

    result <- data.frame(variable = variable,
                         contribution = contribution,
                         variable_name = variable_name,
                         variable_value = variable_value,
                         cumulative = cumulative,
                         sign = factor(c(as.character(sign(contribution)[-length(contribution)]), "X"), levels = c("-1", "0", "1", "X")),
                         position = length(variable):1,
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
  results$position <- rev(seq_along(results$position))
  class(results) <- c("break_down", "data.frame")

  if (keep_distributions) {
    yhats_distribution <- do.call(rbind, lapply(results_list, function(x) x[[2]]))
    attr(results, "yhats_distribution") = yhats_distribution
  }

  results
}


# Now we know the path, so we can calculate contributions
# set variable indicators
calculate_contributions_along_path_2d <- function(x, data, new_observation, feature_path, single_predict_function, keep_distributions, label, baseline_yhat, target_yhat) {
  p <- ncol(data)
  open_variables <- 1:p
  current_data <- data

  step <- 0
  yhats <- NULL
  yhats_mean <- c()
  selected_rows <- c()
  for (i in 1:nrow(feature_path)) {
    candidates <- feature_path$ind1[i]
    if (!is.na(feature_path$ind2[i]))
      candidates[2] <- feature_path$ind2[i]
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
  selected <- feature_path[selected_rows,]

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
  cumulative <- c(baseline_yhat, yhats_mean, target_yhat)
  contribution <- c(0, diff(cumulative))
  contribution[1] <- cumulative[1]
  contribution[length(contribution)] <- cumulative[length(contribution)]

  list(variable_name = variable_name,
       variable_value = variable_value,
       variable = variable,
       cumulative = cumulative,
       contribution = contribution,
       yhats = yhats)
}

