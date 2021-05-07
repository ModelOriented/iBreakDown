#' Explanation Level Uncertainty of Sequential Variable Attribution
#'
#' This function calculates the break down algorithm for \code{B} random orderings.
#' Then it calculates the distribution of attributions for these different orderings.
#' Note that the \code{shap()} function is just a simplified interface to the \code{break_down_uncertainty()} function
#' with a default value set to \code{B=25}.
#'
#' @param x an explainer created with function \code{\link[DALEX]{explain}} or a model.
#' @param data validation dataset, will be extracted from \code{x} if it is an explainer.
#' @param predict_function predict function, will be extracted from \code{x} if it is an explainer.
#' @param new_observation a new observation with columns that correspond to variables used in the model.
#' @param ... other parameters.
#' @param B number of random paths
#' @param keep_distributions if \code{TRUE} then we will keep distribution for predicted values. It's needed by the describe function.
#' @param path if specified, then this path will be highlighed on the plot. Use \code{average} in order to show an average effect
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model.
#'
#' @return an object of the \code{break_down_uncertainty} class.
#' @importFrom utils head
#'
#' @seealso \code{\link{break_down}}, \code{\link{local_attributions}}
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
#'                                data = titanic_imputed,
#'                                y = titanic_imputed$survived,
#'                            label = "glm")
#'
#' # there is no explanation level uncertanity linked with additive models
#' bd_glm <- break_down_uncertainty(explain_titanic_glm, titanic_imputed[1, ])
#' bd_glm
#' plot(bd_glm)
#'
#' \dontrun{
#' ## Not run:
#' library("randomForest")
#' set.seed(1313)
#' model <- randomForest(status ~ . , data = HR)
#' new_observation <- HR_test[1,]
#'
#' explainer_rf <- explain(model,
#'                         data = HR[1:1000, 1:5])
#'
#' bd_rf <- break_down_uncertainty(explainer_rf,
#'                            new_observation)
#' bd_rf
#' plot(bd_rf)
#'
#' # example for regression - apartment prices
#' # here we do not have intreactions
#' model <- randomForest(m2.price ~ . , data = apartments)
#' explainer_rf <- explain(model,
#'                         data = apartments_test[1:1000, 2:6],
#'                         y = apartments_test$m2.price[1:1000])
#'
#' bd_rf <- break_down_uncertainty(explainer_rf, apartments_test[1,])
#' bd_rf
#' plot(bd_rf)
#'
#' bd_rf <- break_down_uncertainty(explainer_rf, apartments_test[1,], path = 1:5)
#' plot(bd_rf)
#'
#' bd_rf <- break_down_uncertainty(explainer_rf,
#'                                      apartments_test[1,],
#'                                      path = c("floor", "no.rooms", "district",
#'                                          "construction.year", "surface"))
#' plot(bd_rf)
#'
#' bd <- break_down(explainer_rf,
#'                     apartments_test[1,])
#' plot(bd)
#'
#' s <- shap(explainer_rf,
#'                    apartments_test[1,])
#' plot(s)
#' }
#' @export
#' @rdname break_down_uncertainty
break_down_uncertainty <- function(x, ...,
                                   keep_distributions = TRUE,
                                   B = 10)
  UseMethod("break_down_uncertainty")

#' @export
#' @rdname break_down_uncertainty
break_down_uncertainty.explainer <- function(x, new_observation,
                       ...,
                       keep_distributions = TRUE,
                       B = 10) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  break_down_uncertainty.default(model, data, predict_function,
                     new_observation = new_observation,
                     label = label,
                     ...,
                     keep_distributions = keep_distributions,
                     B = B)
}

#' @export
#' @rdname break_down_uncertainty
break_down_uncertainty.default <- function(x, data, predict_function = predict,
                               new_observation,
                               label = class(x)[1],
                               ...,
                               path = NULL,
                               keep_distributions = TRUE,
                               B = 10) {
  # here one can add model and data and new observation
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[1, common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }

  # Now we know the path, so we can calculate contributions
  # set variable indicators
  # start random path
  p <- ncol(data)
  result <- lapply(1:B, function(b) {
    random_path <- sample(1:p)
    tmp <- get_single_random_path(x, data, predict_function, new_observation, label, random_path)
    tmp$B <- b
    tmp
  })
  # should we add a specific path?
  if (!is.null(path)) {
    # average or selected path
    if (head(path, 1) == "average") {
      # let's calculate an average attribution
      extracted_contributions <- do.call(cbind, lapply(result, function(chunk) {
        chunk[order(chunk$label, chunk$variable), "contribution"]
      }))
      result_average <- result[[1]]
      result_average <- result_average[order(result_average$label, result_average$variable),]
      result_average$contribution <- rowMeans(extracted_contributions)
      result_average$B <- 0
      result_average$sign <- sign(result_average$contribution)
      result <- c(list(result_average), result)
    } else {
      # path is a selected ordering
      tmp <- get_single_random_path(x, data, predict_function, new_observation, label, path)
      tmp$B <- 0
      result <- c(list(tmp), result)
    }
  }

  result <- do.call(rbind, result)

  class(result) <- c("break_down_uncertainty", "data.frame")

  if (keep_distributions) {
    ## this yhats is not calculated like in breakDown
    yhats <- list(NULL)

    yhats_distribution <- calculate_yhats_distribution(x, data, predict_function, label, yhats)

    attr(result, "yhats_distribution") <- yhats_distribution
  }

  target_yhat <- predict_function(x, new_observation)
  yhatpred <- as.data.frame(predict_function(x, data))
  baseline_yhat <- colMeans(yhatpred)

  attr(result, "prediction") <- as.numeric(target_yhat)
  attr(result, "intercept") <- as.numeric(baseline_yhat)

  result
}

get_single_random_path <- function(x, data, predict_function, new_observation, label, random_path) {
  # if predict_function returns a single vector, conrvet it to a data frame
  if (length(unlist(predict_function(x, new_observation))) > 1) {
    predict_function_df <- predict_function
  } else {
    predict_function_df <- function(...) {
      tmp <- as.data.frame(predict_function(...))
      colnames(tmp) = label
      tmp
    }
  }

  vnames <- colnames(data)
  names(vnames) <- vnames
  current_data <- data

  yhats <- list()
  yhats[[1]] <- colMeans(predict_function_df(x, current_data))
  for (i in seq_along(random_path)) {
    candidate <- random_path[i]
    current_data[,candidate] <- new_observation[,candidate]
    yhats[[i + 1]] <- colMeans(predict_function_df(x, current_data))
  }

  diffs <- apply(do.call(rbind, yhats), 2, diff)
  if (is.vector(diffs)) { #93
    diffs <- t(diffs)
  }
  #76
  new_observation_vec <- sapply(as.data.frame(new_observation), nice_format) # same as in BD

  single_cols <- lapply(1:ncol(diffs), function(col) {

    variable_names <- vnames[random_path]
    data.frame(
      variable = paste0(variable_names, " = ",
                       sapply(new_observation_vec[variable_names], as.character)),
      contribution = diffs[,col],
      variable_name = variable_names,
      variable_value = sapply(new_observation_vec[variable_names], as.character),
      sign = sign(diffs[,col]),
      label = ifelse(ncol(diffs) == 1, label, paste(label,colnames(diffs)[col], sep = "."))
    )
  })

  do.call(rbind,single_cols)
}

#' @export
#' @rdname break_down_uncertainty
shap <- function(x, ..., B = 25) {
  ret <- break_down_uncertainty(x, ..., B = B, path = "average")

  class(ret) <- c("shap", class(ret))

  ret
}

