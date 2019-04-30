#' Explanation Level Uncertainty of Sequential Variable Attribution
#'
#' The `break_down_uncertainty()` calles `B` times the break down algorithm for random orderings.
#' Then it calculated distribution of attributions for these different orderings.
#' Note that the `shap()` function is just a simplified interface to the `break_down_uncertainty()` function
#' with by default `B=25` random draws.
#'
#' @param x a model to be explained, or an explainer created with function `DALEX::explain()`.
#' @param data validation dataset, will be extracted from `x` if it is an explainer.
#' @param predict_function predict function, will be extracted from `x` if it is an explainer.
#' @param new_observation a new observation with columns that correspond to variables used in the model.
#' @param ... other parameters.
#' @param B number of random paths
#' @param path if specified, then this path will be highlighed on the plot. Use `average` in order to show an average effect
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model.
#'
#' @return an object of the `break_down_uncertainty` class.
#' @importFrom utils head
#'
#' @seealso \code{\link{break_down}}, \code{\link{local_attributions}}
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @examples
#' library("DALEX")
#' library("iBreakDown")
#' # Toy examples, because CRAN angels ask for them
#' titanic <- na.omit(titanic)
#' set.seed(1313)
#' titanic_small <- titanic[sample(1:nrow(titanic), 500), c(1,2,6,9)]
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic_small, family = "binomial")
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic_small[,-9],
#'                            y = titanic_small$survived == "yes")
#'
#' # there is no explanation level uncertanity linked with additive models
#' bd_rf <- break_down_uncertainty(explain_titanic_glm, titanic_small[1, ])
#' bd_rf
#' plot(bd_rf)
#'
#' \donttest{
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
#' bd_rf <- shap(explainer_rf,
#'               apartments_test[1,])
#' bd_rf
#' plot(bd_rf)
#' plot(bd_rf, show_boxplots = FALSE)
#' }
#' @export
#' @rdname break_down_uncertainty
break_down_uncertainty <- function(x, ..., B = 10)
  UseMethod("break_down_uncertainty")

#' @export
#' @rdname break_down_uncertainty
break_down_uncertainty.explainer <- function(x, new_observation,
                       ..., B = 10) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  break_down_uncertainty.default(model, data, predict_function,
                     new_observation = new_observation,
                     label = label,
                     ..., B = B)
}

#' @export
#' @rdname break_down_uncertainty
break_down_uncertainty.default <- function(x, data, predict_function = predict,
                               new_observation,
                               label = class(x)[1],
                               ...,
                               path = NULL,
                               B = 10) {
  # here one can add model and data and new observation
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[, common_variables, drop = FALSE]
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
      extracted_contributions <- sapply(result, function(chunk) {
        chunk[order(chunk$variable), "contribution"]
      })
      result_average <- result[[1]]
      result_average$contribution <- rowMeans(extracted_contributions)
      result_average$variable <- result_average$variable[order(result_average$variable)]
      result_average$B <- 0
      result <- c(result, list(result_average))
    } else {
      # path is a selected ordering
      tmp <- get_single_random_path(x, data, predict_function, new_observation, label, path)
      tmp$B <- 0
      result <- c(result, list(tmp))
    }
  }

  result <- do.call(rbind, result)

  class(result) <- c("break_down_uncertainty", "data.frame")

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
  single_cols <- lapply(1:ncol(diffs), function(col) {
    data.frame(contribution = diffs[,col],
               label = ifelse(ncol(diffs) == 1, label, paste(label,colnames(diffs)[col], sep = ".")),
               variable = vnames[random_path])
  })

  do.call(rbind,single_cols)
}

#' @export
#' @rdname break_down_uncertainty
shap <- function(x, ..., B = 25) {
  break_down_uncertainty(x, ..., B = B, path = "average")
}

