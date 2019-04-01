#' Explanation Level Uncertainty of Sequential Variable Attribution
#'
#' This function calculated uncertainty that comes from ordering in Sequential Variable Attribution methods.
#' It runs `break_down` algorithm B times with random ordering of variables and then we calculate the uncertanity of attributions that comes from the ordering.
#'
#' @param x a model to be explained, or an explainer created with function `DALEX::explain()`.
#' @param data validation dataset, will be extracted from `x` if it is an explainer.
#' @param predict_function predict function, will be extracted from `x` if it is an explainer.
#' @param new_observation a new observation with columns that correspond to variables used in the model.
#' @param ... other parameters.
#' @param B number of random paths
#' @param path if specified, then this path will be highlighed on the plot
#' @param label name of the model. By default it's extracted from the 'class' attribute of the model.
#'
#' @return an object of the `break_down_uncertainty` class.
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
#' bd_rf <- local_attributions_uncertainty(explain_titanic_glm, titanic_small[1, ])
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
#'                         data = HR[1:1000,1:5])
#'
#' bd_rf <- local_attributions_uncertainty(explainer_rf,
#'                            new_observation)
#' bd_rf
#' plot(bd_rf)
#'
#' # example for regression - apartment prices
#' # here we do not have intreactions
#' model <- randomForest(m2.price ~ . , data = apartments)
#' explainer_rf <- explain(model,
#'                         data = apartments_test[1:1000,2:6],
#'                         y = apartments_test$m2.price[1:1000])
#'
#' bd_rf <- local_attributions_uncertainty(explainer_rf, apartments_test[1,])
#' bd_rf
#' plot(bd_rf)
#' }
#' @export
#' @rdname local_attributions_uncertainty
local_attributions_uncertainty <- function(x, ..., B = 10)
  UseMethod("local_attributions_uncertainty")

#' @export
#' @rdname local_attributions_uncertainty
local_attributions_uncertainty.explainer <- function(x, new_observation,
                       ..., B = 10) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  local_attributions_uncertainty.default(model, data, predict_function,
                     new_observation = new_observation,
                     label = label,
                     ..., B = B)
}

#' @export
#' @rdname local_attributions_uncertainty
local_attributions_uncertainty.default <- function(x, data, predict_function = predict,
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
    tmp <- get_single_random_path(x, data, predict_function, new_observation, label, path)
    tmp$B <- 0
    result <- c(result, list(tmp))
  }

  result <- do.call(rbind, result)

  class(result) <- c("break_down_uncertainty", "data.frame")

  result
}

get_single_random_path <- function(x, data, predict_function, new_observation, label, random_path) {
  # if predict_function returns a single vector, convet it to a data frame
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
