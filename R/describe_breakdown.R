#' Natural language description of a DALEX explainer
#'
#'
#' @description  Generic function \code{describe} generates a natural language description of
#' \code{break_down()} and \code{break_down_uncertainity()}explanations,
#' which enchaces their interpretability.
#'
#' @details Function \code{describe.break_down()} first selects one of four different description
#' scenarios. Depending on \code{nonsignificance_treshold} which selects variables with
#' significant contribution to model's final prediction, the scenario can be one of the following:
#' 1. Model's prediction for the selected instance is significantly higher than the average prediction.
#' 2. Model's prediction is significantly lower.
#' 3. Model's prediction is close to it's average prediction, however there are significant
#' variables counteracting with each other
#' 4. Model's prediction is close to it's average prediction and all the variables are rather nonsignificant.
#' For each scenario, a template is choosen. Currently only the defalut \code{argumentation_mode} can be choosen, which
#' chooses three most significant variables and dicribes them.
#'
#' @details Arguments \code{display_values}, which shows values of the variables, and
#' \code{display_numbers}, which allows for displaying numerical values of prediciton changes, controls
#' the complexity of the generated description. Additional parameter \code{short_description} allows for generating short description,
#' while \code{display_distribution_details} displays additional details of the distribution of
#' model's prediction for all instances, if describing a break_down explanation. Currently only the default
#' mode of argumentation is supported.
#'
#' @param explainer a DALEX explainer
#' @param nonsignificance_treshold a parameter specifying a treshold for variable importance
#' @param ... other arguments
#' @param label a short description describing model's prediction
#' @param short_description allows for generating short description
#' @param display_values allows for displaying variable values
#' @param display_numbers allows for displaying numerical values
#' @param display_distribution_details displays detailed description of predictions distribution
#' @param display_argumentation choosing the type of argumentation to be displayed
#' @param display_shap displays additional information about average contribution. Use only if the explainer is a shap explainer
#'
#' @return A string of natural language description of an explainer
#'
#' @importFrom graphics plot
#' @importFrom stats quantile aggregate sd
#'
#' @examples
#' library("DALEX")
#' library("randomForest")
#' library("iBreakDown")
#'
#' titanic <- na.omit(titanic)
#' model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                   fare + sibsp + parch,  data = titanic)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic[,-9],
#'                               y = titanic$survived == "yes",
#'                               label = "Random Forest v7")
#' set.seed(1234)
#' random_passanger <- titanic[sample(nrow(titanic),1),c(1,2,3,4,6,7,8)]
#' rf_la <- break_down(explain_titanic_rf, random_passanger, keep_distributions = TRUE)
#'
#' description <- describe(rf_la,
#'                         label = "the passanger will survive with probability",
#'                         short_description = FALSE,
#'                         display_values =  TRUE,
#'                         display_numbers = TRUE,
#'                         display_distribution_details = FALSE,
#'                         display_argumentation = 1)
#'
#' description
#'
#'
#'
#' @export
#' @rdname describe

describe <- function(explainer, nonsignificance_treshold = 0.15, ...)
  UseMethod("describe")

#' @export
#' @rdname describe

describe.break_down <- function(explainer, nonsignificance_treshold = 0.15, ...,
                                label = NULL,
                                short_description = FALSE,
                                display_values = FALSE,
                                display_numbers = FALSE,
                                display_distribution_details = FALSE,
                                display_argumentation = 1,
                                display_shap = FALSE
                                ) {

  # Error handling

  if (!( class(display_values) == 'logical' &
         class(display_numbers) == 'logical' &
         class(short_description) == 'logical' &
         class(display_distribution_details) == 'logical' &
         class(display_argumentation) == 'numeric' &
         class(nonsignificance_treshold) == 'numeric' &
         class(display_shap) == 'logical')) {
    stop("Arguments are not valid")
  }

  model_name <- as.character(explainer$label[1])
  model_name <- paste(toupper(substr(model_name, 1, 1)), substr(model_name, 2, nchar(model_name)), sep="")

  description_profile <- description_profile(explainer, nonsignificance_treshold) # We determine the description scenario

  if (is.null(attr(explainer, "yhats_distribution"))) {
    distribution_kept <- FALSE
  } else {
    distribution_kept <- TRUE
  }

  if (is.null(label)) {
    label = "the prediction for the selected instance is"
  }

  if (short_description) {
    descriptions <- make_short_description(explainer, display_values, label, model_name, description_profile)
  } else {

    # Making an introduction
    introduction <- make_introduction(explainer,
                                     label,
                                     display_numbers,
                                     display_distribution_details,
                                     model_name,
                                     distribution_kept,
                                     description_profile)

    # Making argumentation
    argumentation <- make_argument(explainer,
                                   label, display_values,
                                   display_numbers,
                                   display_argumentation,
                                   model_name,
                                   description_profile,
                                   display_shap)

    summary <- make_summary(explainer, display_argumentation)

    descriptions <- paste0(introduction,"\n \n",argumentation, "\n \n",summary)
  }
  class(descriptions) <- "descriptions"
  descriptions
}

