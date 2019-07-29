#' Makes a short description of iBreakDown explainer
#'
#' @param explainer iBreakDown explainer
#' @param display_values if TRUE then displays variable valeus
#' @param label prediction label
#' @param model_name name of the model to be explained
#' @param description_profile a scenario for description
#'
#'


make_short_description <- function(explainer, display_values, label, model_name, description_profile) {
  # Returns a short description of a break_down explanation

  intercept <- round(explainer$contribution[1],3)
  prediction <- round(explainer$contribution[length(explainer$contribution)],3)
  most_important_variable <- explainer[2, ]
  most_important_contribution <- most_important_variable$contribution

  # We choose the most informative description depending on possible cases:
  # Case 1: prediction is highly positive or negative and the most important variables sign
  #         follows the predictions sign

  sign_coherence <- if ((prediction - intercept) * most_important_contribution > 0) TRUE else FALSE

  if (description_profile %in% 0:1 & sign_coherence) {
    sign <- if (most_important_contribution > 0) "increases" else "decreases"
    values <- if (display_values) paste0(" (= ",as.character(most_important_variable$variable_value),")") else ""

    short_description <- paste0(model_name," predicts, that ", label," ", prediction,
                                ". The most important variable, which ",sign," model's prediction, is ",
                                most_important_variable$variable_name,values,".")
  } else {
    # Case 2: the explanation is to complex to be put in one sentece. Short_description is stil
    # important as it points to an element of the plot which is on the bottom of it.
    # (people tend to read from above)
    short_description <- paste0(model_name," predicts, that ", label," ", prediction,".")
  }
  short_description
}
