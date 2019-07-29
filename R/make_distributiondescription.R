#'  Describes distribution of a model pedictions.
#'
#' @param explainer iBreakDown explainer
#' @param display_numbers TRUE for displaying variable contributions and intercept
#' @param display_distribution_details TRUE for displaying detailed description of predictions distribution
#' @param model_name name of the model to be explained
#'
#'
#' @return a decription of predictions distribution
#'
#'
#'
#'



describe_distribution <- function(explainer,
                                  display_numbers,
                                  display_distribution_details,
                                  model_name) {


  yhats_distribution <- attr(explainer, "yhats_distribution")
  model_predictions <- yhats_distribution[which(yhats_distribution$variable == "all data"),]$prediction
  prediction <- round(explainer[dim(explainer)[1],'contribution'],3)
  intercept <- round(explainer$contribution[1],3)
  median_prediction <- round(median(model_predictions),3)

  if (!display_distribution_details) {

      quantiles <- quantile(model_predictions, seq(0,1, by = 0.01))

      if (prediction > median_prediction) {
        place <- names(which(quantiles < prediction)[length(which(quantiles < prediction))])
        description <- paste0("For the selected instance model's prediction is higher, than for ", place," of all observations.")
      }
      if (prediction == median_prediction) {
        description <- paste0("For the selected instance model's prediction is the median prediction.")
      }
      if (prediction < median_prediction) {
        place <- names(which(quantiles < prediction)[length(which(quantiles < prediction))])
        place <- 100 - as.numeric(unlist(strsplit(place, split = "%"))) # Here we revert the quantile
        place <- paste0(place, "%")

        description <- paste0("For the selected instance model's prediction is lower, than for ", place," of all observations.")
      }
  } else {
    mean <- round(mean(model_predictions),3)
    median <- round(median(model_predictions),3)
    sd <- round(sd(model_predictions),3)
    if ((((median - mean)*3)/sd) < -0.5) skeweness <- "right-skewed"
    if ((((median - mean)*3)/sd) > 0.5) skeweness <- "left-skewed"
    if (abs(((median - mean)*3)/sd) < 0.5) skeweness <- "central"

    predictions_quartile <- quantile(model_predictions)
    quartile <- which(predictions_quartile < prediction)[length(which(predictions_quartile < prediction))][[1]]
    quartile_which <- if (quartile == 1) "first" else if (quartile == 2) "second" else if (quartile == 3) "third" else "fourth"

    description <- paste0("Model predictions ranges from ", round(min(model_predictions),3), " to ", round(max(model_predictions), 3), ". The distribution of ", model_name, "'s predictions is ", skeweness, " with average equal to ", mean , " and median equal to ", median, ". The standard deviation is ", sd, ". Model's prediction for the selected instance is in the ", quartile_which, " quartile.")
  }
  description
}
