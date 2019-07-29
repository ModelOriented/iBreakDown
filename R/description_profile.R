#' Returns a scenario for a description
#'
#' 0 = prediction significantly higher than the average
#' 1 = prediction significantly lower than the average
#' 2 = prediction close to the average, but there are significant contributors
#' 3 = prediction close to the average and there are no significant contributors
#'
#'
#' @param explainer an iBreakDown explainer
#' @param nonsignificance_treshold a treshold for specyfiying which predictions are close to the
#'                                 average model prediction. A prediction is significantly higher/lower
#'                                 than the average prediction if
#'                                 (average prediction - prediction)/(average prediction)
#'                                 is higher than nonsignificance_treshold. It also has effect
#'                                 on when a variables impact is considered as unimportant.
#'
#' @return an integer from 0 to 3
#'

description_profile <- function(explainer,
                                nonsignificance_treshold = 0.15){

  model_intercept <- round(explainer$contribution[1],3)
  model_prediction <- round(explainer$contribution[length(explainer$contribution)],3)
  model_contributions <- explainer$contribution[-c(1,length(explainer$contribution))]
  distance <- abs(model_intercept - model_prediction)

  if (distance/model_intercept > nonsignificance_treshold) {
    if (model_intercept < model_prediction) {
      profile <- 0
    } else {
      profile <- 1
    }
  } else {
    if (sort(model_contributions, decreasing = TRUE)[1] > nonsignificance_treshold*10*distance){
      profile <- 2
    } else {
      profile <- 3
    }
  }
  profile
}
