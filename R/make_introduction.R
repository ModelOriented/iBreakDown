#' Makes an introduction for the description function
#'
#' @param explainer an iBreakDown explainer
#' @param label prediction description
#' @param display_numbers TRUE for displaying variable contributions and intercept
#' @param distribution_details TRUE for displaying detailed description of predictions distribution
#' @param model_name name of the model to be explained
#' @param distribution_kept TRUE if the distribution details are stored
#' @param description_profile description scenario
#'
#'
#'


make_introduction <- function(explainer, label, display_numbers, distribution_details, model_name, distribution_kept, description_profile){
  # Makes an introduction

  intercept <- round(explainer$contribution[1],3)
  prediction <- round(explainer$contribution[length(explainer$contribution)],3)
  contributions <- explainer$contribution[-c(1,length(explainer$contribution))]

  numbers <- if (display_numbers) paste0(" ",as.character(intercept)) else ""

    if (description_profile == 0) {
      #introduction <- glue("{model_name} predicts, that {label} {prediction}, which is higher than the average model prediction{numbers}.")
      introduction <- paste0({model_name}," predicts, that ",label," ",prediction," which is higher than the average model prediction",numbers,".")
      }
      if (description_profile == 1) {
        #introduction <- glue("{model_name} predicts, that {label} {prediction}, which is lower than the average model prediction{numbers}.")
        introduction <- paste0({model_name}," predicts, that ", label," ", prediction," which is lower than the average model prediction",numbers,".")
      }
      if (description_profile == 2 | description_profile == 3) {
        #introduction <- glue("{model_name} predicts, that {label} {prediction}, which is close to the average model prediction{numbers}.")
        introduction <- paste0({model_name}," predicts, that ", label," ", prediction," which is close to the average model prediction",numbers,".")
      }

      if (distribution_kept) {
        distribution_description <- describe_distribution(explainer,
                                                          display_numbers,
                                                          distribution_details,
                                                          model_name)

        introduction <- paste(introduction, distribution_description)
      }

introduction

}
