#' Natural language description of shap explainer
#'
#' @details Function \code{describe.break_down_uncertainty()} generates a natural language description
#' for a shap explanation. It imitates the \code{describe.break_down()}, the only difference is that contributions
#' are average contributions of iterated break_down contributions. Argument `display_shap` allows for
#' average's dispersion description.
#'
#' @examples
#'
#'library("DALEX")
#'library("iBreakDown")
#' titanic <- na.omit(titanic)
#' model_titanic_glm <- glm(titanic$survived == "yes" ~ age + gender + class + fare + sibsp,
#'                          data = titanic[ ,-9], family = "binomial")
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                               data = titanic[,-9],
#'                               y = titanic$survived == "yes",
#'                               label = "glm")
#' passanger <- titanic[sample(nrow(titanic), 1) ,-9]
#' shap_glm <- shap(explain_titanic_glm, passanger)
#' plot(shap_glm)
#'
#' describe(shap_glm,
#'          label = "the selected passanger survives with probability",
#'          display_shap = TRUE,
#'          display_numbers = TRUE)
#'
#'
#' @importFrom graphics plot
#' @importFrom stats quantile
#'
#'
#' @export
#' @rdname describe

describe.break_down_uncertainty <- function(explainer, nonsignificance_treshold = 0.15, ...,
                                            label = NULL,
                                            short_description = FALSE,
                                            display_values = FALSE,
                                            display_numbers = FALSE,
                                            display_distribution_details = FALSE,
                                            display_argumentation = 1,
                                            display_shap = FALSE) {

  explainer_break_down <- convert_explainer(explainer)
  describe(explainer = explainer_break_down,
           nonsignificance_treshold = nonsignificance_treshold,
           ...,
           label = label,
           short_description = short_description,
           display_values = display_values,
           display_numbers = display_numbers,
           display_distribution_details = display_distribution_details,
           display_argumentation = display_argumentation,
           display_shap = display_shap)
}


