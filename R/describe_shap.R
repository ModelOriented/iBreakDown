#' Natural language description of a shap explanation
#'
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

describe.break_down_uncertainty <- function(x,
                                            nonsignificance_treshold = 0.15,
                                            ...,
                                            label = NULL,
                                            short_description = FALSE,
                                            display_values = FALSE,
                                            display_numbers = FALSE,
                                            display_distribution_details = FALSE,
                                            display_shap = FALSE) {

  explanation_break_down <- convert_explanation.break_down_uncertainty(x)
  describe(x = explanation_break_down,
           nonsignificance_treshold = nonsignificance_treshold,
           label = label,
           short_description = short_description,
           display_values = display_values,
           display_numbers = display_numbers,
           display_distribution_details = display_distribution_details,
           display_shap = display_shap)
}

# Converts a `break_down_uncertainty` to `break_down` explanation
#
# Called by `r describe.break_down_uncertainty`.

convert_explanation.break_down_uncertainty <- function(x) {
  # We transform explanations's data frame
  df <- x[which(x$B == 0), -ncol(x)]
  df <- df[order(abs(df$contribution), decreasing = TRUE), ]
  sign <- if (attr(x, "intercept") >= 0) 1 else -1
  df_intercept <- data.frame(variable = "intercept",
                             contribution = attr(x, "intercept"),
                             variable_name = "intercept",
                             variable_value = attr(x, "intercept"),
                             sign = sign,
                             label = x$label[1])

  df_prediction <- data.frame(variable = "prediction",
                              contribution = attr(x, "prediction"),
                              variable_name = "prediction",
                              variable_value = attr(x, "prediction"),
                              sign = if (attr(x, "prediction") >= 0) 1 else -1,
                              label = x$label[1])
  df <- rbind(df_intercept, df, df_prediction)
  position <- c(1:nrow(df))
  df['position'] <- position
  df['cummulative'] <- cumsum(df$contribution)
  df_break_down <- data.frame(variable = df['variable'],
                              contribution = df['contribution'],
                              variable_name = df['variable_name'],
                              variable_value = df["variable_value"],
                              cummulative = df['cummulative'],
                              sign = df["sign"],
                              position = df["position"],
                              label = df["label"])

  # We add information about boxplots
  variables <- as.list(unique(as.character(x$variable_name)))
  shap_contributions <- lapply(variables, function(variable) {
    x[x$variable_name == variable & x$B > 0, 'contribution']
  })
  names(shap_contributions) <- variables
  attr(df_break_down, "shap_contributions") <- shap_contributions
  # We keep yhats_distribution
  attr(df_break_down, 'yhats_distribution') <- attr(x, 'yhats_distribution')
  class(df_break_down) <- c("break_down", "data.frame")
  df_break_down
}

