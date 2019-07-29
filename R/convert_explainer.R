#' Converts a `break_down_uncertainty` to `break_down` explainer
#'
#' Called by `r describe.break_down_uncertainty`.
#'
#' @param explainer explainer



convert_explainer <- function(explainer)
  UseMethod("convert_explainer")

convert_explainer.break_down_uncertainty <- function(explainer) {
  # We transform explainer's data frame
  df <- explainer[which(explainer$B == 0), -ncol(explainer)]
  df <- df[order(abs(df$contribution), decreasing = TRUE), ]
  sign <- if (attr(explainer, "intercept") >= 0) 1 else -1
  df_intercept <- data.frame(variable = "intercept",
                             contribution = attr(explainer, "intercept"),
                             variable_name = "intercept",
                             variable_value = attr(explainer, "intercept"),
                             sign = sign,
                             label = explainer$label[1])

  df_prediction <- data.frame(variable = "prediction",
                              contribution = attr(explainer, "prediction"),
                              variable_name = "prediction",
                              variable_value = attr(explainer, "prediction"),
                              sign = if (attr(explainer, "prediction") >= 0) 1 else -1,
                              label = explainer$label[1])
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
  variables <- as.list(unique(as.character(explainer$variable_name)))
  shap_contributions <- lapply(variables, function(x) {
    explainer[explainer$variable_name == x & explainer$B > 0, 'contribution']
  })
  names(shap_contributions) <- variables
  attr(df_break_down, "shap_contributions") <- shap_contributions
  # We keep yhats_distribution
  attr(df_break_down, 'yhats_distribution') <- attr(explainer, 'yhats_distribution')
  class(df_break_down) <- c("break_down", "data.frame")
  df_break_down
}
