#' Generates Textual Explanations for Predictive Models
#'
#' @description Generic function \code{describe} generates natural language explanations based on
#' \code{\link{break_down}} and \code{\link{shap}} explanations, what enhances their interpretability.
#'
#' @details Function \code{describe} generates a textual explanations by extracting information from
#' a \code{\link{break_down}} or \code{\link{shap}} explanation. It makes an argument justifying why
#' the model's prediction is lower or higher, than it's average prediction. The description consists of
#' an introduction, argumenation and summary making use from the claim, support, evidence argumentation
#' structure, as recomended for the World Universities Debating style.
#'
#' The function first selects one of four different scenarios, due to
#' \code{nonsignificance_treshold}. The chosen scenario can be one of the following:
#' 1. Model's prediction for the selected instance is significantly higher than the average prediction.
#' 2. Model's prediction is significantly lower.
#' 3. Model's prediction is close to it's average prediction, however there are significant
#' variables counteracting with each other
#' 4. Model's prediction is close to it's average prediction and all the variables are rather nonsignificant.
#' Then an explanation due to the chosen scenario is generated.
#'
#' @param x an explanation created with \code{\link{break_down}} or \code{\link{shap}}
#' @param nonsignificance_treshold a numeric specifying a treshold for variable importance
#' @param ... other arguments
#' @param label a character string describing model's prediction
#' @param short_description a boolean, returns a short description
#' @param display_values a boolean, displays variables' values
#' @param display_numbers a boolean, displays a description containing numerical values
#' @param display_distribution_details a boolean, displays details about the distribution of model's predictions
#' @param display_shap a boolean, adds information about variables' average contribution. Use only with \code{\link{shap}} explanation.
#'
#' @return A character string of textual explanation
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
#'                               data = titanic[ ,-9],
#'                               y = titanic$survived == "yes",
#'                               label = "Random Forest v7")
#'
#' bd_explanation <- break_down(explain_titanic_rf, titanic[1, ], keep_distributions = TRUE)
#'
#' description <- describe(bd_explanation,
#'                         label = "the passanger will survive with probability",
#'                         short_description = FALSE,
#'                         display_values =  TRUE,
#'                         display_numbers = TRUE,
#'                         display_distribution_details = FALSE)
#'
#' description
#'
#'
#'
#' @export
#' @rdname describe

describe <- function(x, nonsignificance_treshold = 0.15, ...)
  UseMethod("describe")

#' @export
#' @rdname describe

describe.break_down <- function(x,
                                nonsignificance_treshold = 0.15,
                                ...,
                                label = NULL,
                                short_description = FALSE,
                                display_values = FALSE,
                                display_numbers = FALSE,
                                display_distribution_details = FALSE,
                                display_shap = FALSE
                                ) {
  # Error handling
  if (!( class(display_values) == 'logical' &
         class(display_numbers) == 'logical' &
         class(short_description) == 'logical' &
         class(display_distribution_details) == 'logical' &
         class(nonsignificance_treshold) == 'numeric' &
         class(display_shap) == 'logical')) {
    stop("Arguments are not valid")
  }

  # check model's name for description
  model_name <- as.character(x$label[1])
  model_name <- paste(toupper(substr(model_name, 1, 1)), substr(model_name, 2, nchar(model_name)), sep="")

  description_profile <- description_profile(x, nonsignificance_treshold)

  distribution_kept <- ifelse(is.null(attr(x, "yhats_distribution")), FALSE, TRUE)

  if (is.null(label)) label <- "the prediction for the selected instance is"

  if (short_description) {
    descriptions <- make_short_description(x,
                                           display_values,
                                           label,
                                           model_name,
                                           description_profile)
  } else {
    introduction <- make_introduction(x,
                                     label,
                                     display_numbers,
                                     display_distribution_details,
                                     model_name,
                                     distribution_kept,
                                     description_profile)
    argumentation <- make_argument(x,
                                   label,
                                   display_values,
                                   display_numbers,
                                   model_name,
                                   description_profile,
                                   display_shap)
    summary <- make_summary(x)

    descriptions <- paste0(introduction,"\n\n",argumentation, "\n\n",summary)
   descriptions <- gsub("\n\n\n","\n\n", descriptions)
  }
  class(descriptions) <- c("break_down_description", "character")
  descriptions
}

# Returns a scenario for a description
#
# 1 = prediction significantly higher than the average
# 2 = prediction significantly lower than the average
# 3 = prediction close to the average, but there are significant contributors
# 4 = prediction close to the average and there are no significant contributors
#
# @return an integer from 1 to 4
#

description_profile <- function(x,
                                nonsignificance_treshold){

  model_intercept <- round(x$contribution[1],3)
  model_prediction <- round(x$contribution[length(x$contribution)],3)
  model_contributions <- x$contribution[-c(1,length(x$contribution))]
  model_highest_contribution <- sort(abs(model_contributions), decreasing = TRUE)[1]
  distance <- abs(model_intercept - model_prediction)

  if (distance/model_intercept > nonsignificance_treshold) {
    profile <- if (model_intercept < model_prediction) 1 else 2
  } else {
    is_significant <- (model_highest_contribution > nonsignificance_treshold*10*distance)
    profile <- if (is_significant) 3 else 4
  }
  profile
}

#  Describes a distribution of model's pedictions.
#
# @return a decription of predictions distribution

describe_distribution <- function(x,
                                  display_numbers,
                                  display_distribution_details,
                                  model_name) {

  yhats_distribution <- attr(x, "yhats_distribution")
  model_predictions <- yhats_distribution[which(yhats_distribution$variable == "all data"),]$prediction
  prediction <- round(x[dim(x)[1],'contribution'],3)
  intercept <- round(x$contribution[1],3)
  median_prediction <- round(median(model_predictions),3)

  if (!display_distribution_details) {

    quantiles <- quantile(model_predictions, seq(0,1, by = 0.01))

    if (prediction > median_prediction) {
      place <- names(which(quantiles < prediction)[length(which(quantiles < prediction))])
      description <- paste0("For the selected instance model's prediction is higher, than for ",
                            place," of all observations.")
    }
    if (prediction == median_prediction) {
      description <- paste0("For the selected instance model's prediction is the median prediction.")
    }
    if (prediction < median_prediction) {
      place <- names(which(quantiles < prediction)[length(which(quantiles < prediction))])
      place <- 100 - as.numeric(unlist(strsplit(place, split = "%"))) # Here we revert the quantile
      place <- paste0(place, "%")
      description <- paste0(" For the selected instance model's prediction is lower, than for ",
                            place," of all observations.")
    }
  } else {
    mean <- round(mean(model_predictions),3)
    median <- round(median(model_predictions),3)
    sd <- round(sd(model_predictions),3)
    if ((((median - mean)*3)/sd) < -0.5) skeweness <- "right-skewed"
    if ((((median - mean)*3)/sd) > 0.5) skeweness <- "left-skewed"
    if (abs(((median - mean)*3)/sd) < 0.5) skeweness <- "central"

    predictions_quartile <- quantile(model_predictions)
    quartile <- which(predictions_quartile < prediction)
    quartile <- quartile[[length(quartile)]]
    quartile_which <- switch(quartile,
                             '1' = "first",
                             '2' = "second",
                             '3' = 'third',
                             '4' = "fourth")

    description <- paste0("Model predictions range from ", round(min(model_predictions),3),
                          " to ", round(max(model_predictions), 3), ". The distribution of ",
                          model_name, "'s predictions is ", skeweness, " with average equal to ",
                          mean , " and median equal to ", median, ". The standard deviation is ",
                          sd, ". Model's prediction for the selected instance is in the ",
                          quartile_which, " quartile.")
  }
  description
}

# Makes an introduction for the description function

make_introduction <- function(x,
                              label,
                              display_numbers,
                              distribution_details,
                              model_name,
                              distribution_kept,
                              description_profile){

  intercept <- round(x$contribution[1],3)
  prediction <- round(x$contribution[length(x$contribution)],3)
  contributions <- x$contribution[-c(1,length(x$contribution))]

  numbers <- if (display_numbers) paste0(" ",as.character(intercept)) else NULL
  introduction <- switch(description_profile,
                         '1' = paste0(model_name," predicts, that ",label," ",prediction,
                                " which is higher than the average model prediction",
                                numbers,"."),
                         '2' = paste0(model_name," predicts, that ", label," ", prediction,
                                " which is lower than the average model prediction",
                                numbers,"."),
                         '3' = paste0(model_name," predicts, that ", label," ", prediction,
                                " which is close to the average model prediction",
                                numbers, "."),
                         '4' = paste0(model_name," predicts, that ", label," ", prediction,
                                " which is close to the average model prediction",
                                numbers, ".")
                         )

  if (distribution_kept) {
    distribution_description <- describe_distribution(x,
                                                      display_numbers,
                                                      distribution_details,
                                                      model_name)

    introduction <- paste0(introduction, distribution_description)
  }
  introduction
}

# Makes an argument for description

make_argument <- function(x,
                          label,
                          display_values,
                          display_numbers,
                          model_name,
                          description_profile,
                          display_shap) {

  # Preparing a data frame for generating arguments
  df <- x[-c(1,nrow(x)), c("variable_name","contribution", "variable_value")]
  df['importance'] <- abs(df$contribution)
  df <- df[order(df$importance, decreasing = TRUE), ] # We do not cut arguments with importance below the treshold
  nrow_df <- min(3, nrow(df))
  df['contribution'] <- round(df['contribution'], 3)
  df['shap'] <- "" # needed for shap explanation
  shap <- NULL

  if (display_values) {
    df$variable_value <- paste0("(= ", df$variable_value, ")")
    df$variable_name <- paste0(df$variable_name, " ", df$variable_value)
  }


  shap <- describe_shap(display_shap = display_shap,
                        x = x,
                        df = df,
                        nrow_df = nrow_df)

  if (display_numbers) {
    argument1 <- NULL
    argument2 <- NULL
    argument3 <- NULL

    if (nrow(df) > 0) {
    sign1 <- if (df$contribution[1] > 0) "increases" else "decreases"
    argument1 <- paste0("The most important variable is ", {df$variable_name[1]},
                        ". It ",{sign1}, " the prediction by ", {abs(df$contribution[1])},
                        ".", df$shap[1])
    }
    if (nrow(df) > 1) {
      sign2 <- if (df$contribution[2] > 0) "increases" else "decreases"
      argument2 <- paste0("The second most important variable is ", {df$variable_name[2]},
                          ". It ", {sign2}, " the prediction by ", {abs(df$contribution[2])},
                          ".", df$shap[2])
    }
    if (nrow(df) > 2) {
      sign3 <- if (df$contribution[3] > 0) "increases" else "decreases"
      argument3 <- paste0("The third most important variable is ", {df$variable_name[3]},
                          ". It ", {sign3}, " the prediction by ", {abs(df$contribution[3])},
                          ".", df$shap[3])
    }
    argumentation <- paste(argument1, argument2, argument3, shap, sep = "\n")
  } else {
    df <- df[seq_along(nrow_df), ]
    arguments_increasing <- make_argument_plain(df, "positive")
    arguments_decreasing <- make_argument_plain(df, "negative")

    #arguments' order depends on description_profile
    argumentation <- switch(description_profile,
                            '1' = paste(arguments_increasing, arguments_decreasing, shap, sep = "\n"),
                            '2' = paste(arguments_decreasing, arguments_increasing, shap, sep = "\n"),
                            '3' = paste(arguments_increasing,arguments_decreasing, shap, sep = "\n"),
                            '4' = paste(arguments_increasing,arguments_decreasing, shap, sep = "\n")
                            )
  }
  argumentation <- gsub("\n\n","\n", argumentation)
  argumentation
}

# Makes a summary for the description


make_summary <- function(x){
    # We make the same df as during argument selection
    df <- x[-c(1,dim(x)[1]), c("contribution","variable")]
    df['importance'] <- abs(df$contribution)
    df <- df[order(df$importance, decreasing = TRUE), ]
    other_importance <- sum(df[ ,'contribution']) -sum(head(df[ ,'contribution'],3))

    summary <- paste0("Other variables are with less importance. ",
                      "The contribution of all other variables is ",
                      round(other_importance,3),
                      ".")
    summary
  }


make_short_description <- function(x,
                                   display_values,
                                   label,
                                   model_name,
                                   description_profile) {
  # Returns a short description of a break_down explanation

  intercept <- round(x$contribution[1],3)
  prediction <- round(x$contribution[length(x$contribution)],3)
  most_important_variable <- x[2, ]
  most_important_contribution <- most_important_variable$contribution

  # We choose the most informative description depending on possible cases:
  # Case 1: prediction is highly positive or negative and the most important variables sign
  #         follows the predictions sign

  sign_coherence <- (prediction - intercept) * most_important_contribution > 0

  if (description_profile %in% 1:2 & sign_coherence) {
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
# Makes an argument description for display_numbers == FALSE

make_argument_plain <- function(df, mode) {
  if (mode == "positive") {

    df_positive <- df[which(df$contribution >= 0), ]
    prefix_pos <- if (nrow(df_positive) > 1) "are" else "is"
    s_pos <- if (nrow(df_positive) > 1) "s" else ""
    increasing <- paste(df_positive$variable_name, collapse = ", ")
    arguments <- if (nrow(df_positive) == 0) NULL else paste0("The most important variable", s_pos, " that increase the prediction ", prefix_pos, " ", increasing, ".")

  } else if (mode == "negative") {

    df_negative <- df[which(df$contribution < 0), ]
    prefix_neg <- if (nrow(df_negative) > 1) "are" else "is"
    s_neg <- if (nrow(df_negative) > 1) "s" else ""
    decreasing <- paste(df_negative$variable_name, collapse = ", ")
    arguments <- if (nrow(df_negative) == 0) NULL else paste0("The most important variable", s_neg, " that decrease the prediction ", prefix_neg, " ", decreasing, ".")

  }
  arguments
}
# Generates SHAP values' description

describe_shap <- function(display_shap, x, df, nrow_df){
  if (display_shap) {
    if (is.null(attr(x, 'shap_contributions'))) {
      warning("Explanation is not a break_down_uncertainty/shap explanation")
    } else {
      boxplot_length <- sapply(attr(x, 'shap_contributions'), function(contribution) {
        abs(unname(quantile(contribution)['75%'] - quantile(contribution)['25%']))
      })
      is_important <- sapply(names(boxplot_length), function(name) {
        df[df$variable_name == name, 'importance'] > unname(boxplot_length[name])
      })
      sapply(names(is_important), function(name) {
        df[df$variable_name == name, 'shap'] <- is_important[name]})
      shap <- paste(df[seq_along(nrow_df), 'shap'], collapse = "")
      shap <- ifelse(shap =="",
                     "The average contribution of all the above variables is significant.",
                     paste0("From the above variables ",
                            paste(df[!(df$shap == ""), 'shap'], collapse = ", "),
                            " may not be significant",
                            " due to high average contribution dispersion."))
    }
  } else {
  NULL
  }
}
