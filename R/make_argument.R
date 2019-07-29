#' Makes an argument for description
#'
#' @param explainer an iBreakDown explanation
#' @param label a prediction label
#' @param display_values displays values
#' @param display_numbers displays numbers
#' @param display_argumentation displays argumentation mode
#' @param model_name name of the model to be explained
#' @param description_profile description scenario
#' @param display_shap displays addition information about shap explanation
#'
#' @return an argument



make_argument <- function(explainer,
                          label,
                          display_values,
                          display_numbers,
                          display_argumentation,
                          model_name,
                          description_profile,
                          display_shap) {

  # Argumentation strategy No.1: We take three variables with largest contributions and sort them by contribution.

  if (display_argumentation == 1) {
    # Preparing a data frame for generating arguments
    df <- explainer[-c(1,nrow(explainer)), c("variable_name","contribution", "variable_value")]
    df['importance'] <- abs(df$contribution)
    df <- df[order(df$importance, decreasing = TRUE), ] # We do not cut arguments with importance below the treshold
    df[1:3,'order'] <- c("", "second ", "third ") # We choose three most significant variables
    df['contribution'] <- round(df['contribution'], 3)
    df['shap'] <- "" # needed for shap explanation

    if (display_values) {
     df['variable_value'] <- sapply(df['variable_value'], function(x) {
       y <- paste0("(= ", x, ")")
     })
     df$variable_name <- paste0(df$variable_name, " ", df$variable_value)
    }

    show_shap <- FALSE
    if (display_shap) {
      if (is.null(attr(explainer, 'shap_contributions'))) {
        warning("Explainer is not a break_down_uncertainty/shap explainer")
      } else {
        show_shap <- TRUE
        df['shap'] <- sapply(as.character(df[ ,'variable_name']), function(x) {
          x <- as.character(x)
          importance <- df[df$variable_name == x,'importance']
          quantiles <- summary(attr(explainer, 'shap_contributions')[[x]])
          important <- if (abs(quantiles[[4]] - quantiles[[2]]) < importance) T else F
          # important <- if (sd(attr(explainer, 'shap_contributions')[[x]])/sqrt(B) < importance) T else F
          if (important) "" else x
          })
        }
    }
    if (show_shap) {
      shap <-paste(df[1:3, 'shap'],collapse = "")
      if (shap =="") {
        shap <- "The average contribution of all the above variable's is significant."
      } else {
        shap <- paste0("From the above variables ",
                       paste(df[!(df$shap == ""), 'shap'], collapse = ", "),
                       " may not be significant may not be significant",
                       "duu to high average contribution dispersion.")
      }
    }

    if (display_numbers) {
      sign1 <- if (df$contribution[1] > 0) "increases" else "decreases"
      argument1 <- paste0("The ", {df$order[1]}, "most important variable is ", {df$variable_name[1]}, ". It ",{sign1}, " the prediction by ", {abs(df$contribution[1])}, ".", df$shap[1])
      sign2 <- if (df$contribution[2] > 0) "increases" else "decreases"
      argument2 <- paste0("The ", {df$order[2]}, "most important variable is ", {df$variable_name[2]},". It ", {sign2}, " the prediction by ", {abs(df$contribution[2])}, ".", df$shap[2])
      sign3 <- if (df$contribution[3] > 0) "increases" else "decreases"
      argument3 <- paste0("The ", {df$order[3]}, "most important variable is ", {df$variable_name[3]},". It ", {sign3}, " the prediction by ", {abs(df$contribution[3])}, ".", df$shap[3])
      if (show_shap) {
      argumentation <- paste(argument1, argument2, argument3, shap, sep = " \n")
      } else {
        argumentation <- paste(argument1, argument2, argument3, sep = " \n")
      }
    } else {
      df <- df[1:3, ]
      df_positive <- df[which(df$contribution >= 0), ]
      df_negative <- df[which(df$contribution < 0), ]
      prefix_pos <- if (nrow(df_positive) > 1) "are" else "is"
      prefix_neg <- if (nrow(df_negative) > 1) "are" else "is"

      s_pos <- if (nrow(df_positive) > 1) "s" else ""
      s_neg <- if (nrow(df_negative) > 1) "s" else ""

      increasing <- paste(df_positive$variable_name, collapse = ", ")
      decreasing <- paste(df_negative$variable_name, collapse = ", ")

      arguments_increasing <- if (nrow(df_positive) == 0) "" else paste0("The most important variable", s_pos, " that increase the prediction ", prefix_pos, " ", increasing, ".")
      arguments_decreasing <- if (nrow(df_negative) == 0) "" else paste0("The most important variable", s_neg, " that decrease the prediction ", prefix_neg, " ", decreasing, ".")

      #order denend on profile
      if (description_profile == 0) argumentation <- paste(arguments_increasing, arguments_decreasing, sep = " \n")
      if (description_profile == 1) argumentation <- paste(arguments_decreasing, arguments_increasing, sep = " \n")
      if (description_profile == 2 | description_profile == 3) {
        argumentation <- paste(arguments_increasing,arguments_decreasing, sep = " \n")
        if (nrow(df_positive) == 0 | nrow(df_negative) == 0) {
          argumentation <- gsub("\n","", argumentation)
        }
        }
        if (show_shap) {
          argumentation <- paste(argumentation, shap, sep = " \n")
        }
    }
    }

  argumentation
}


