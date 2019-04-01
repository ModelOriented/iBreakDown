#' Plot Break Down Objects in D3 with r2d3 package.
#'
#' Experimental interactive explainer created with 'D3.js' library.
#'
#' @param x the model model of `break_down`` class.
#' @param ... other parameters.
#' @param baseline if numeric then veritical line will start in baseline.
#' @param max_features maximal number of features to be included in the plot. default value is 10.
#' @param min_max a range of OX axis. By deafult `NA` therefore will be extracted from the contributions of `x`. But can be set to some constants, usefull if these plots are used for comparisons.
#' @param vcolors named vector with colors.
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument.
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{\link{signif}} which keeps a specified number of significant digits.
#' Or the default \code{\link{round}} to have the same precision for all components.
#'
#' @return an `r2d3` object.
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @examples
#' library("DALEX")
#' library("iBreakDown")
#' # Toy examples, because CRAN angels ask for them
#' titanic <- na.omit(titanic)
#' set.seed(1313)
#' titanic_small <- titanic[sample(1:nrow(titanic), 500), c(1,2,6,9)]
#' model_titanic_glm <- glm(survived == "yes" ~ gender + age + fare,
#'                        data = titanic_small, family = "binomial")
#' explain_titanic_glm <- explain(model_titanic_glm,
#'                            data = titanic_small[,-9],
#'                            y = titanic_small$survived == "yes")
#' bd_rf <- local_attributions(explain_titanic_glm, titanic_small[1, ])
#' bd_rf
#' plotD3(bd_rf)
#'
#' \donttest{
#' library("randomForest")
#' titanic <- na.omit(titanic)
#' model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
#'                                    fare + sibsp + parch,  data = titanic)
#'
#' explain_titanic_rf <- explain(model_titanic_rf,
#'                               data = titanic[,-9],
#'                               y = titanic$survived == "yes",
#'                               label = "Random Forest v7")
#'
#' new_passanger <- data.frame(
#'   class = factor("1st", levels = c("1st", "2nd", "3rd", "deck crew", "engineering crew",
#'                                     "restaurant staff", "victualling crew")),
#'   gender = factor("male", levels = c("female", "male")),
#'   age = 8,
#'   sibsp = 0,
#'   parch = 0,
#'   fare = 72,
#'   embarked = factor("Southampton",
#'                   levels = c("Belfast", "Cherbourg", "Queenstown", "Southampton")))
#'
#'   rf_la <- local_attributions(explain_titanic_rf, new_passanger)
#'   rf_la
#'
#'   plotD3(rf_la)
#'   plotD3(rf_la, max_features = 3)
#'   plotD3(rf_la, max_features = 3, min_max = c(0,1))
#' }
#' @export
#' @rdname plotD3
plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @export
#' @rdname plotD3
plotD3.break_down <- function(x, ...,
                        baseline = NA,
                        max_features = 10,
                        min_max = NA,
                        vcolors = DALEX::theme_drwhy_colors_break_down(),
                        digits = 3, rounding_function = round) {

  # remove first and last row
  model_prediction <- rounding_function(x$contribution[nrow(x)], digits)
  model_baseline <- rounding_function(ifelse(is.na(baseline), x$contribution[1], baseline), digits)
  x <- prepare_data_for_break_down_plot3D(x, vcolors, max_features)

  # convert to list
  # will be easier to pass to D3 plot
  x_as_list <- lapply(1:nrow(x), function(i) {
    list(variable = as.character(x$variable[i]),
         contribution = x$contribution[i],
         cummulative = x$cummulative[i],
         sign = as.character(x$sign[i]),
         label = paste0(substr(x$variable[i], 1, 100),
                        "<br>", ifelse(x$contribution[i] > 0, "increases", "decreases"),
                        " average response <br>by ",
                        rounding_function(abs(x$contribution[i]), digits))
         )
  })

  # range
  if (any(is.na(min_max))) {
    min_max <- range(c(model_baseline, model_prediction,
                       min(x$cummulative), max(x$cummulative))) * c(0.95,1.05)
  }

  # plot D3 object
  r2d3::r2d3(
    data = x_as_list,
    script = system.file("breakDownD3.js", package = "iBreakDown"),
    dependencies = system.file("tooltipD3.js", package = "iBreakDown"),
    options = list(xmin = min_max[1], xmax = min_max[2],
                   model_avg = model_baseline, model_res = model_prediction),
    d3_version = "4"
  )
}

prepare_data_for_break_down_plot3D <- function(x, vcolors, max_features = 10) {
  x <- x[-c(1,nrow(x)),]

  if (nrow(x) > max_features) {
    last_row <- max_features + 1
    new_x <- x[1:last_row,]
    new_x$variable <- as.character(new_x$variable)
    new_x$variable[last_row] = "  + all other factors"
    new_x$contribution[last_row] = sum(x$contribution[last_row:nrow(x)])
    new_x$cummulative[last_row] = x$cummulative[nrow(x)]
    new_x$sign[last_row] = ifelse(new_x$contribution[last_row] > 0,
                                  "1","-1")
    x <- new_x
  }

  # add colors
  x$sign <- vcolors[x$sign]
  x
}

