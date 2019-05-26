#' Plot Break Down Objects in D3 with r2d3 package.
#'
#' Experimental interactive explainer created with 'D3.js' library.
#'
#' @param x the model model of `break_down` class.
#' @param ... other parameters.
#' @param baseline TODO if numeric then veritical line will start in baseline.
#' @param max_features maximal number of features to be included in the plot. default value is 10.
#' @param min_max a range of OX axis. By deafult `NA` therefore will be extracted from the contributions of `x`. But can be set to some constants, usefull if these plots are used for comparisons.
#' @param vcolors TODO named vector with colors.
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument.
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{\link{signif}} which keeps a specified number of significant digits.
#' Or the default \code{\link{round}} to have the same precision for all components.
#' @param bar_width todo
#' @param scale_height todo
#' @param margin todo
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
#'                            y = titanic_small$survived == "yes",
#'                            label = "glm")
#' bd_glm <- local_attributions(explain_titanic_glm, titanic_small[1, ])
#' bd_glm
#' plotD3(bd_glm)
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
                        digits = 3, rounding_function = round,
                        bar_width = 12, scale_height = FALSE, margin = 0.2) {

  n <- length(list(...)) + 1
  m <- c()

  bdl <- list(x, ...)
  deletedIndexes <- c()

  dl <- list()
  modelNames <- c()

  for (i in 1:n) {
    x <- bdl[[i]]

    if (!("break_down" %in% class(x))) stop("The function requires an object created with local_attributions().")

    # because apparently one explainer can make multiple plots
    if (length(levels(x$label)) > 1) {
      # update plot count
      n <- n + length(levels(x$label)) - 1

      # add new data frames to list
      bdl <- c(bdl, split(x, f=x$label))

      # remember indexes to delete
      deletedIndexes <- c(deletedIndexes, i)
    }
  }

  # delete doubled data frames
  bdl[deletedIndexes] <- NULL

  # iterate through updated data frame list
  for (i in 1:n) {
    x <- bdl[[i]]

    # remember number of features to compare
    m <- c(m, ifelse(nrow(x) - 2 <= max_features, nrow(x), max_features + 3))

    new_x <- prepare_data_for_break_down_plot3D(x, vcolors, max_features, rounding_function, digits)

    dl[[i]] <- new_x

    # remember plot names
    modelNames <- c(modelNames,as.character(x$label[1]))
  }

  if (length(unique(m)) > 1) stop("Models have different numbers of features.")

  m <- unique(m)
  names(dl) <- modelNames

  # count margins
  if (any(is.na(min_max))) {

    df <- do.call(rbind, dl)

    if (is.na(baseline)) {
      model_baseline <- NULL
    } else {
      model_baseline <- baseline
    }


    min_max <- range(c(df$cummulative, model_baseline))
    min_max_margin <- abs(min_max[2]-min_max[1])*margin
    min_max[1] <- min_max[1] - min_max_margin
    min_max[2] <- min_max[2] + min_max_margin
  }

  options <- list(xmin = min_max[1], xmax = min_max[2],
                  n = n, m = m, barWidth = bar_width)

  temp <- jsonlite::toJSON(list(dl))

  # plot D3 object
  r2d3::r2d3(
    data = temp,
    script = system.file("d3js/breakDownD3.js", package = "iBreakDown"),
    dependencies = list(
      system.file("d3js/colorsDrWhy.js", package = "iBreakDown"),
      system.file("d3js/tooltipD3.js", package = "iBreakDown")
    ),
    css = system.file("d3js/themeDrWhy.css", package = "iBreakDown"),
    options = options,
    d3_version = "4"
  )
}

prepare_data_for_break_down_plot3D <- function(x, vcolors, max_features = 10, rounding_function, digits) {

  temp <- data.frame(x[c(1,nrow(x)),])
  x <- data.frame(x[-c(1,nrow(x)),])

  if (nrow(x) > max_features) {
    last_row <- max_features + 1
    new_x <- x[1:last_row,]
    new_x$variable[last_row] <- "  + all other factors"
    new_x$contribution[last_row] <- sum(x$contribution[last_row:nrow(x)])
    new_x$cummulative[last_row] <- x$cummulative[nrow(x)]
    new_x$sign[last_row] <- ifelse(new_x$contribution[last_row] > 0,1,-1)

    x <- new_x
  }

  x <- rbind(temp[1,], x, temp[2,])

  x$sign[x$variable_name == ""] <- "X"
  x[1,"sign"] <- 0
  x[1,"contribution"] <- 0

  x$barStart <- ifelse(x$sign == "1", x$cummulative - x$contribution, x$cummulative)
  x$barSupport <- ifelse(x$sign == "1", x$cummulative, x$cummulative - x$contribution)

  x$contribution <- rounding_function(x$contribution, digits)
  x$cummulative <- rounding_function(x$cummulative, digits)

  x$label <- paste0(substr(x$variable, 1, 25),
                        "<br>", ifelse(x$contribution > 0, "increases", "decreases"),
                        " average response <br>by ", x$contribution)
  x$variable <- as.character(x$variable)

  x
}

