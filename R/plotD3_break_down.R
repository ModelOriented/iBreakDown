#' @title Plot Break Down Objects in D3 with r2d3 package.
#'
#' @description
#' Plots waterfall break down for objects of the `break_down` class.
#' Usually executed after `break_down()` or `local_attributions()` function.
#'
#' @param x the model model of `break_down` class.
#' @param ... other parameters.
#' @param baseline if numeric then veritical line will start in baseline.
#' @param max_features maximal number of features to be included in the plot. default value is 10.
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument.
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{\link{signif}} which keeps a specified number of significant digits.
#' Or the default \code{\link{round}} to have the same precision for all components.
#' @param bar_width width of bars in px. By default 12px
#' @param margin extend x axis domain range to adjust the plot. Usually value between 0.1 and 0.3, by default it's 0.2
#' @param scale_height should the height of plot scale with window size? By default it's FALSE
#' @param min_max a range of OX axis. By deafult `NA` therefore will be extracted from the contributions of `x`.
#' But can be set to some constants, usefull if these plots are used for comparisons.
#' @param vcolors named vector with colors. By default `NA` therfore will choose DrWhy colors
#' @param chartTitle a character. Set custom title
#'
#' @return an `r2d3` object.
#'
#' @references Predictive Models: Visual Exploration, Explanation and Debugging \url{https://pbiecek.github.io/PM_VEE}
#'
#' @examples
#' library("DALEX")
#' library("iBreakDown")
#'
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
#' library(randomForest)
#'
#' m_rf <- randomForest(status ~ . , data = HR[2:2000,])
#' new_observation <- HR_test[1,]
#' new_observation
#'
#' p_fun <- function(object, newdata){predict(object, newdata=newdata, type = "prob")}
#'
#' bd_rf <- local_attributions(m_rf,
#'                            data = HR_test,
#'                            new_observation =  new_observation,
#'                            predict_function = p_fun)
#'
#' bd_rf
#' plotD3(bd_rf)
#'
#' @export
#' @rdname plotD3
plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @export
#' @rdname plotD3
plotD3.break_down <- function(x, ...,
                        baseline = NA,
                        max_features = 10,
                        digits = 3, rounding_function = round,
                        bar_width = 12,
                        margin = 0.2,
                        scale_height = FALSE,
                        min_max = NA,
                        vcolors = NA,
                        chartTitle = NA) {

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
    if (length(levels(x[,'label'])) > 1) {
      # update plot count
      n <- n + length(levels(x[,'label'])) - 1

      # add new data frames to list
      bdl <- c(bdl, split(x, f=x[,'label']))

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

    new_x <- prepare_data_for_break_down_plotD3(x, baseline, max_features, rounding_function, digits)

    dl[[i]] <- new_x

    # remember plot names
    modelNames <- c(modelNames,as.character(x[,'label'][1]))
  }

  if (length(unique(m)) > 1) stop("Models have different numbers of features.")

  m <- unique(m)
  names(dl) <- modelNames

  df <- do.call(rbind, dl)

  # later count longest label width in d3
  labelList <- as.character(df[,'variable'])

  if (any(is.na(min_max))) {
    min_max <- range(df[,'cummulative'])
  }

  # count margins

  min_max_margin <- abs(min_max[2]-min_max[1])*margin
  min_max[1] <- min_max[1] - min_max_margin
  min_max[2] <- min_max[2] + min_max_margin

  options <- list(xmin = min_max[1], xmax = min_max[2],
                  n = n, m = m, barWidth = bar_width,
                  scaleHeight = scale_height,
                  vcolors = ifelse(is.na(vcolors), "default", vcolors),
                  chartTitle = ifelse(is.na(vcolors), "Local attributions", chartTitle))

  temp <- jsonlite::toJSON(list(dl, labelList))

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

prepare_data_for_break_down_plotD3 <- function(x, baseline, max_features = 10, rounding_function, digits) {

  # fix df
  x[,'variable'] <- as.character(x[,'variable'])
  x[,'variable_name'] <- as.character(x[,'variable_name'])

  x[x[,'variable_name']=="",'variable_name'] <- "prediction"

  temp <- data.frame(x[c(1,nrow(x)),])
  x <- data.frame(x[-c(1,nrow(x)),])

  if (nrow(x) > max_features) {
    last_row <- max_features + 1
    new_x <- x[1:last_row,]
    new_x[last_row,'variable'] <- "+ all other factors"
    new_x[last_row,'contribution'] <- sum(x[last_row:nrow(x),'contribution'])
    new_x[last_row,'cummulative'] <- x[nrow(x),'cummulative']
    new_x[last_row,'sign'] <- ifelse(new_x[last_row,'contribution'] > 0,1,-1)

    x <- new_x
  }

  x <- rbind(temp[1,], x, temp[2,])

  if (is.na(baseline)) {
    baseline <- x[1,"cummulative"]
  }

  # fix contribution and sign
  x[c(1,nrow(x)),"contribution"] <- x[c(1,nrow(x)),"contribution"] - baseline

  x[c(1,nrow(x)),"sign"] <- ifelse(x[c(1,nrow(x)),"contribution"] > 0,1,ifelse(x[c(1,nrow(x)),"contribution"] < 0,-1,0))

  # use for bars
  x[,'barStart'] <- ifelse(x[,'sign'] == "1", x[,'cummulative'] - x[,'contribution'], x[,'cummulative'])
  x[,'barSupport'] <- ifelse(x[,'sign'] == "1", x[,'cummulative'], x[,'cummulative'] - x[,'contribution'])

  # use for text label and tooltip
  x[,'contribution'] <- rounding_function(x['contribution'], digits)
  x[,'cummulative'] <- rounding_function(x['cummulative'], digits)

  # use for color
  x[c(1,nrow(x)),"sign"] <- "X"

  x[,'tooltipText'] <- ifelse(x[,'sign'] == "X", paste0("Average response: ",x[1,'cummulative'],
                                                        "<br>", "Prediction: ",
                                                        x[nrow(x),'cummulative']),
                              paste0(substr(x[,'variable'], 1, 25),
                                     "<br>", ifelse(x[,'contribution'] > 0, "increases", "decreases"),
                                     " average response <br>by ", abs(x[,'contribution'])))

  x
}
