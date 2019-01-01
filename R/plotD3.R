#' Plot Break Down Objects in D3 with r2d3
#'
#' @param x the model model of 'break_down' class
#' @param ... other parameters
#' @param vcolors named vector with colors
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{signif()} which keeps a specified number of significant digits.
#' Or the default \code{round()} to have the same precision for all components
#'
#' @return a r2d3 object
#'
#' @examples
#' \dontrun{
#' ## Not run:
#' # prepare dataset
#' library("titanic")
#' titanic <- titanic_train[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
#' titanic$Survived <- factor(titanic$Survived)
#' titanic$Sex <- factor(titanic$Sex)
#' titanic$Embarked <- factor(titanic$Embarked)
#' titanic <- na.omit(titanic)
#'
#' # prepare model
#' library("randomForest")
#' rf_model <- randomForest(Survived ~ .,  data = titanic)
#'
#' library("DALEX2")
#' predict_fuction <- function(m,x) predict(m, x, type = "prob")[,2]
#' rf_explain <- explain(rf_model, data = titanic,
#'                       y = titanic$Survived == "1", label = "RF",
#'                       predict_function = predict_fuction)
#'
#' library("breakDown2")
#' rf_la <- local_attributions(rf_explain, titanic[2,])
#'
#' plot(rf_la)
#' plotD3(rf_la)
#' }
#' @export
#' @rdname plotD3
plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @export
#' @rdname plotD3
plotD3.break_down <- function(x, ...,
                        max_features = 4,
                        vcolors = c("-1" = "#a3142f", "0" = "#a3142f", "1" = "#0f6333", "X" = "#0f6333"),
                        digits = 3, rounding_function = round) {
  x <- rf_la

  class(x) = "data.frame"

  # remove first and last row
  model_baseline <- rounding_function(x$contribution[1], digits)
  model_prediction <- rounding_function(x$contribution[nrow(x)], digits)
  x <- x[-c(1,nrow(x)),]

  if (nrow(x) > max_features) {
    last_row <- max_features + 1
    new_x <- x[1:last_row,]
    new_x$variable <- as.character(new_x$variable)
    new_x$variable[last_row] = "  all other factors"
    new_x$contribution[last_row] = sum(x$contribution[last_row:nrow(x)])
    new_x$cummulative[last_row] = x$cummulative[nrow(x)]
    new_x$sign[last_row] = ifelse(new_x$contribution[last_row] > 0,
                                  "1","-1")
    x <- new_x
  }

  # add colors
  x$sign <- vcolors[x$sign]

  # convert to list
  x_as_list <- lapply(1:nrow(x), function(i) {
    list(variable = as.character(x$variable[i]),
         contribution = x$contribution[i],
         cummulative = x$cummulative[i],
         sign = as.character(x$sign[i]),
         label = paste0(substr(x$variable[i], 3, 100),
                        "<br>", ifelse(x$contribution[i] > 0, "increases", "decreases"),
                        " average response <br>by ",
                        rounding_function(x$contribution[i], digits))
         )
  })

  # range
  min_max <- range(c(model_baseline, model_prediction,
                     min(x$cummulative), max(x$cummulative))) * c(0.95,1.05)

  # plot D3 object
  r2d3::r2d3(
#    data = list(structure(list("+ Sex = female", 0.306481792717087,    0.67272268907563, "#0f6333", "Sex = 'female' <br>increases average response <br>by 0.3065"), .Names = c("variable", "contribution","cummulative", "sign", "label")), structure(list("+ Fare = 71",    0.0709915966386554, 0.743714285714286, "#a3142f", "Fare = 71 (low value) <br>increases average response <br>by 0.071"), .Names = c("variable","contribution", "cummulative", "sign", "label")), structure(list(    "+ Pclass = 1", 0.210210084033614, 0.953924369747899, "#0f6333",    "Pclass = 1 (low value) <br>increases average response <br>by 0.2102"), .Names = c("variable", "contribution", "cummulative","sign", "label")), structure(list("+ Embarked = C", 0.0145014005602241,    0.968425770308123, "#a3142f", "Embarked = 'C' <br>decreases average response <br>by 0.02"), .Names = c("variable", "contribution","cummulative", "sign", "label")), structure(list("+ other factors",    0.0154089635854342, 0.983834733893557, "#a3142f", "All other features <br>decrease average response <br>by 0.01"), .Names = c("variable","contribution", "cummulative", "sign", "label"))),
    data = x_as_list,
    script = "breakDownD3.js",
    dependencies = "tooltipD3.js",
    css = "breakDownD3.css",
    options = list(xmin =min_max[1], xmax = min_max[2],
                   model_avg = model_baseline, model_res = model_prediction),
    d3_version = "4"
  )
}
