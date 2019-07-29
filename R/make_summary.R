#' Makes a summary for the description
#' @param explainer an iBreakDown explainer
#' @param display_argumentation displays argumentation mode
#' @return a summary


make_summary <- function(explainer, display_argumentation){
  if(display_argumentation == 1){
    # We make the same df as during argument selection
    df <- explainer[-c(1,dim(explainer)[1]), c("contribution","variable")]
    df['importance'] <- abs(df$contribution)
    df <- df[order(df$importance, decreasing = TRUE), ]
    other_importance <- sum(df[-c(1,2,3),"contribution"])

    summary <- paste0("Other variables are with less importance. ",
                      "The contribution of all other variables is ",
                      round(other_importance,3),
                      " .")
  }

}
