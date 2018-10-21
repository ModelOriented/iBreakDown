# Clean the object of the break_down class
# Internal function
# @param var_contributions list with variable contributions
# @param baseline level on which the baseline line shall be plotted
#
# @return enriched break_down class
#
create.break_down <- function(var_contributions, baseline = 0) {
  broken_cumm <- data.frame(var_contributions,
                            cummulative = cumsum(as.numeric(var_contributions$contribution)),
                            sign = factor(sign(as.numeric(var_contributions$contribution)), levels = c(-1, 0, 1)),
                            position = seq_along(var_contributions$variable))
  broken_cumm <- rbind(broken_cumm,
                       data.frame(variable = "prediction",
                                  contribution = sum(broken_cumm$contribution),
                                  variable_name = "",
                                  variable_value = "",
                                  cummulative = sum(broken_cumm$contribution),
                                  sign = "X",
                                  position = max(broken_cumm$position)+1))
 attr(broken_cumm, "baseline") <- baseline
 class(broken_cumm) <- "break_down"
 broken_cumm
}
