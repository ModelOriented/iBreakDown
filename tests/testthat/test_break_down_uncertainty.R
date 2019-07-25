context("Check break_down_uncertainty() function")

library("DALEX")
library("iBreakDown")
library("randomForest")
set.seed(1313)

model <- randomForest(status ~ . , data = HR)
new_observation <- HR_test[1,]

explainer_rf <- explain(model,
                        data = HR[1:1000,1:5],
                        y = HR$status[1:1000])

bd_rf_A <- break_down_uncertainty(explainer_rf,
                                        new_observation,
                                        path = c(3,2,4,1,5))
pl_A <- plot(bd_rf_A)

# example for regression - apartment prices
# here we do not have intreactions
model <- randomForest(m2.price ~ . , data = apartments)
explainer_rf <- explain(model,
                        data = apartments_test[1:1000,2:6],
                        y = apartments_test$m2.price[1:1000])

bd_rf_B <- break_down_uncertainty(explainer_rf,
                                        apartments_test[1,],
                                        path = c("floor", "no.rooms", "district", "construction.year", "surface"))
pl_B <- plot(bd_rf_B)

bd_rf_C <- shap(explainer_rf,
                apartments_test[1,])
pl_C <- plot(bd_rf_C, show_boxplots = FALSE)

bd_rf_D <- shap(explainer_rf,
                apartments_test[1,], keep_distribution = FALSE)
pl_D <- plot(bd_rf_D, show_boxplots = FALSE)

# tests

test_that("Output format", {
  expect_is(bd_rf_A, "break_down_uncertainty")
  expect_is(bd_rf_B, "break_down_uncertainty")
  expect_is(bd_rf_C, "break_down_uncertainty")
  expect_is(pl_A, "ggplot")
  expect_is(pl_B, "ggplot")
  expect_is(pl_C, "ggplot")
})

