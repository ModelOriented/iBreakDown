context("Check break_down() function")

# preparation
set.seed(1313)
library("DALEX")
library("iBreakDown")
library("randomForest")
model <- randomForest(status ~ . , data = HR)
new_observation <- HR_test[1,]

explainer_rf <- explain(model,
                         data = HR[1:1000,1:5],
                         y = HR$status[1:1000])

bd_rf <- break_down(explainer_rf, new_observation)
bd_rf_interactions <- break_down(explainer_rf, new_observation, keep_distributions = TRUE, interactions = TRUE)
pl_rf <- plot(bd_rf)

# tests

test_that("Output format", {
  expect_is(bd_rf, "break_down")
  expect_is(bd_rf_interactions, "break_down")
  expect_is(pl_rf, "ggplot")
})

