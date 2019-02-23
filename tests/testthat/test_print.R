context("Check print() function for break_down object")

library("DALEX")
library("iBreakDown")
library("randomForest")
set.seed(1313)
model <- randomForest(Petal.Length ~ . , data = iris)
new_observation <- iris[1,]
explainer_rf <- explain(model,
                       data = iris,
                       y = iris$Petal.Length)
bd_rf <- local_attributions(explainer_rf,
                          new_observation)

test_that("Test output",{
  expect_is(print(bd_rf), "NULL")
})
