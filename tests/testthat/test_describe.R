context("Check describe() function")

library("DALEX")
library("randomForest")
library("iBreakDown")

titanic <- na.omit(titanic)
model_titanic_rf <- randomForest(survived == "yes" ~ gender + age + class + embarked +
                                  fare + sibsp + parch,  data = titanic)

explain_titanic_rf <- explain(model_titanic_rf,
                              data = titanic[,-9],
                              y = titanic$survived == "yes",
                              label = "Random Forest v7")
random_passanger <- titanic[sample(nrow(titanic),1),c(1,2,3,4,6,7,8)]


rf_la <- break_down(explain_titanic_rf, random_passanger, keep_distributions = TRUE)
test_that("Output format", {
  expect_is(iBreakDown::describe(rf_la), "break_down_description")
})

rf_la <- break_down(explain_titanic_rf, random_passanger, keep_distributions = FALSE)
test_that("Output format", {
  expect_is(iBreakDown::describe(rf_la), "break_down_description")})


test_that("Output format", {
  expect_error(describe(rf_la, display_values = 4))})

n <- 4
test <- expand.grid(replicate(n, c(TRUE,FALSE), simplify = FALSE))
test_result <- apply(test, MARGIN = 1, function(x){
  random_passanger <- titanic[sample(nrow(titanic),1),c(1,2,3,4,6,7,8)]
  rf_la <- break_down(explain_titanic_rf, random_passanger, keep_distributions = TRUE)
  description <- iBreakDown::describe(x = rf_la,
                                      label = "the passanger will survive with probability",
                                      short_description = x[[1]],
                                      display_values =  x[[2]],
                                      display_numbers = x[[3]],
                                      display_distribution_details = x[[4]])
  test_that("Output format", {
    expect_is(description, "break_down_description")
  })
})

n <- 4
test <- expand.grid(replicate(n, c(TRUE,FALSE), simplify = FALSE))
test_result_shap <- apply(test, MARGIN = 1, function(x){
  random_passanger <- titanic[sample(nrow(titanic),1),c(1,2,3,4,6,7,8)]
  rf_la_shap <- shap(explain_titanic_rf, random_passanger, B = 2)
  description <- iBreakDown::describe(x = rf_la_shap,
                                      label = "the passanger will survive with probability",
                                      short_description = x[[1]],
                                      display_values =  x[[2]],
                                      display_numbers = x[[3]],
                                      display_distribution_details = x[[4]],
                                      display_shap = TRUE)
  test_that("Output format", {
    expect_is(description, "break_down_description")
  })
})

## test for less than 3 variables
titanic_small <- titanic[sample(1:nrow(titanic), 500),]


model_small <- glm(survived == "yes" ~ age + gender,
                   data = titanic_small[, c(1,2,9)],
                   family = "binomial")

explain_model_small <- explain(model_small,
                               data = titanic_small[, c(1,2)],
                               y = titanic_small$survived == "yes")


shap_small <- shap(explain_model_small, new_observation = titanic_small[1,c(1,2)])
bd_small <- break_down(explain_model_small, new_observation = titanic_small[1,c(1,2)])

test_that("Output format", {
  expect_is(iBreakDown::describe(shap_small, display_numbers = TRUE,
                                 display_values = TRUE, display_shap = TRUE)
            ,"break_down_description")
})

test_that("Output format", {
  expect_is(iBreakDown::describe(bd_small, display_numers = TRUE,
                                 display_values = TRUE)
            ,"break_down_description")
})

