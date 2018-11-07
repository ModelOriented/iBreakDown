context("Check break_interactions() function for model or explainer")

# preparation
library(DALEX)
set.seed(1313)
# example with interaction
# classification for HR data
m_rf_class <- randomForest::randomForest(status ~ . , data = HR)
new_observation <- HRTest[1,]

exp_rf_class <- explain(m_rf_class,
                        data = HR[1:1000,1:5],
                        y = HR$status[1:1000])

bd_rf_class <- local_interactions(exp_rf_class,
                                  new_observation)

bd_rf_class_distr <- local_interactions(exp_rf_class,
                                        new_observation,
                                        keep_distributions = TRUE)

# example for regression - apartment prices
# here we do not have intreactions
m_rf_reg <- randomForest::randomForest(m2.price ~ . , data = apartments)
exp_rf_reg <- explain(m_rf_reg,
                      data = apartmentsTest[1:1000,2:6],
                      y = apartmentsTest$m2.price[1:1000])

bd_rf_reg <- local_interactions(exp_rf_reg,
                                apartmentsTest[1,])

bd_rf_reg_distr <- local_interactions(exp_rf_reg,
                                      apartmentsTest[1,],
                                      keep_distributions = TRUE)

# tests

test_that("Output format", {
  expect_is(bd_rf_class, "break_down")
  expect_is(bd_rf_class_distr, "break_down")
  expect_is(bd_rf_reg, "break_down")
  expect_is(bd_rf_reg_distr, "break_down")
})

test_that("Error when no distribution before provided", {
  expect_error(local_interactions())
})
