context("Check plot() function for broken object")

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

bd_rf_class <- local_attributions(exp_rf_class,
                                  new_observation)

bd_rf_class_distr <- local_attributions(exp_rf_class,
                                        new_observation,
                                        keep_distributions = TRUE)

# example for regression - apartment prices
# here we do not have intreactions
m_rf_reg <- randomForest::randomForest(m2.price ~ . , data = apartments)
exp_rf_reg <- explain(m_rf_reg,
                      data = apartmentsTest[1:1000,2:6],
                      y = apartmentsTest$m2.price[1:1000])

bd_rf_reg <- local_attributions(exp_rf_reg,
                                apartmentsTest[1,])

bd_rf_reg_distr <- local_attributions(exp_rf_reg,
                                      apartmentsTest[1,],
                                      keep_distributions = TRUE)

# tests

test_that("Output format", {
  expect_is(plot(bd_rf_class), "gg")
  expect_is(plot(bd_rf_class, start_baseline = TRUE), "gg")
  expect_is(plot(bd_rf_class_distr, plot_distributions = TRUE), "gg")
  expect_is(plot(bd_rf_reg), "gg")
  expect_is(plot(bd_rf_reg, max_features = 2), "gg")
  expect_is(plot(bd_rf_reg, start_baseline = TRUE), "gg")
  expect_is(plot(bd_rf_reg_distr, plot_distributions = TRUE), "gg")
})

test_that("Error when no distribution before provided", {
  expect_error(plot(bd_rf_class, plot_distributions = TRUE))
  expect_error(plot(bd_rf_reg, plot_distributions = TRUE))
})

test_that("Test plot3D", {
  expect_is(plotD3(bd_rf_class), "r2d3")
  expect_is(plotD3(bd_rf_class, max_features = 10), "r2d3")
  expect_is(plotD3(bd_rf_class, max_features = 2, min_max = c(0,1)), "r2d3")
})

