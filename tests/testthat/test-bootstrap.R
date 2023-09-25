test_that("calculate_weighted_mean_ci works correctly", {

  # Test 1: Basic functionality
  pm <- c(1, 2, 3, 4)
  pop_size <- c(10, 20, 30, 40)
  result <- calculate_weighted_mean_ci(pm, pop_size, R = 10)
  expect_equal(length(result), 3)
  expect_true(is.numeric(result$pop_weight_pm_exp))
  expect_true(is.numeric(result$lower))
  expect_true(is.numeric(result$upper))

  # Test 2: When all bootstrap resampled means are identical
  pm <- rep(10, 4)
  pop_size <- rep(10, 4)
  result <- calculate_weighted_mean_ci(pm, pop_size, R = 10)
  expect_equal(result$lower, result$upper)
  expect_equal(result$lower, result$pop_weight_pm_exp)

  # Test 3: Check if confidence intervals are reasonable
  pm <- c(1, 2, 3, 4)
  pop_size <- c(10, 20, 30, 40)
  result <- calculate_weighted_mean_ci(pm, pop_size, R = 10)
  expect_lte(result$lower, result$pop_weight_pm_exp)
  expect_gte(result$upper, result$pop_weight_pm_exp)

})
