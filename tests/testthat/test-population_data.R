# Create a unit test
test_that("is_partition function works as expected", {
  df1 <- data.frame(min_age = c(25, 30, 35, 40),
                    max_age = c(29, 34, 39, 44))

  df2 <- data.frame(min_age = c(25, 35, 45),
                    max_age = c(29, 44, 54))

  df3 <- data.frame(min_age = c(25, 30, 35),
                    max_age = c(28, 34, 39))

  df4 <- data.frame(min_age = c(25, 30, 35),
                    max_age = c(28, 34, 34))

  expect_true(is_partition(df1))
  expect_false(is_partition(df2))
  expect_false(is_partition(df3))
  expect_false(is_partition(df4))
})
