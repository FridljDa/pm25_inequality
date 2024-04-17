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

test_that("has_overlaps works correctly", {

  # Test with overlapping intervals
  df1 <- data.frame(min_age = c(25, 26), max_age = c(26, 27))
  expect_true(has_overlaps(df1))

  # Test with non-overlapping intervals
  df2 <- data.frame(min_age = c(25, 27), max_age = c(26, 28))
  expect_false(has_overlaps(df2))

  # Test with single row
  df3 <- data.frame(min_age = c(25), max_age = c(26))
  expect_false(has_overlaps(df3))

  # Test with empty data.frame
  df4 <- data.frame(min_age = integer(0), max_age = integer(0))
  expect_false(has_overlaps(df4))

  # Test with missing 'min_age' column
  df5 <- data.frame(max_age = c(26, 27))
  expect_error(has_overlaps(df5), "The data.frame must have 'min_age' and 'max_age' columns.")

  # Test with missing 'max_age' column
  df6 <- data.frame(min_age = c(25, 26))
  expect_error(has_overlaps(df6), "The data.frame must have 'min_age' and 'max_age' columns.")

  # Test with unsorted 'min_age'
  df7 <- data.frame(min_age = c(27, 25), max_age = c(28, 26))
  expect_false(has_overlaps(df7))

  # Test with equal 'min_age' and 'max_age'
  df8 <- data.frame(min_age = c(25, 25), max_age = c(25, 25))
  expect_true(has_overlaps(df8))

})
