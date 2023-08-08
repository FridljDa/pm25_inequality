test_that("group_and_sum function works correctly", {

  # Sample data
  df <- data.frame(
    A = c("x", "x", "y", "y"),
    B = c("a", "a", "b", "b"),
    value1 = c(1, 2, 3, 4),
    value2 = c(5, 6, 7, 8)
  )

  # Expected result after grouping by 'A' and 'B' and summing 'value1' and 'value2'
  expected <- data.frame(
    A = c("x", "y"),
    B = c("a", "b"),
    value1 = c(3, 7),
    value2 = c(11, 15)
  )

  # Test
  result <- group_and_sum(df, columns_to_sum = c("value1", "value2"))
  expect_equal(result, expected)

  # Test with columns_to_not_group
  result2 <- group_and_sum(df, columns_to_sum = c("value1", "value2"), columns_to_not_group = c("B"))
  expected2 <- data.frame(
    A = c("x", "y"),
    value1 = c(3, 7),
    value2 = c(11, 15)
  )
  expect_equal(result2, expected2)


  result3 <- group_and_sum(df, columns_to_sum = c("value2"))
  expect_equal(result3, df)

  # Test for missing columns
  expect_error(group_and_sum(df, columns_to_sum = c("value1", "value3")),
               "The following specified columns are not present in the data frame: value3")

})


