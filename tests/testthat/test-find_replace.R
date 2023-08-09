library(testthat)

test_that("replace_values behaves correctly", {
  # Create a sample data frame
  df <- data.frame(a = c("1", "2", "3"), b = c("a", "b", "c"))

  # Create a findreplace dataframe to define the replacements
  findreplace <- data.frame(replacecolumns = c("a", "b"), from = c("1", "a"), to = c("10", "z"))

  # Expected result after replacing values
  expected_output <- data.frame(a = c(10, 2, 3), b = c("z", "b", "c"))

  # Run the replace_values function
  result <- replace_values(df, findreplace)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)

  # Check error handling when required columns are missing
  findreplace_missing_columns <- data.frame(replacecolumns = c("a"), from = c("1"))
  expect_error(replace_values(df, findreplace_missing_columns), "missing the required columns")

  # Check error handling when there are duplicate combinations of replacecolumns and from
  findreplace_duplicate <- data.frame(replacecolumns = c("a", "a"), from = c("1", "1"), to = c("10", "20"))
  expect_error(replace_values(df, findreplace_duplicate))

  # Check warning handling when no value is assigned
  findreplace_missing_value <- data.frame(replacecolumns = c("a", "b"), from = c("1", "x"), to = c("10", "z"))
  expect_warning(replace_values(df, findreplace_missing_value))
})
