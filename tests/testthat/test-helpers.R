
# Define test cases for the replace_missing function
test_that("replace_missing_vectors correctly replaces missing values", {
  is_na <- function(x) { return(is.na(x)) }
  vec1 <- c(1, NA, 3, NA, 5)
  vec2 <- c(NA, 2, NA, 4, NA)

  # Expected output after replacing NA's in vec1 with corresponding non-NA's in vec2
  expected_output <- c(1, 2, 3, 4, 5)

  test_output <- replace_missing_vectors(vec1, vec2, is_na)

  # Test if the output matches the expected output
  expect_equal(test_output, expected_output)

  # Test if the output still contains NA's
  expect_false(any(is.na(test_output)))

  # Test if the function works with all NA's in vec2
  vec2_all_na <- c(NA, NA, NA, NA, NA)
  output_with_all_na <- replace_missing_vectors(vec1, vec2_all_na, is_na)

  # In this case, the output should match the original vec1, as there are no valid replacements in vec2_all_na
  expect_equal(output_with_all_na, vec1)
})


test_that("Replacement of NA values is successful", {
  df <- data.frame(col1 = c(1, 2, NA, 4), col2 = c("a", "b", NA, "d"), stringsAsFactors = FALSE)
  findreplace <- data.frame(replacecolumns = c("col1", "col2"), from = c(NA, NA), to = c(666, "missing"), stringsAsFactors = FALSE)
  df_replaced <- replace_values(df, findreplace)

  expect_true(all(!is.na(df_replaced)))
  expect_equal(df_replaced$col1[3], 666)    # Expecting numeric value 666 instead of "missing"
  expect_equal(df_replaced$col2[3], "missing")
})


test_that("Value replacement is successful", {
  df <- data.frame(col1 = c(1, 2, 3, 4), col2 = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  findreplace <- data.frame(replacecolumns = c("col1", "col2"), from = c(1, "a"), to = c(100, "z"), stringsAsFactors = FALSE)
  df <- replace_values(df, findreplace)

  expect_equal(df$col1[1], 100)
  expect_equal(df$col2[1], "z")
})

test_that("No replacement when 'from' value is not in the column", {
  df <- data.frame(col1 = c(1, 2, 3, 4), col2 = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  findreplace <- data.frame(replacecolumns = c("col1", "col2"), from = c(100, "z"), to = c(1000, "zz"), stringsAsFactors = FALSE)
  df <- replace_values(df, findreplace)

  expect_equal(df$col1[1], 1)  # unchanged
  expect_equal(df$col2[1], "a")  # unchanged
})

