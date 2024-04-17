library(testthat)
library(dplyr)
library(tidyr)

test_that("test join filter age", {
  df1 <- data.frame(
    join = 1,
    id = c(1, 2, 3),
    min_age = c(5, 6, 4),
    max_age = c(10, 9, 11)

  )

  df2 <- data.frame(
    join = 1,
    id = c(1, 2, 3),
    min_age = c(5, 6, 4),
    max_age = c(10, 9, 11)
  )

  # Test: Correctly joined data and adjusted age ranges based on right_outer
  result <- inner_join_age(df1, df2, by = "join")
  expect_equal(nrow(result), 6)
  for(i in seq_along(nrow(result))){
    result_i <- result[i,]
    expect_lte(df1[result_i$id.x, "min_age"], df2[result_i$id.y, "min_age"])
    expect_lte(df2[result_i$id.y, "max_age"], df1[result_i$id.x, "max_age"])
    expect_equal(result_i$min_age, df1[result_i$id.x, "min_age"])
    expect_equal(result_i$max_age, df1[result_i$id.x, "max_age"])
  }

  # Test: If right_outer is FALSE, adjust age ranges accordingly
  result <- inner_join_age(df1, df2, by = "join", right_outer = FALSE)
  expect_equal(nrow(result), 6)
  for(i in seq_along(nrow(result))){
    result_i <- result[i,]
    expect_lte(df2[result_i$id.y, "min_age"], df1[result_i$id.x, "min_age"])
    expect_lte(df1[result_i$id.x, "max_age"], df2[result_i$id.y, "max_age"])
    expect_equal(result_i$min_age, df2[result_i$id.y, "min_age"])
    expect_equal(result_i$max_age, df2[result_i$id.y, "max_age"])
  }

  # Test: Check if an error is thrown when one of the dataframes doesn't have min_age and max_age columns
  df3 <- data.frame(
    id = c(1, 2, 3),
    min_val = c(5, 10, 15),
    max_val = c(10, 20, 25)
  )
  expect_error(inner_join_age(df3, df2, by = "id"), "df1 does not have both 'min_age' and 'max_age' columns")
  expect_error(inner_join_age(df1, df3, by = "id"), "df2 does not have both 'min_age' and 'max_age' columns")

})
