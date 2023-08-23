#age_preserving_joins
#' Inner join two data frames and filter by age.
#'
#' This function performs an inner join on two data frames, filters based on minimum and maximum age and
#' removes redundant age columns. It allows for the option of specifying the joining type to be a right outer join.
#'
#' @param df1 The first data frame to join.
#' @param df2 The second data frame to join.
#' @param by A vector of variable names to join by.
#' @param right_outer A logical value. If TRUE, the function performs a right outer join.
#' @return A data frame that is the result of joining df1 and df2 and filtered by min_age and max_age.
#'
#' @export
#' @examples
#' df1 <- data.frame(id = c(1, 2, 3), min_age = c(20, 25, 30), max_age = c(40, 45, 50))
#' df2 <- data.frame(id = c(1, 2, 4), min_age = c(30, 35, 40), max_age = c(50, 55, 60))
#' inner_join_age(df1, df2, by = "id", right_outer = TRUE)
inner_join_age <- function(df1, df2, by, right_outer = TRUE, group_column = NULL) {
  if (!all(c("min_age", "max_age") %in% names(df1))) stop("df1 does not have both 'min_age' and 'max_age' columns")
  if (!all(c("min_age", "max_age") %in% names(df2))) stop("df2 does not have both 'min_age' and 'max_age' columns")

  # Perform an inner join on the two data frames.
  df3 <- inner_join(df1,
                    df2,
                    by = by,
                    multiple = "all"
  )

  # Filter and mutate based on the right_outer argument.
  if (right_outer) {
    df3 <- df3 %>%
      filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>% # Keep rows where min_age from df2 is less than or equal to min_age from df1, and max_age from df1 is less than or equal to max_age from df2
      mutate(
        min_age = min_age.y, max_age = max_age.y # Assign min_age from df2 to min_age and max_age from df2 to max_age
      )
  } else {
    df3 <- df3 %>%
      filter(min_age.x <= min_age.y & max_age.y <= max_age.x) %>% # Keep rows where min_age from df1 is less than or equal to min_age from df2, and max_age from df2 is less than or equal to max_age from df1
      mutate(
        min_age = min_age.x, max_age = max_age.x # Assign min_age from df1 to min_age and max_age from df1 to max_age
      )
  }

  # Remove redundant columns.
  df3 <- df3 %>%
    select(-c(min_age.y, min_age.x, max_age.x, max_age.y)) # Drop unnecessary age columns

  if (!is.null(group_column)) {
    df3 <- df3 %>%
      group_and_sum(group_column)
  }

  return(df3)
}

#' Inner join two data frames using right outer join and filter by age.
#'
#' This function is a wrapper for the \code{\link{inner_join_age}} function with the \code{right_outer} parameter set to TRUE.
#'
#' @param df1 The first data frame to join.
#' @param df2 The second data frame to join.
#' @param by A vector of variable names to join by.
#' @return A data frame that is the result of joining df1 and df2 using a right outer join and filtered by min_age and max_age.
#'
#' @export
#' @examples
#' df1 <- data.frame(id = c(1, 2, 3), min_age = c(20, 25, 30), max_age = c(40, 45, 50))
#' df2 <- data.frame(id = c(1, 2, 4), min_age = c(30, 35, 40), max_age = c(50, 55, 60))
#' inner_join_age_right_outer(df1, df2, by = "id")
inner_join_age_right_outer <- function(df1, df2, by, group_column = NULL) {
  inner_join_age(df1, df2, by, right_outer = TRUE, group_column = group_column)
}

#' Inner join two data frames using left outer join and filter by age.
#'
#' This function is a wrapper for the \code{\link{inner_join_age}} function with the \code{right_outer} parameter set to FALSE.
#'
#' @param df1 The first data frame to join.
#' @param df2 The second data frame to join.
#' @param by A vector of variable names to join by.
#' @return A data frame that is the result of joining df1 and df2 using a left outer join and filtered by min_age and max_age.
#'
#' @export
#' @examples
#' df1 <- data.frame(id = c(1, 2, 3), min_age = c(20, 25, 30), max_age = c(40, 45, 50))
#' df2 <- data.frame(id = c(1, 2, 4), min_age = c(30, 35, 40), max_age = c(50, 55, 60))
#' inner_join_age_left_outer(df1, df2, by = "id")
inner_join_age_left_outer <- function(df1, df2, by, group_column = NULL) {
  inner_join_age(df1, df2, by, right_outer = FALSE, group_column = group_column)
}

#' Group and Sum Specified Columns in a Data Frame
#'
#' This function groups the input data frame by all columns except the ones specified
#' in `columns_to_sum` and `columns_to_not_group`, then sums up the values in the specified columns.
#'
#' @param df A data frame.
#' @param columns_to_sum A character vector of column names in `df` to be summed.
#' @param columns_to_not_group An optional character vector of column names in `df` not to be grouped.
#'   Default is `NULL`.
#'
#' @return A grouped and summarized data frame.
#' @examples
#' df <- data.frame(
#'   A = c("x", "x", "y", "y"),
#'   B = c("a", "a", "b", "b"),
#'   value1 = c(1, 2, 3, 4),
#'   value2 = c(5, 6, 7, 8)
#' )
#' result <- group_and_sum(df, columns_to_sum = c("value1", "value2"), columns_to_not_group = c("B"))
#' print(result)
#'
#' @export

group_and_sum <- function(df, columns_to_sum, columns_to_not_group = NULL) {

  # Combine columns_to_sum and columns_to_not_group
  columns_to_exclude <- unique(c(columns_to_sum, columns_to_not_group))

  # Identify missing columns
  missing_columns <- setdiff(columns_to_exclude, colnames(df))

  # Check if there are any missing columns
  if (length(missing_columns) > 0) {
    stop("The following specified columns are not present in the data frame: ",
         paste(missing_columns, collapse = ", "))
  }

  # Group by all columns except those in 'columns_to_exclude'
  df <- df %>%
    group_by_at(vars(one_of(setdiff(colnames(df), columns_to_exclude))))

  # Sum up the specified columns
  summarise_list <- lapply(columns_to_sum, function(col) {
    quo(sum(!!sym(col), na.rm = TRUE))
  })
  names(summarise_list) <- columns_to_sum

  df <- df %>%
    summarise(!!!summarise_list)

  df <- df %>%
    ungroup() %>%
    as.data.frame()
  return(df)
}




