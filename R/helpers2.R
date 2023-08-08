
library(dplyr)

#' Check if a Package is Used in an R Script
#'
#' This function checks if functions from a specified package are used in the given R script.
#' It returns the names of functions from the package that are used in the script, or NULL if none are used.
#' Additionally, a message indicating the usage status will be displayed.
#'
#' @param path_to_script A string providing the path to the R script.
#' @param pkg A string indicating the name of the package to check.
#'
#' @return A character vector with the names of the functions from the package used in the script,
#' or NULL if none are used.
#' @examples
#' \dontrun{
#' result <- check_package_usage("path_to_your_script.R", "dplyr")
#' cat(result)
#' }
#' @export
check_package_usage <- function(path_to_script, pkg) {

  # Helper function to get functions from a package
  get_package_functions <- function(package_name) {
    ls(name = asNamespace(package_name))
  }

  # Load the package functions without attaching it
  pkg_funcs <- get_package_functions(pkg)

  # Read the R script
  script_content <- readLines(path_to_script)

  # Check for usage
  used_funcs <- sapply(pkg_funcs, function(func) {
    pattern <- paste0("\\b", func, "\\b")
    any(grepl(pattern, script_content))
  })

  # Return the functions used or a message indicating no usage
  if (length(used_funcs) > 0) {
    used_functions <- names(used_funcs[used_funcs])
    message("The following functions from the package are used in the script:\n")
    return(paste(used_functions, collapse = ", "))
  } else {
    message("No functions from the specified package are used in the script.")
    return(NULL)
  }
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



