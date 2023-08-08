
library(dplyr)

#' Check Package Usage in an R Script
#'
#' This function checks which functions from a given set of packages are used in an R script.
#' It takes the path to the R script and a vector of package names, then returns the packages that are actually used.
#'
#' @param path_to_script A string representing the path to the R script.
#' @param pkgs A character vector containing the names of the packages to check.
#'
#' @return A character vector containing the names of the packages that are used in the script, or NULL if none of the specified packages are used.
#' @examples
#' \dontrun{
#'   path <- "path/to/your/script.R"
#'   packages <- c("dplyr", "ggplot2", "tidyr")
#'   result <- check_package_usage(path, packages)
#'   print(result)
#' }
#' @export
check_package_usage <- function(path_to_script, pkgs) {

  # Helper function to get functions from a package
  get_package_functions <- function(package_name) {
    ls(name = asNamespace(package_name))
  }

  # Read the R script
  script_content <- readLines(path_to_script)

  # Store packages that are used
  packages_used <- c()

  # Iterate over packages
  for (pkg in pkgs) {
    # Load the package functions without attaching it
    pkg_funcs <- get_package_functions(pkg)

    # Check for usage
    used_funcs <- sapply(pkg_funcs, function(func) {
      pattern <- paste0("\\b", func, "\\b")
      any(grepl(pattern, script_content))
    })

    # Check if any functions were used
    if (any(used_funcs)) {
      packages_used <- c(packages_used, pkg)
    }
  }

  # Return the packages used or a message indicating no usage
  if (length(packages_used) > 0) {
    message("The following packages are used in the script:")
    return(packages_used)
  } else {
    message("No functions from the specified packages are used in the script.")
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



