#' Check if values can be converted to numeric
#'
#' This function checks if all values in a vector can be converted to numeric using as.numeric().
#' If there's at least one value that cannot be converted, it returns FALSE.
#' If all values can be converted, it returns TRUE.
#'
#' @param x A vector of any type.
#'
#' @return A boolean indicating whether all elements can be converted to numeric or not.
#' @export
#'
#' @examples
#' can_be_numeric(c("1", "2", "3")) # Returns: TRUE
#' can_be_numeric(c("1", "2", "b")) # Returns: FALSE
can_be_numeric <- function(x) {
  if (is.numeric(x)) {
    return(TRUE)
  }
  converted <- suppressWarnings(as.numeric(x))
  return(!any(is.na(converted)))
}

#' Convert columns of a dataframe that can be numeric to numeric
#'
#' This function takes a dataframe as input and iterates over each column.
#' If a column can be converted to a numeric type, it performs the conversion.
#'
#' @param df A dataframe with the columns to convert.
#'
#' @return A dataframe with columns converted to numeric type if possible.
#'
#' @examples
#' # create a sample data frame
#' df <- data.frame(a = c("1", "2", "3"), b = c("a", "b", "c"))
#' df <- convert_columns_to_numeric(df)
#'
convert_columns_to_numeric <- function(df) {
  for (column in colnames(df)) {
    if (can_be_numeric(df[[column]])) {
      df[[column]] <- as.numeric(df[[column]])
    }
  }
  return(df)
}

#' Replace specific values in a dataframe
#'
#' This function takes a dataframe and a findreplace dataframe as input. It replaces specific values in the dataframe
#' as defined by the findreplace dataframe.
#'
#' @param df A dataframe with the columns to replace values in.
#' @param findreplace A dataframe defining what values to find and what to replace them with.
#' @param NA_string A unique string to replace NA values with during processing (default is "Unique_NA_String"). Should not be present in df.
#'
#' @return A dataframe with replaced values as defined by the findreplace dataframe.
#'
#' @examples
#' # create a sample data frame
#' df <- data.frame(a = c("1", "2", "3"), b = c("a", "b", "c"))
#' findreplace <- data.frame(replacecolumns = c("a", "b"), from = c("1", "a"), to = c("10", "z"))
#' df <- replace_values(df, findreplace)
#'
replace_values <- function(df, findreplace, NA_string = "Unique_NA_String", silent = TRUE) {
  df <- as.data.frame(df)
  findreplace <- as.data.frame(findreplace)

  findreplace <- findreplace %>%
    select(replacecolumns, from, to) %>%
    distinct()

  # Check if findreplace contains duplicate combinations of replacecolumns and from
  duplicates <- findreplace[duplicated(findreplace[, c("replacecolumns", "from")]), ]

  if (nrow(duplicates) > 0) {
    browser()
    stop(paste(
      "Error: findreplace contains", nrow(duplicates), "rows with the same replacecolumns, from combination.\n",
      "Duplicated rows:\n", capture.output(print(duplicates))
    ))
  }

  if (silent == FALSE) cat("running replace_values", "\n")

  # reduce everything
  df <- convert_columns_to_numeric(df) %>%
    dplyr::mutate(across(everything(), as.character))

  # Replace NA values with the unique string in df and findreplace
  df <- replace(df, is.na(df), NA_string)
  findreplace <- replace(findreplace, is.na(findreplace), NA_string)

  # Pre-filter findreplace
  findreplace <- dplyr::filter(findreplace, replacecolumns %in% colnames(df))

  for (replacecolumn in unique(findreplace$replacecolumns)) {
    if (silent == FALSE) cat("replacing ", replacecolumn, "\n")
    findreplace_column <- dplyr::filter(findreplace, replacecolumns == replacecolumn) %>%
      dplyr::mutate(replacecolumns = NULL)

    # reduce everything
    findreplace_column <- convert_columns_to_numeric(findreplace_column) %>%
      dplyr::mutate(across(everything(), as.character))

    replacement <- df %>%
      dplyr::select(all_of(replacecolumn)) %>%
      dplyr::left_join(findreplace_column,
        by = setNames("from", replacecolumn)
      )

    missing <- replacement %>%
      dplyr::filter(is.na(to)) %>%
      dplyr::distinct()

    if (nrow(missing) > 0) {
      print(paste("no value assigned in", replacecolumn, "for"))
      print(missing[, 1] %>% unique())
    }

    # if(silent == FALSE){
    #  print("replacement")
    #  print(head(replacement))
    #  print("df")
    #  print(head(df))
    # }
    df[, replacecolumn] <- dplyr::coalesce(replacement$to, df[, replacecolumn])
  }

  # Convert the unique string back to NA
  df <- replace(df, df == NA_string, NA)
  df <- convert_columns_to_numeric(df)

  return(df)
}

#' Replace missing values in a vector
#'
#' This function replaces missing values in the first vector (`vec1`) with corresponding values from a second vector (`vec2`),
#' if the corresponding values are not missing.
#' If no missing values are detected in `vec1`, the function returns `vec1` without performing any further operations.
#'
#' @param vec1 A vector in which missing values should be replaced.
#' @param vec2 A vector from which replacement values should be taken.
#' @param is_missing A function that identifies missing values.
#' @param vec_name An optional name for the vectors, used in printed messages. Default is NULL.
#'
#' @return A vector with missing values replaced where possible.
#' @export
#'
#' @examples
#' vec1 <- c(1, NA, 3, NA, 5)
#' vec2 <- c(2, 4, 6, 8, 10)
#' replace_missing_vectors(vec1, vec2, is.na)
replace_missing_vectors <- function(vec1, vec2, is_missing, vec_name = NULL) {
  # Check if there are any missing values in vec1
  missing_indexes_vec1 <- sapply(vec1, is_missing)

  # If there are no missing values, return vec1 without performing any further operations
  if (!any(missing_indexes_vec1)) {
    cat(if (!is.null(vec_name)) paste(vec_name, ": ") else "", "No missing values detected. No replacements made.\n")
    return(vec1)
  }

  # If there are missing values, continue with the function as normal
  missing_indexes_vec2 <- sapply(vec2, is_missing)

  # Only keep indices where vec1 is missing but vec2 is not
  valid_indexes <- missing_indexes_vec1 & !missing_indexes_vec2

  # Calculate the proportion of values that will be replaced
  prop_replaced <- round(sum(valid_indexes) / length(vec1) * 100, 2)

  # Create a prefix for the message based on whether a vec_name was provided
  msg_prefix <- if (!is.null(vec_name)) paste(vec_name, ": ") else ""

  # Print the proportion of values replaced, incorporating the vec_name if provided
  cat(msg_prefix, "Proportion of values replaced: ", prop_replaced, "%\n")

  # Create a copy of the vector to avoid modifying the original one
  vec_replaced <- vec1

  # Replace the missing values in vec1 with the corresponding values in vec2
  vec_replaced[valid_indexes] <- vec2[valid_indexes]

  # Calculate the proportion of values that are still missing after replacement
  missing_after_replacement <- round(sum(sapply(vec_replaced, is_missing)) / length(vec_replaced) * 100, 2)

  # Print the proportion of missing values after replacement, incorporating the vec_name if provided
  cat(msg_prefix, "Proportion of values missing after replacement: ", missing_after_replacement, "%\n")

  return(vec_replaced)
}

#' Find Rows with NA Values in a Dataframe
#'
#' This function returns rows with at least one NA value from a provided dataframe.
#'
#' @param df A dataframe
#'
#' @return A dataframe with rows containing at least one NA value.
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 6, 7, 8))
#' find_na_rows(df)
find_na_rows <- function(df) {
  rows_with_na <- df[apply(is.na(df), 1, any), ]
  return(rows_with_na)
}

#' Group, Summarize and Add New Column
#'
#' This function groups a dataframe by all columns except the specified one,
#' sums the 'Deaths' column, and adds a new column with a provided name and value.
#'
#' @param df A dataframe
#' @param column A character string specifying the column name to exclude from grouping and to add
#' @param new_col_value A character string specifying the value to fill the new column
#'
#' @return A dataframe summarized and with the new column added.
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3, 4), b = c(5, 6, 7, 8), Deaths = c(10, 20, 30, 40))
#' group_summarize_add_column(df, column = "b", new_col_value = "A")
group_summarize_add_column <- function(df, column, new_col_value) {
  df %>%
    group_by_at(setdiff(colnames(df), c(column, "Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate({{ column }} := new_col_value)
}

#' Read Data from CSV file
#'
#' This function reads data from a CSV file at a given path. It specifies column types
#' for the data to ensure it is read correctly. This function uses the readr package
#' to read the CSV.
#'
#' @param path A string. The path to the CSV file to read.
#'
#' @return A data frame with columns: Year, min_age, max_age, county, rural_urban_class,
#' Deaths, Education, Race. The "Race" column is read as a factor with specific levels.
#'
#' @examples
#' df <- read_data("path/to/your/file.csv")
#'
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @importFrom readr col_integer
#' @importFrom readr col_factor
read_data <- function(path) {
  col_types_data <- cols(
    Year = col_integer(),
    min_age = col_integer(), max_age = col_integer(),
    county = col_integer(), rural_urban_class = col_integer(),
    Deaths = col_integer(), Education = col_integer(),
    Race = col_factor(levels = c(
      "All",
      "American Indian or Alaska Native",
      "Asian or Pacific Islander",
      "Black or African American",
      "White",
      "White, Hispanic or Latino",
      "White, Not Hispanic or Latino"
    ))
  )

  df <- readr::read_csv(path,
                        col_types = col_types_data
  )
  return(df)
}

#' Run script based on system type
#'
#' This function runs a given R script file with optional arguments, using different settings depending on the current system's type.
#' The function will also print which script was run and how long it took.
#'
#' @param script A string specifying the name/path of the script to be run.
#' @param args A string specifying the arguments to be passed to the script. Default is "" (no arguments).
#'
#' @return NULL. This function is mainly called for its side effects (running the script and printing the results).
#' @examples
#' \dontrun{
#'   run_script("my_script.R")
#'   run_script("my_script.R", "--arg value")
#' }
#' @export
run_script <- function(script, args = "") {
  sysname <- Sys.info()["sysname"]

  # Record the start time
  start_time <- proc.time()

  if (sysname == "Darwin") {
    print(paste("Running script", script, "on Darwin"))
    system(paste("Rscript", script, args))
  } else if (sysname == "Windows") {
    memory.limit(size = 500000)

    exec <- paste0("C:/Program Files/R/R-", R.Version()$major, ".", R.Version()$minor, "/bin/Rscript.exe")
    exec <- shQuote(exec)

    print(paste("Running script", script, "on Windows"))
    system(paste(exec, "--vanilla", script, args))
  } else {
    print(paste("Running script", script, "on", sysname))
    system(paste("Rscript", script, args))
  }

  # Record the end time
  end_time <- proc.time()

  # Calculate and print the time difference
  print(paste("Time taken to run script", script, ":", as.numeric(end_time - start_time, "secs"), "seconds"))
}

#' Create directory if it does not exist
#'
#' This function checks if a directory at the specified path exists.
#' If it does not exist, the function creates it.
#'
#' @param dir_path A character string specifying the path to the directory to be created.
#'
#' @return Invisible NULL. The function is called for its side effect of creating a directory.
#'
#' @examples
#' \dontrun{
#' create_directory("path/to/your/directory")
#' }
#' @export
create_directory <- function(dir_path) {
  if (!file.exists(dir_path)) {
    dir.create(dir_path)
  }
  invisible(NULL)
}

#' Search for Files in a Directory
#'
#' This function takes a directory path and a string, and returns the path to all files
#' and directories in that path, where the name contains the string.
#' Optionally, it can also return only those files and directories whose names end with the string.
#'
#' @param directory A string specifying the path to the directory.
#' @param string The string to search for in the file and directory names.
#' @param ends_with A logical value. If TRUE, only files and directories whose names
#' end with the specified string are returned. If FALSE (the default), files and directories
#' whose names contain the string anywhere are returned.
#'
#' @return A character vector containing the full paths to the matching files and directories.
#' @examples
#' \dontrun{
#' search_files("path/to/directory", "test")
#' search_files("path/to/directory", "test", ends_with = TRUE)
#' }
#' @export
search_files <- function(directory, string, ends_with = FALSE) {

  # Ensure the directory exists before proceeding
  if(!dir.exists(directory)){
    stop(paste("Directory", directory, "does not exist."))
  }

  # Get all file and directory names in the directory
  all_files <- list.files(directory, full.names = TRUE, recursive = FALSE)

  # Modify the string if ends_with = TRUE
  if(ends_with) {
    string <- paste0(string, "$")
  }

  # Filter files and directories whose names contain the specified string
  matching_files <- all_files[grepl(string, basename(all_files))]

  return(matching_files)
}

