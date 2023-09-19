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

#' Simplify Data Frame Columns
#'
#' This function iterates through the columns of the provided data frame,
#' attempting to convert character columns to numeric if possible, and then back to character.
#' If the column is a factor, it will be converted to a character column.
#'
#' @param df A data frame containing the columns to be simplified.
#'
#' @return A data frame with character columns that have been simplified based on the described rules.
#'
#' @examples
#' df <- data.frame(a = c("1", "2", "3"), b = as.factor(c("a", "b", "c")))
#' df <- simplify_columns_df(df)
#'
simplify_columns_df <- function(df, columns = colnames(df)) {
  #TODO check that all columns present in df
  for (column in columns) {
    # Try to convert character columns to numeric if possible
    if (is.character(df[[column]]) && can_be_numeric(df[[column]])) {
      # Convert from character to numeric and then back to character
      df[[column]] <- df[[column]] %>%
        as.numeric() %>%
        as.character()
    } else if (is.factor(df[[column]])) { # for factor columns
      # Convert factor columns to character
      df[[column]] <- df[[column]] %>%
        as.character()
    } else if (is.numeric(df[[column]])) { # for factor columns
      # Convert factor columns to character
      df[[column]] <- df[[column]] %>%
        as.character()
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
#'                    It must contain three columns:
#'                    - `replacecolumns`: the names of the columns in `df` to replace values in
#'                    - `from`: the values to be replaced in the specified columns
#'                    - `to`: the new values that will replace the existing values in the specified columns
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
replace_values <- function(df, findreplace, keep_value_if_missing = TRUE, NA_string = "Unique_NA_String", silent = TRUE) {
  df <- as.data.frame(df)
  findreplace <- as.data.frame(findreplace)

  # Check if the column is not of type numeric, factor, or character
  for (column in colnames(df)) {
    if (!any(sapply(df[[column]], function(x) is.numeric(x) || is.factor(x) || is.character(x)))) {
      stop(paste("Error: Column", column, "is not of type numeric, factor, or character."))
    }
  }

  # Check if the required columns are present in findreplace
  required_columns <- c("replacecolumns", "from", "to")
  missing_columns <- setdiff(required_columns, names(findreplace))
  if (length(missing_columns) > 0) {
    stop(paste("Error: findreplace is missing the required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Check if findreplace contains duplicate combinations of replacecolumns and from
  duplicates <- findreplace[duplicated(findreplace[, c("replacecolumns", "from")]), ]

  if (nrow(duplicates) > 0) {
    stop(paste(
      "Error: findreplace contains", nrow(duplicates), "rows with the same replacecolumns, from combination.\n",
      "Duplicated rows:\n", capture.output(print(duplicates))
    ))
  }

  if (!silent) cat("running replace_values", "\n")

  # Replace NA values with the unique string in df and findreplace
  df <- replace(df, is.na(df), NA_string)
  findreplace <- replace(findreplace, is.na(findreplace), NA_string)

  # Pre-filter findreplace
  findreplace <- findreplace %>%
        dplyr::filter(replacecolumns %in% colnames(df))

  for (replacecolumn in unique(findreplace$replacecolumns)) {
    if (silent == FALSE) cat("replacing ", replacecolumn, "\n")
    findreplace_column <- findreplace %>%
      dplyr::filter(replacecolumns == replacecolumn) %>%
      dplyr::mutate(replacecolumns = NULL)

    is_factor_column <- is.factor(df[, replacecolumn])
    # reduce everything
    df <- df %>%
      simplify_columns_df(columns = replacecolumn)

    # reduce everything
    findreplace_column <- findreplace_column %>%
      simplify_columns_df(columns = "from") %>%
      dplyr::filter(from %in% df[, replacecolumn]) %>%
      distinct()

    replacement <- df %>%
      dplyr::select(all_of(replacecolumn)) %>%
      dplyr::left_join(findreplace_column,
        by = setNames("from", replacecolumn)
      )
    stopifnot(nrow(replacement) == nrow(df))

    missing <- replacement %>%
      dplyr::filter(is.na(to)) %>%
      dplyr::distinct()

    if (nrow(missing) > 0) {
      warning(
        paste("No value assigned in", replacecolumn, "for", nrow(missing), "elements:\n"),
        paste(missing[, 1] %>% unique(), collapse = " ")
      )
    }

    if (!silent) {
      cat("peplacing \n")
      print("replacement")
      print(head(replacement))
      print("df")
      print(head(df))
    }

    #if keep_value_if_missing = FALSE, replace missing values with NA
    if(keep_value_if_missing){
      df[, replacecolumn] <- dplyr::coalesce(replacement$to, df[, replacecolumn])
    }else{
      df[, replacecolumn] <- replacement$to
    }

    #if(is_factor_column) df[, replacecolumn] <- as.factor(df[, replacecolumn])
  }

  # Convert the unique string back to NA
  df <- replace(df, df == NA_string, NA)
  #df <- convert_columns_to_numeric(df)

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

  # Exclude the given 'column' and "Deaths" from grouping, and sum the "Deaths" column
  # result <- group_and_sum(df, columns_to_sum = c("Deaths"), columns_to_not_group = c(column))

  # Add the new column with the specified value
  # result %>%
  #  mutate({{ column }} := new_col_value)
}

#' Replace strings or rename columns based on a find-replace mapping
#'
#' @param input A string vector or a data.frame.
#' @param findreplace A data.frame with two character columns: 'from' and 'to'.
#' @return Either a modified string vector or a data.frame with renamed columns.
#' @examples
#' findreplace <- data.frame(from = c("apple", "banana"), to = c("fruit", "berry"))
#' replaceOrRename(c("apple", "banana", "apple pie"), findreplace)
#' replaceOrRename(data.frame(apple = 1:3, banana = 4:6), findreplace)
replaceOrRename <- function(input, findreplace) {
  # Check if input is a string vector
  if (is.character(input)) {
    for (i in 1:nrow(findreplace)) {
      input <- gsub(findreplace$from[i], findreplace$to[i], input)
    }
    return(input)
  }

  # Check if input is a data.frame
  if (is.data.frame(input)) {
    colnames(input) <- plyr::revalue(colnames(input), setNames(as.character(findreplace$to), findreplace$from))
    return(input)
  }

  stop("Input must be either a string vector or a data.frame.")
}
