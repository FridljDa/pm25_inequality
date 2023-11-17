# power_set.R
#' Generate the power set of a vector
#'
#' @param x A vector whose power set you want to generate.
#' @return A list containing all subsets of the input vector.
power_set <- function(x) {
  # Base case: power set of an empty set is a set containing an empty set
  if (length(x) == 0) {
    return(list(numeric(0)))
  }

  # Take the first element
  el <- x[1]

  # Generate the power set for the remaining elements
  remaining_power_set <- power_set(x[-1])

  # Add the current element to each subset of the remaining power set
  extended_power_set <- lapply(remaining_power_set, function(s) c(el, s))

  # Combine the two
  return(c(remaining_power_set, extended_power_set))
}

is_maximal <- function(subset, all_subsets) {
  for (other_subset in all_subsets) {
    if (all(subset %in% other_subset) && !all(other_subset %in% subset)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Diagnose join issues between two data frames
#'
#' @param df1 A data frame to be joined
#' @param df2 Another data frame to be joined
#' @param join_cols A character vector specifying the columns to join on
#' @return A data frame after performing the required joins
#' @examples
#' diagnose_join_issues(df1, df2, c("col1", "col2"))
diagnose_join_issues <- function(df1, df2, join_cols, power_set_limit = 5) {
  # Try to get the names of df1 and df2, fall back to default names if it fails
  df1_name <- tryCatch(deparse(substitute(df1)), error = function(e) "df1")
  df2_name <- tryCatch(deparse(substitute(df2)), error = function(e) "df2")

  # Perform anti_join to identify unjoinable rows
  anti_joined <- anti_join(df1, df2, by = join_cols)
  full_joined <- full_join(df1, df2, by = join_cols)

  if (nrow(anti_joined) == 0) {
    return(anti_joined)
  }
  # Check if there are unjoinable rows

  # Generate informative warning messages
  warning(paste("In diagnose_join_issues():", df1_name, "and",
                df2_name, "are not joinable in", nrow(anti_joined), "rows of",
                nrow(full_joined), "rows."))

  problem_identified <- FALSE
  # Identify which columns have problematic values
  for (col in join_cols) {
    unique_anti_joined_values <- unique(anti_joined[[col]])
    unique_df1_values <- unique(df1[[col]])
    unique_df2_values <- unique(df2[[col]])

    not_in_df1 <- setdiff(unique_anti_joined_values, unique_df1_values)
    not_in_df2 <- setdiff(unique_anti_joined_values, unique_df2_values)

    if (length(not_in_df1) > 0) {
      problem_identified <- TRUE
      warning(paste("Column '", col, "' has values not found in", df1_name, ":", paste(head(not_in_df1, 10), collapse = ", ")))
    }

    if (length(not_in_df2) > 0) {
      problem_identified <- TRUE
      warning(paste("Column '", col, "' has values not found in", df2_name, ":", paste(head(not_in_df2, 10), collapse = ", ")))
    }
  }

  if (!problem_identified) {
    # Check if join_cols has too many elements for power set

    if (length(join_cols) > power_set_limit) { # Adjust this limit as you see fit

      warning("Skipping power set calculation due to large number of join columns.")
    } else {
      # Take power set of join_cols
      power_set_join_cols <- power_set(join_cols)
      power_set_join_cols <- power_set_join_cols[-1]

      # Identify subsets of join_cols where joining works
      power_set_join_cols_joins <- sapply(power_set_join_cols, function(power_set_join_cols_i) {
        anti_joined <- anti_join(df1, df2, by = power_set_join_cols_i)
        return(nrow(anti_joined) == 0)
      })

      # Filter for those
      power_set_join_cols_sub <- power_set_join_cols[power_set_join_cols_joins]

      # Find maximal elements
      maximal_elements <- sapply(power_set_join_cols_sub, function(sub) is_maximal(sub, power_set_join_cols_sub))

      # Filter for maximal elements
      maximal_power_set_join_cols_sub <- power_set_join_cols_sub[maximal_elements]


      # Print maximal subsets where joining is successful as a warning
      if (length(maximal_power_set_join_cols_sub) > 0) {
        for (maximal_subset in maximal_power_set_join_cols_sub) {
          missing_cols <- setdiff(join_cols, maximal_subset)
          warning(paste(
            "A maximal successful joining can be achieved using the columns:",
            toString(maximal_subset),
            ". Columns not in this subset:",
            toString(missing_cols)
          ))
        }
      }
    }
  }

  # Show first few unjoinable rows for diagnostic purposes
  warning("First few unjoinable rows:")
  print(head(anti_joined))


  # Your main logic here

  return(anti_joined)
}
