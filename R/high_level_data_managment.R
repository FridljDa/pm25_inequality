
#read_data
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
#' @import readr
read_data <- function(path) {
  # Read the first row of the CSV file
  first_row <- readr::read_csv(path,
                               n_max = 1,
                               show_col_types = FALSE
  )

  # Get the column names
  column_names_present <- colnames(first_row)

  col_types_data <- cols(
    source2 = col_factor(levels = c("Census", "CDC", "cens2")),
    attr = col_factor(levels = c("overall", "total", "attributable")),
    label_cause = col_factor(levels = c("all-cause",  "ncd_lri",
                                              "cvd_ihd", "cvd_stroke",
                                              "neo_lung", "resp_copd",
                                              "lri", "t2_dm")),
    pop_size = col_double(),
    mean = col_double(), lower = col_double(), upper = col_double(),
    pm = col_double(), prop = col_double(),
    state = col_integer(),
    variable = col_character(),
    Region = col_character(),
    Year = col_integer(),
    fromYear = col_integer(),
    value = col_double(),
    min_age = col_integer(), max_age = col_integer(),
    county = col_integer(),
    FIPS.code = col_integer(),
    tract = col_integer(),
    GEO_ID = col_character(),
    Population = col_double(),
    agr_by = col_factor(levels = c("nation","county", "STATEFP")),
    rural_urban_class = col_factor(levels = c(
      "Large metro", "1",
      "Small-medium metro", "2",
      "Non metro", "3",
      "All", "666"
    )),
    svi_bin = col_factor(levels = c(
      "low svi", "1",
      "middle svi", "2",
      "high svi", "3",
      "All", "666",
      "Unknown"
    )),
    Deaths = col_integer(),
    Education = col_factor(levels = c(
      "666", "Unknown",
      "High school graduate or lower" , "lower",
      "Some college education but no 4-year college degree", "middle",
      "4-year college graduate or higher","higher"
    )),
    nation = col_factor(levels = c(
      "us"
    )),
    Race = col_factor(levels = c(
      "All",
      "American Indian or Alaska Native",
      "Asian or Pacific Islander",
      "Black or African American",
      "White"
    )),
    measure1 = col_factor(levels = c(
      "Deaths",
      "YLL"
    )),
    method = col_factor(levels = c(
      "di_gee",
      "di_gee_white",
      "burnett",
      "GBD"
    )),
    measure2 = col_factor(levels = c(
      "crude rate per 100,000", "crude rate",
      "age-adjusted rate per 100,000", "age-adjusted rate",
      "absolute number", "absolute number"
    )),
    scenario = col_factor(levels = c(
      "NAAQS",
      "WHO",
      "future threshold",
      "real"
    )),
    measure3 = col_factor(levels = c(
      "value",
      "prop. of overall burden",
      "proportion of disparity to Black or African American attributable"
    )),
    Gender.Code = col_factor(levels = c(
      "All genders", "A", "F", "M"
    )),
    source = col_factor(levels = c(
      "nvss", "National Vital Statistics System"
    )),
    Hispanic.Origin = col_factor(levels = c(
      "All Origins", "Hispanic or Latino", "Not Hispanic or Latino"
    )),
    Ethnicity = col_factor(levels = c(
      "Black American", "Black or African American, All Origins",
      "American Indian or Alaska Native", "American Indian or Alaska Native, All Origins",
      "Asian or Pacific Islander", "Asian or Pacific Islander, All Origins",
      "Hispanic or Latino White", "White, Hispanic or Latino",
      "NH White", "White, Not Hispanic or Latino",
      "White", "White, All Origins",
      "All, All Origins", "All, All Origins"
    ))
    # TODO Hispanic.Origin
  )

  column_names_available <- names(col_types_data[["cols"]])
  column_names_intersection <- intersect(column_names_available, column_names_present)
  column_names_dif <- setdiff(column_names_present, column_names_available)
  if (length(column_names_dif) > 0) {
    message("No col_types specified for: ", paste(column_names_dif, collapse = ", "))
  }

  col_types_data[["cols"]] <- col_types_data[["cols"]][column_names_intersection]

  df <- readr::read_csv(path,
                        col_types = col_types_data,
                        show_col_types = FALSE
  )

  parsing_problems <- readr::problems(df)

  if (nrow(parsing_problems) > 0) {
    df2 <- readr::read_csv(path)
    unique_levels <- lapply(df2, unique)
    problem_column <- parsing_problems$col[1]
    first_problem <- df2[1, problem_column]
    #browser()

    warning(paste("Parsing issues detected in" , path, "in column",
                  problem_column, "(", first_problem, "). Please check the CSV file."))
    return(df2)
  }

  return(df)
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
  if (!dir.exists(directory)) {
    stop(paste("Directory", directory, "does not exist."))
  }

  # Get all file and directory names in the directory
  all_files <- list.files(directory, full.names = TRUE, recursive = FALSE)

  # Modify the string if ends_with = TRUE
  if (ends_with) {
    string <- paste0(string, "$")
  }

  # Filter files and directories whose names contain the specified string
  matching_files <- all_files[grepl(string, basename(all_files))]

  return(matching_files)
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
    dir.create(dir_path, recursive = T)
  }
  invisible(NULL)
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
#' run_script("my_script.R")
#' run_script("my_script.R", "--arg value")
#' }
#' @export
run_script <- function(script, args = "") {
  sysname <- Sys.info()["sysname"]

  # Record the start time
  start_time <- proc.time()
  print(paste("Running script", script, "on", sysname))

  if (sysname == "Darwin") {
    system(paste("Rscript", script, args))
  } else if (sysname == "Windows") {
    memory.limit(size = 500000)

    exec <- paste0("C:/Program Files/R/R-", R.Version()$major, ".", R.Version()$minor, "/bin/Rscript.exe")
    exec <- shQuote(exec)

    system(paste(exec, "--vanilla", script, args))
  } else {
    system(paste("Rscript", script, args))
  }

  # Record the end time
  end_time <- proc.time()

  # Calculate and print the time difference
  print(paste("Time taken to run script", script, ":", as.numeric(end_time - start_time, "secs"), "seconds"))
}

#' Diagnose join issues between two data frames
#'
#' @param df1 A data frame to be joined
#' @param df2 Another data frame to be joined
#' @param join_cols A character vector specifying the columns to join on
#' @return A data frame after performing the required joins
#' @examples
#' diagnose_join_issues(df1, df2, c("col1", "col2"))
diagnose_join_issues <- function(df1, df2, join_cols) {

  # Try to get the names of df1 and df2, fall back to default names if it fails
  df1_name <- tryCatch(deparse(substitute(df1)), error = function(e) "df1")
  df2_name <- tryCatch(deparse(substitute(df2)), error = function(e) "df2")

  # Perform anti_join to identify unjoinable rows
  anti_joined <- anti_join(df1, df2, by = join_cols)

  # Check if there are unjoinable rows
  if (nrow(anti_joined) > 0) {

    # Generate informative warning messages
    warning(paste("In diagnose_join_issues():", df1_name, "and", df2_name, "are not joinable in", nrow(anti_joined), "rows."))

    # Identify which columns have problematic values
    for (col in join_cols) {
      unique_anti_joined_values <- unique(anti_joined[[col]])
      unique_df1_values <- unique(df1[[col]])
      unique_df2_values <- unique(df2[[col]])

      not_in_df1 <- setdiff(unique_anti_joined_values, unique_df1_values)
      not_in_df2 <- setdiff(unique_anti_joined_values, unique_df2_values)

      if (length(not_in_df1) > 0) {
        warning(paste("Column '", col, "' has values not found in", df1_name, ":", paste(head(not_in_df1, 10), collapse = ", ")))
      }

      if (length(not_in_df2) > 0) {
        warning(paste("Column '", col, "' has values not found in", df2_name, ":", paste(head(not_in_df2, 10), collapse = ", ")))
      }
    }

    # Show first few unjoinable rows for diagnostic purposes
    warning("First few unjoinable rows:")
    print(head(anti_joined))
  }

  # Your main logic here

  return(anti_joined)
}

