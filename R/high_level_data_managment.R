# read_data
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
    label_cause = col_factor(levels = c(
      "all-cause", "ncd_lri",
      "cvd_ihd", "cvd_stroke",
      "neo_lung", "resp_copd",
      "lri", "t2_dm"
    )),
    pop_size = col_double(),
    mean = col_double(), lower = col_double(), upper = col_double(),
    pm = col_double(), prop = col_double(),
    state = col_integer(),
    variable = col_character(),
    STATEFP = col_character(),
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
    agr_by = col_factor(levels = c("nation", "county", "STATEFP")),
    rural_urban_class = col_factor(levels = c(
      "Large metro", "1",
      "Small-medium metro", "2",
      "Non metro", "3",
      "All", "666"
    )),
    svi_bin = col_factor(levels = c(
      "Vulnerable SVI", "1",
      "Moderate SVI", "2",
      "Resilient SVI", "3",
      "All", "666",
      "Unknown"
    )),
    svi_bin1 = col_factor(levels = c(
      "Low SES", "1",
      "Middle SES", "2",
      "High SES", "3",
      "All", "666",
      "Unknown"
    )),
    svi_bin2 = col_factor(levels = c(
      "Low HC", "1",
      "Middle HC", "2",
      "High HC", "3",
      "All", "666",
      "Unknown"
    )),
    svi_bin3 = col_factor(levels = c(
      "Low MS", "1",
      "Middle MS", "2",
      "High MS", "3",
      "All", "666",
      "Unknown"
    )),
    svi_bin4 = col_factor(levels = c(
      "Low HTT", "1",
      "Middle HTT", "2",
      "High HTT", "3",
      "All", "666",
      "Unknown"
    )),
    Deaths = col_integer(),
    Education = col_factor(levels = c(
      "666", "Unknown",
      "High school graduate or lower", "lower",
      "Some college education but no 4-year college degree", "middle",
      "4-year college graduate or higher", "higher"
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
    df2 <- readr::read_csv(path, show_col_types = FALSE)
    unique_levels <- lapply(df2, unique)
    problem_column <- parsing_problems$col[1]
    first_problem <- df2[1, problem_column]
    # browser()

    warning(paste(
      "Parsing issues detected in", path, "in column",
      problem_column, "(", first_problem, "). Please check the CSV file."
    ))
    return(df2)
  }

  df <- df %>%
    dplyr::mutate(across(where(is.factor), droplevels))

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
#' @param args A string specifying the arguments to be passed to the script. Default is NULL (no arguments).
#'
#' @return NULL. This function is mainly called for its side effects (running the script and printing the results).
#' @examples
#' \dontrun{
#' run_script("my_script.R")
#' run_script("my_script.R", "--arg value")
#' }
#' @export
run_script <- function(script, args = NULL) {
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



#' Add custom rows to a data frame
#'
#' This function takes a data frame `df` and adds custom rows to it by mutating specific columns.
#' The custom rows have the value "666" for the specified columns.
#'
#' @param df A data frame that you want to add custom rows to.
#' @return A data frame with the custom rows added.
#' @examples
#' # Create a sample data frame
#' sample_df <- data.frame(
#'   rural_urban_class = c("A", "B"),
#'   svi_bin = c(1, 2),
#'   svi_bin1 = c(3, 4),
#'   svi_bin2 = c(5, 6),
#'   svi_bin3 = c(7, 8),
#'   svi_bin4 = c(9, 10)
#' )
#'
#' # Add custom rows
#' modified_df <- add_custom_rows(sample_df)
add_custom_rows <- function(df) {
  library(dplyr)

  df <- rbind(
    df %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin3 = "666", svi_bin4 = "666"),
    df %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin3 = "666"),
    df %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin4 = "666"),
    df %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin1 = "666", svi_bin3 = "666", svi_bin4 = "666"),
    df %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin2 = "666", svi_bin3 = "666", svi_bin4 = "666"),
    df %>% mutate(rural_urban_class = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin3 = "666", svi_bin4 = "666"),
    df %>% mutate(svi_bin = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin3 = "666", svi_bin4 = "666")
  )

  return(df)
}

#' Generate Filtered Dataframes Based on Condition Combinations
#'
#' This function takes a dataframe and filters it based on all possible combinations
#' of the conditions specified for each variable.
#'
#' @param df A dataframe to filter.
#' @return A list of filtered dataframes.
generate_filtered_dfs <- function(df) {
  df <- as.data.frame(df)
  df_list <- list()

  columns <- c("Education", "Ethnicity", "rural_urban_class", "svi_bin", "svi_bin1", "svi_bin2", "svi_bin3", "svi_bin4")

  # bool_list <- rep(list(c(TRUE, FALSE)), length(columns))

  # Use expand.grid to generate all combinations
  # all_combinations <- do.call(expand.grid, bool_list)
  # colnames(all_combinations) <- columns

  # Loop through all combinations of conditions
  for (edu in c(666, "not 666")) {
    for (eth in c("All, All Origins", "not All, All Origins")) {
      for (ruc in c("All", "not All")) {
        for (svi in c("All", "not All")) {
          for (svi1 in c("All", "not All")) {
            for (svi2 in c("All", "not All")) {
              for (svi3 in c("All", "not All")) {
                for (svi4 in c("All", "not All")) {
                  # Generate filter conditions
                  filter_conditions <- list(
                    if (edu == 666) quote(Education == 666) else quote(Education != 666),
                    if (eth == "All, All Origins") quote(Ethnicity == "All, All Origins") else quote(Ethnicity != "All, All Origins"),
                    if (ruc == "All") quote(rural_urban_class == "All") else quote(rural_urban_class != "All"),
                    if (svi == "All") quote(svi_bin == "All") else quote(svi_bin != "All"),
                    if (svi1 == "All") quote(svi_bin1 == "All") else quote(svi_bin1 != "All"),
                    if (svi2 == "All") quote(svi_bin2 == "All") else quote(svi_bin2 != "All"),
                    if (svi3 == "All") quote(svi_bin3 == "All") else quote(svi_bin3 != "All"),
                    if (svi4 == "All") quote(svi_bin4 == "All") else quote(svi_bin4 != "All")
                  )

                  # Filter dataframe based on conditions
                  temp_df <- df %>% filter(!!!filter_conditions)


                  key <- list(
                    if (edu == 666) NA else "Education",
                    if (eth == "All, All Origins") NA else "Ethnicity",
                    if (ruc == "All") NA else "rural_urban_class",
                    if (svi == "All") NA else "svi_bin",
                    if (svi1 == "All") NA else "svi_bin1",
                    if (svi2 == "All") NA else "svi_bin2",
                    if (svi3 == "All") NA else "svi_bin3",
                    if (svi4 == "All") NA else "svi_bin4"
                  )
                  key <- key[!is.na(key)]
                  if(rlang::is_empty(key)){
                    key <- "All"
                  }else{
                    key <- paste(key, collapse = "*")
                  }

                  df_list[[key]] <- temp_df
                }
              }
            }
          }
        }
      }
    }
  }

  # Filter out empty dataframes
  df_list <- purrr::discard(df_list, ~ nrow(.x) == 0)
  return(df_list)
}

# Use the function
#df_list <- generate_filtered_dfs(attr_burd)
