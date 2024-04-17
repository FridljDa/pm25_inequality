

#' Add Social Vulnerability Index (SVI) to a Dataframe
#'
#' This function takes a dataframe and a specified FIPS code column, then adds a Social Vulnerability Index (SVI)
#' using a lookup table provided in a CSV file.
#'
#' @param df A dataframe that contains the data.
#' @param FIPS.code.column A string indicating the name of the column containing the FIPS code (default is "FIPS.code").
#' @param path_to_svi A string with the path to the CSV file containing the SVI lookup table (default is "data/08_svi/svi_lookup_table.csv").
#'
#' @importFrom tidyr replace_na
#' @return A dataframe with the added SVI values.
#'
#' @examples
#' \dontrun{
#' # Create example dataframe
#' example_df <- data.frame(
#'   FIPS.code = c("1001", "1003", "1005", "1007", "1009"),
#'   Year = c("1990", "1991", "1992", "1993", "1994"),
#'   some_data = c(23, 45, 67, 89, 12)
#' )
#'
#' # Use the function to add SVI to example_df
#' result_df <- add_social_vuln_index_new(example_df, "FIPS.code", "Year", "data/08_svi/svi_lookup_table.csv")
#' print(result_df)
#' }
add_social_vuln_index <- function(df,
                                  FIPS.code.column = "FIPS.code",
                                  Year.colum = "Year",
                                  path_to_svi = "data/08_svi/svi_lookup_table.csv",
                                  silent = TRUE) {

  # Check if FIPS.code.column and Year.colum are present in df
  if (!FIPS.code.column %in% names(df) || !Year.colum %in% names(df)) {
    stop("Error: The specified FIPS code column or Year column is not present in the dataframe.")
  }

  present_years <- unique(df[, Year.colum])
  corresponding_year <- c("1990" = 2000, "1991" = 2010, "1992" = 2010, "1993" = 2010, "1994" = 2010,
                          "1995" = 2000, "1996" = 2010, "1997" = 2010, "1998" = 2010, "1999" = 2010,
                          "2000" = 2000, "2001" = 2010, "2002" = 2010, "2003" = 2010, "2004" = 2010,
                          "2005" = 2010, "2006" = 2010, "2007" = 2010, "2008" = 2010, "2009" = 2000,
                          "2010" = 2010, "2011" = 2010, "2012" = 2010, "2013" = 2010, "2014" = 2014,
                          "2015" = 2016, "2016" = 2016)

  # Read the SVI lookup table from the specified path
  svi_lookup_table <- read_data(path_to_svi)
  replace_column_names <- c("svi_bin","svi_bin1","svi_bin2","svi_bin3","svi_bin4")

  # Create findreplace_df for each column in replace_column_names
  findreplace_dfs <- lapply(replace_column_names, function(col) {
    data.frame(
      replacecolumns = col,
      from = svi_lookup_table$county %>% as.numeric(),
      to = svi_lookup_table[[col]],
      fromYear = svi_lookup_table$fromYear
    )
  })

  df[replace_column_names] <- NA
  # Add svi_bin columns using FIPS code column
  df <- df %>%
    mutate(across(all_of(replace_column_names), ~ .data[[FIPS.code.column]] %>% as.numeric()))


  df_replaced <- purrr::map_dfr(
    present_years,
    function(year) {
      df_i <- df %>% filter(.data[[Year.colum]] == year)
      corresponding_year_i <- corresponding_year[[as.character(year)]]

      # Replace values for each column in replace_column_names
      for (findreplace_df in findreplace_dfs) {
        findreplace_df_i <- findreplace_df %>%
          filter(fromYear == corresponding_year_i)

        df_i <- replace_values(df_i, findreplace_df_i, silent = silent, keep_value_if_missing = FALSE)
      }

      df_i
    }
  )

  # Convert to character before replacing NA
  df_replaced <- df_replaced %>%
    mutate(across(all_of(replace_column_names), as.character))

  # Replace NA values
  df_replaced <- df_replaced %>%
    mutate(across(all_of(replace_column_names), ~ replace_na(.x, "Unknown")))

  # Convert back to factor if needed
  df_replaced <- df_replaced %>%
    mutate(across(all_of(replace_column_names), as.factor))

  return(df_replaced)
}


#' Add Rural-Urban Classification to a Dataframe
#'
#' This function takes a dataframe and specified columns for FIPS code and Year, then adds a rural-urban classification
#' using a lookup table provided in a CSV file.
#'
#' @param df A dataframe that contains the data.
#' @param FIPS.code.column A string indicating the name of the column containing the FIPS code (default is "FIPS.code").
#' @param Year.colum A string indicating the name of the column containing the Year (default is "Year").
#' @param path_to_rural_urban_class A string with the path to the CSV file containing the rural-urban class lookup table (default is "data/rural_urban_class.csv").
#'
#' @return A dataframe with the added rural-urban classification.
#' @importFrom tidyr replace_na
add_rural_urban_class <- function(df,
                                  FIPS.code.column = "FIPS.code",
                                  Year.colum = "Year",
                                  path_to_rural_urban_class = "data/rural_urban_class.csv",
                                  silent = TRUE,
                                  corresponding_year = setNames(c(1990, rep(2010, 9), 2000, rep(2010, 8), 2000, rep(2010, 7)), 1990:2016)) {
  # Check if FIPS.code.column and Year.colum are present in df
  if (!FIPS.code.column %in% names(df) || !Year.colum %in% names(df)) {
    stop("Error: The specified FIPS code column or Year column is not present in the dataframe.")
  }

  # Read the rural-urban class lookup table from the specified path
  rural_urban_class_df <- read_data(path_to_rural_urban_class)

  # TODO: Adjust findreplace_df based on the structure of rural_urban_class_df
  findreplace_df <- data.frame(
    replacecolumns = "rural_urban_class",
    from = rural_urban_class_df$FIPS.code,
    to = rural_urban_class_df$rural_urban_class,
    fromYear = rural_urban_class_df$fromYear
  )

  # Add rural_urban_class column using dplyr
  df <- df %>%
    mutate(rural_urban_class = .data[[FIPS.code.column]])

  present_years <- unique(df[, Year.colum])

  df_replaced <- purrr::map_dfr(
    present_years,
    function(year) {
      df_i <- df %>%
        filter(.data[[Year.colum]] == year)

      corresponding_year_i <- corresponding_year[[as.character(year)]]
      findreplace_df_i <- findreplace_df %>%
        filter(fromYear == corresponding_year_i)

      df_i %>%
        replace_values(findreplace_df_i, silent = silent, keep_value_if_missing = FALSE)
    }
  )

  df_replaced <- df_replaced %>%
    mutate(rural_urban_class = rural_urban_class %>% as.character,
           rural_urban_class = replace_na(rural_urban_class, "Unknown"),
           rural_urban_class = rural_urban_class %>% as.factor)

  return(df_replaced)
}

