
# add_information

#' Add Social Vulnerability Index (SVI) to a Dataframe
#'
#' This function takes a dataframe and a specified FIPS code column, then adds a Social Vulnerability Index (SVI)
#' using a lookup table provided in a CSV file.
#'
#' @param df A dataframe that contains the data.
#' @param FIPS.code.column A string indicating the name of the column containing the FIPS code (default is "FIPS.code").
#' @param path_to_svi A string with the path to the CSV file containing the SVI lookup table (default is "data/08_svi/svi_lookup_table.csv").
#'
#' @return A dataframe with the added SVI values.
#'
#' @examples
#' # Assuming existence of a dataframe 'df' with a "FIPS.code" column
#' df_with_svi <- add_social_vuln_index(df)
#'
add_social_vuln_index <- function(df,
                                  FIPS.code.column = "FIPS.code",
                                  Year.colum = "Year",
                                  path_to_svi = "data/08_svi/svi_lookup_table.csv",
                                  silent = TRUE) {
  # Check if FIPS.code.column and Year.colum are present in df
  if (!FIPS.code.column %in% names(df) || !Year.colum %in% names(df)) {
    stop("Error: The specified FIPS code column or Year column is not present in the dataframe.")
  }

  corresponding_year <- c(
    "1990" = "2000",
    "1991" = "2010",
    "1992" = "2010",
    "1993" = "2010",
    "1994" = "2010",
    "1995" = "2000",
    "1996" = "2010",
    "1997" = "2010",
    "1998" = "2010",
    "1999" = "2010",
    "2000" = "2000",
    "2001" = "2010",
    "2002" = "2010",
    "2003" = "2010",
    "2004" = "2010",
    "2005" = "2010",
    "2006" = "2010",
    "2007" = "2010",
    "2008" = "2010",
    "2009" = "2000",
    "2010" = "2010",
    "2011" = "2010",
    "2012" = "2010",
    "2013" = "2010",
    "2014" = "2010",
    "2015" = "2020",
    "2016" = "2020"
  )

  # Read the SVI lookup table from the specified path
  svi_lookup_table <- read_data(path_to_svi)

  # Create a dataframe for find and replace operation
  findreplace_df <- data.frame(
    replacecolumns = "svi_bin",
    from = svi_lookup_table$county,
    to = svi_lookup_table$svi_bin,
    fromYear = svi_lookup_table$fromYear
  )

  # Add svi_bin column using FIPS code column
  df <- df %>%
    mutate(svi_bin = .data[[FIPS.code.column]])

  present_years <- unique(df[, Year.colum])

  #TODO
  #df_replaced <- purrr::map_dfr(
  #  present_years,
  #  function(year) {
  #    df_i <- df %>%
  #      filter(.data[[Year.colum]] == year)

  #    corresponding_year_i <- corresponding_year[[as.character(year)]]
  #   findreplace_df_i <- findreplace_df %>%
  #      filter(fromYear == corresponding_year_i)

  #    df_i %>%
  #      replace_values(findreplace_df_i, silent = silent, keep_value_if_missing = FALSE)
  #  }
  #)
  findreplace_df <- findreplace_df %>% filter(fromYear == 2010)
  df_replaced <- df %>%
    replace_values(findreplace_df, silent = silent, keep_value_if_missing = FALSE)

  df_replaced <- df_replaced %>%
    mutate(svi_bin = svi_bin %>% as.character,
           svi_bin = replace_na(svi_bin, "Unknown"),
           svi_bin = svi_bin %>% as.factor)

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
#'
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
