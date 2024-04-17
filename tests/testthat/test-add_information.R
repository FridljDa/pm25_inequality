library(testthat)

library(testthat)

test_that("add_social_vuln_index works correctly", {
  # Create a mock dataframe
  test_df <- data.frame(FIPS.code = c( "1003", "1005", "1031") %>% as.numeric(),
                        Year = c("2000", "2010", "2010") %>% as.numeric())
  # Path to your CSV file containing the rural-urban class lookup table
  path_to_svi <- here::here("data/08_svi/svi_lookup_table.csv")

  svi_lookup_table <- read_data(path_to_svi)
  svi_lookup_table <- test_df %>%
    inner_join(svi_lookup_table, by = c("FIPS.code" = "county", "Year" = "fromYear"))

  svi_lookup_table
  # Run the function with correct input
  result_df <- add_social_vuln_index(test_df)

  # Test that the result is as expected
  expect_equal(as.character(result_df$svi_bin), as.character(svi_lookup_table$svi_bin))

  # Test error handling for missing FIPS code column
  expect_error(add_social_vuln_index(test_df, FIPS.code.column = "wrong_column"))

  # Test error handling for missing Year column
  expect_error(add_social_vuln_index(test_df, Year.colum = "wrong_column"))
})

test_that("add_social_vuln_index does not throw too many warnings", {
  #file_list <- list.files(here("data/06_dem.agr/county", year_dem))
  #ces_agr_df <- lapply(file_list, function(file) fread(here("data/06_dem.agr/county", year_dem, file)))
  #ces_agr_df <- rbindlist(ces_agr_df)

  #ces_agr <- ces_agr_df %>%
  #  select(county) %>%
  #  distinct()
    #TODO
})

test_that("add_rural_urban_class does not throw too many warnings", {
  #TODO
})

test_that("add_rural_urban_class adds rural-urban classification correctly", {
  # Sample dataframe
  df <- data.frame(
    FIPS.code = c(1021, 1017),
    Year = c(1990, 2010),
    some_data = c(100, 200)
  )

  # Path to your CSV file containing the rural-urban class lookup table
  path_to_rural_urban_class <- here::here("data/rural_urban_class.csv")

  rural_urban_class <- read_data(path_to_rural_urban_class)
  # Define the expected output
  expected_output <- data.frame(
    FIPS.code = c(1021, 1017),
    Year = c(1990, 2010),
    some_data = c(100, 200),
    rural_urban_class = c(2, 3) # replace with actual classes
  )

  # Run the add_rural_urban_class function
  result <- add_rural_urban_class(df, path_to_rural_urban_class = path_to_rural_urban_class)

  # Check if the result matches the expected output
  expect_equal(as.character(result$rural_urban_class), as.character(expected_output$rural_urban_class))
})
