library(testthat)

library(testthat)

test_that("add_social_vuln_index works correctly", {
  # Create a mock dataframe
  test_df <- data.frame(FIPS.code = c("1001", "1003", "1005", "1031"),
                        Year = c("1990", "2000", "2010", "2010"))
  # Path to your CSV file containing the rural-urban class lookup table
  path_to_svi <- here::here("data/08_svi/svi_lookup_table.csv")

  svi_lookup_table <- read_data(path_to_svi)
  svi_lookup_table <- svi_lookup_table %>%
    filter(as.numeric(county) %in% test_df$FIPS.code,
           fromYear == 2010)

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
