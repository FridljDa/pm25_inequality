library(testthat)

test_that("add_social_vuln_index adds SVI bin correctly", {
  # Sample dataframe
  df <- data.frame(
    FIPS.code = c(01001, 01019),
    some_data = c(100, 200)
  )

  # Path to your CSV file containing the SVI lookup table
  path_to_svi <- here::here("data/08_svi/svi_lookup_table.csv")

  svi <- read_csv(path_to_svi) %>%
    filter(as.numeric(county) %in% as.numeric(df$FIPS.code))
  svi

  # Define the expected output based on the SVI lookup table
  expected_output <- data.frame(
    FIPS.code = c(01001, 01019),
    some_data = c(100, 200),
    svi_bin = c(2, 1) # replace with actual bins according to the lookup table
  )

  # Run the add_social_vuln_index function
  result <- add_social_vuln_index(df, path_to_svi = path_to_svi, silent = FALSE)

  # Check if the result matches the expected output
  expect_equal(result, expected_output)

  # Define the expected output based on the SVI lookup table
  expected_output <- data.frame(
    FIPS.code = c(01001, 01019),
    some_data = c(100, 200),
    svi_bin = c(2, 1) # replace with actual bins according to the lookup table
  )

  # Run the add_social_vuln_index function
  result <- add_social_vuln_index(df, path_to_svi = path_to_svi)

  # Sample dataframe with an invalid FIPS code
  df_with_invalid_fips <- data.frame(
    FIPS.code = c(01001, "not_fips_code"),
    some_data = c(100, 200)
  )

  # Check if the function throws a warning with the invalid FIPS code
  expect_warning(add_social_vuln_index(df_with_invalid_fips, path_to_svi = path_to_svi))
})

if(FALSE){
  test_that("add_rural_urban_class adds rural-urban classification correctly", {
    # Sample dataframe
    df <- data.frame(
      FIPS.code = c("FIPS1", "FIPS2"),
      Year = c(1990, 2010),
      some_data = c(100, 200)
    )

    # Path to your CSV file containing the rural-urban class lookup table
    path_to_rural_urban_class <- here::here("data/rural_urban_class.csv")

    # Define the expected output
    expected_output <- data.frame(
      FIPS.code = c("FIPS1", "FIPS2"),
      Year = c(1990, 2010),
      some_data = c(100, 200),
      rural_urban_class = c("Class1", "Class2") # replace with actual classes
    )

    # Run the add_rural_urban_class function
    result <- add_rural_urban_class(df, path_to_rural_urban_class = path_to_rural_urban_class)

    # Check if the result matches the expected output
    expect_equal(result, expected_output)
  })

}
