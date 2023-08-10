
library(dplyr)
library(readr)
library(here)
library(data.table)
library(readxl)
devtools::load_all()

# List of years for which you have CSV files
years <- c(2000, 2010, 2020)

# Initialize an empty list to store the data frames for each year
svi_county <- list()

# Loop through each year and read the corresponding CSV file into the list
for (year in years) {
  file_name <- here(paste0("data/08_svi/SVI_", year, "_US_county.csv"))
  svi_county[[as.character(year)]] <- read_csv(file_name, show_col_types = FALSE)
}


rural_urban_class <- read_csv(here("data/rural_urban_class.csv"), show_col_types = FALSE)
#filter(fromYear == 2010)
rural_urban_class <- rural_urban_class  %>%
  split(rural_urban_class$fromYear) %>%
  lapply(function(x){
    as.numeric(x$FIPS.code)
  })

names(rural_urban_class) <- paste0("rural_urban_class_",names(rural_urban_class))


# Open a connection to the file, using append = TRUE to append the content
sink("output.txt", append = TRUE)

year_dem <- 1990
for(year_dem in 1990:2016){ #1990:2016
  file_list <- list.files(here("data/06_dem.agr/county", year_dem))
  ces_agr_df <- lapply(file_list, function(file) fread(here("data/06_dem.agr/county", year_dem, file)))
  ces_agr_df <- rbindlist(ces_agr_df)

  ces_agr <- ces_agr_df %>%
    select(county) %>%
    distinct()
  ces_agr <- unlist(ces_agr)
  ces_agr <- as.numeric(ces_agr)



  list_of_counties <- list(
    ces_agr = ces_agr %>% as.numeric(),
    svi_county_2000 = svi_county[["2000"]]$STCOFIPS %>% as.numeric(),
    svi_county_2010 = svi_county[["2010"]]$FIPS %>% as.numeric(),
    svi_county_2020 = svi_county[["2020"]]$STCNTY %>% as.numeric()
  )
  list_of_counties2 <- list(
    ces_agr = ces_agr %>% as.numeric()
  )
  list_of_counties2 <- append(list_of_counties2, rural_urban_class)



  dif <- outer_difference_heatmap(list_of_counties)
  dif2 <- outer_difference_heatmap(list_of_counties2)



  dif@matrix
  dif2@matrix



  # Assuming dif@matrix is the matrix
  # Exclude the first column and find the index of the minimum value in the remaining columns
  index_of_lowest_value <- which.min(dif@matrix[1, -1])

  # Add 1 to the index to account for the excluded first column
  column_with_lowest_value <- colnames(dif@matrix)[index_of_lowest_value + 1]
  value_of_lowest <- dif@matrix[1, index_of_lowest_value + 1]



  # Assuming dif@matrix is the matrix
  # Exclude the first column and find the index of the minimum value in the remaining columns
  index_of_lowest_value2 <- which.min(dif2@matrix[1, -1])

  # Add 1 to the index to account for the excluded first column
  column_with_lowest_value2 <- colnames(dif2@matrix)[index_of_lowest_value2 + 1]
  value_of_lowest2 <- dif2@matrix[1, index_of_lowest_value2 + 1]

  data <- data.frame(year_dem = year_dem,
                     column_with_lowest_value = column_with_lowest_value,
                     column_with_lowest_value2 = column_with_lowest_value2)

  readr::write_csv(data,
                   "correct_year.csv",
                   append = TRUE,
                   col_names = !file.exists("correct_year.csv")
                   )
  #cat("For the year", year_dem, "take ", column_with_lowest_value, "with value", value_of_lowest, "\n")
  #cat("For the year", year_dem, "take ", column_with_lowest_value2, "with value", value_of_lowest2, "\n")

}

# Close the connection to the file
sink()

