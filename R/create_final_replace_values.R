suppressMessages({
  library(tibble)
  library(dplyr)
})

findreplace_education <- tribble(
  ~replacecolumns,  ~from, ~to,
  "Education", "lower", "High school graduate or lower",
  "Education", "middle", "Some college education but no 4-year college degree",
  "Education", "higher", "4-year college graduate or higher",
  "Education", "666", "666"
)

findreplace_measure2 <- tribble(
  ~replacecolumns,  ~from, ~to,
  "measure2", "crude rate", "crude rate per 100,000",
  "measure2", "age-adjusted rate", "age-adjusted rate per 100,000",
  "measure2", "absolute number", "absolute number"
)


findreplace_gender_code <- tribble(
  ~replacecolumns, ~from, ~to,
  "Gender.Code", "A", "All genders"
)

findreplace_source <- tribble(
  ~replacecolumns, ~from, ~to,
  "source", "nvss", "National Vital Statistics System"
)

findreplace_ethnicity <- tribble(
  ~replacecolumns, ~from, ~to,
  "Ethnicity", "Black or African American, All Origins", "Black American",
  "Ethnicity", "American Indian or Alaska Native, All Origins", "American Indian or Alaska Native",
  "Ethnicity", "Asian or Pacific Islander, All Origins", "Asian or Pacific Islander",
  "Ethnicity", "White, Hispanic or Latino", "Hispanic or Latino White",
  "Ethnicity", "White, Not Hispanic or Latino", "NH White",
  "Ethnicity", "White, All Origins", "White",
  "Ethnicity", "All, All Origins", "All, All Origins"
)


findreplace_rural_urban_class <- tribble(
  ~replacecolumns, ~from, ~to,
  "rural_urban_class", "1", "Large metro",
  "rural_urban_class", "2", "Small-medium metro",
  "rural_urban_class", "3", "Non metro",
  "rural_urban_class", "666", "All"
)


findreplace_svi_bin <- tribble(
  ~replacecolumns, ~from, ~to,
  "svi_bin", "1", "High SVI",
  "svi_bin", "2", "Middle SVI",
  "svi_bin", "3", "Low SVI",
  "svi_bin", "666", "All"
)

#SES - RPL_THEME1
# HC - RPL_THEME2
# Racial & Ethnic MS - RPL_THEME3
# HTT - RPL_THEME4

findreplace_svi_bin1 <- tribble(
  ~replacecolumns, ~from, ~to,
  "svi_bin1", "1", "High SES", #Socioeconomic Status
  "svi_bin1", "2", "Middle SES",
  "svi_bin1", "3", "Low SES",
  "svi_bin1", "666", "All"
)

findreplace_svi_bin2 <- tribble(
  ~replacecolumns, ~from, ~to,
  "svi_bin2", "1", "High HC", #Household Characteristics
  "svi_bin2", "2", "Middle HC",
  "svi_bin2", "3", "Low HC",
  "svi_bin2", "666", "All"
)

findreplace_svi_bin3 <- tribble(
  ~replacecolumns, ~from, ~to,
  "svi_bin3", "1", "High MS", #Minority status
  "svi_bin3", "2", "Middle MS",
  "svi_bin3", "3", "Low MS",
  "svi_bin3", "666", "All"
)

findreplace_svi_bin4 <- tribble(
  ~replacecolumns, ~from, ~to,
  "svi_bin4", "1", "High HTT", #Housing Type & transportation
  "svi_bin4", "2", "Middle HTT",
  "svi_bin4", "3", "Low HTT",
  "svi_bin4", "666", "All"
)


findreplace_full <- rbind(findreplace_education, findreplace_ethnicity, findreplace_gender_code,
                          findreplace_measure2, findreplace_rural_urban_class, findreplace_source,
                          findreplace_svi_bin, findreplace_svi_bin1, findreplace_svi_bin2,
                          findreplace_svi_bin3, findreplace_svi_bin4)

findreplace_full <- rbind(
  findreplace_full,
  data.frame(
    replacecolumns = findreplace_full$replacecolumns,
    from = findreplace_full$to,
    to = findreplace_full$to
  )
)

findreplace_full <- distinct(findreplace_full)

write.csv(findreplace_full, file = "data/final_findreplace.csv")
