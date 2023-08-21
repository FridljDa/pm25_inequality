#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 03/27/2021
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
#rm(list = ls(all = TRUE))

# load packages, install if missing
suppressMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  library(testthat)
  library(readxl)
  library(tictoc)
  library(stringr)
})
pkgload::load_all()

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {

  tmpDir <- "data/tmp"
  totalBurdenParsedDir <- "data/09_total_burden_parsed"
  dataDir <- "data"
}else{
  dataDir <- args[2]
  tmpDir <- args[3]
  totalBurdenParsedDir <- args[13]
}

findreplaceDir <- file.path(totalBurdenParsedDir, "findreplace.csv")
states <- file.path(tmpDir, "states.csv") %>% read.csv()
rural_urban_class <- read.csv(file.path(dataDir, "rural_urban_class.csv"))
not_interested_states <- c("ZZ", "YY", "PR", "CC", "MX", "VI", "GU", "CU", "AS", "MP", "XX", "ON", "QC")


if (file.exists(findreplaceDir)) {
  quit()
}
#### ----- 1990-1991------
# Data frame for 'Year' column
year_replacement_df <- data.frame(
  replacecolumns = "Year",
  from = 90:99,
  to = 1990:1999
)

# Data frame for 'Hispanic.Origin' column
hispanic_origin_replacement_df <- data.frame(
  replacecolumns = "Hispanic.Origin",
  from = c(0, 1:5, 99),
  to = c("Not Hispanic or Latino", rep("Hispanic or Latino", 5), "Unknown")
)

# Data frame for 'Gender.Code' column
gender_code_replacement_df <- data.frame(
  replacecolumns = "Gender.Code",
  from = c(1, 2),
  to = c("M", "F")
)

# Data frame for 'Race' column
race_replacement_df <- data.frame(
  replacecolumns = "Race",
  from = c(1:3, 4:8, 9),
  to = c("White", "Black or African American", "American Indian or Alaska Native", rep("Asian or Pacific Islander", 5), "Unknown")
)

# Data frame for 'STATEFP' column
statefp_replacement_df <- data.frame(
  replacecolumns = "STATEFP",
  from = c(1:51, 52:62, NA),
  to = c(states$STATEFP, rep(0, 12))
)

# Data frame for 'interested_state' column
interested_state_replacement_df <- data.frame(
  replacecolumns = "interested_state",
  from = c(1:51, 52:62, NA),
  to = c(rep(1, 51), rep(0, 12))
)

# Data frame for 'min_age' column
min_age_replacement_df <- data.frame(
  replacecolumns = "min_age",
  from = c(1:199, sprintf("2%02d", c(0:11, 99)), sprintf("3%02d", c(0:3, 99)), sprintf("4%02d", c(0:27, 99)), sprintf("5%02d", c(0:23, 99)), sprintf("6%02d", c(0:59, 99)), 999),
  to = c(1:199, rep("0", 13 + 5 + 29 + 25 + 61), "Unknown")
)

# Data frame for 'max_age' column
max_age_replacement_df <- data.frame(
  replacecolumns = "max_age",
  from = c(1:199, sprintf("2%02d", c(0:11, 99)), sprintf("3%02d", c(0:3, 99)), sprintf("4%02d", c(0:27, 99)), sprintf("5%02d", c(0:23, 99)), sprintf("6%02d", c(0:59, 99)), 999),
  to = c(1:199, rep("0", 13 + 5 + 29 + 25 + 61), "Unknown")
)

# Combine all data frames
findreplaces_1990_to_1991 <- rbind(
  year_replacement_df,
  hispanic_origin_replacement_df,
  gender_code_replacement_df,
  race_replacement_df,
  statefp_replacement_df,
  interested_state_replacement_df,
  min_age_replacement_df,
  max_age_replacement_df
)

rm(
  year_replacement_df,
  hispanic_origin_replacement_df,
  gender_code_replacement_df,
  race_replacement_df,
  statefp_replacement_df,
  interested_state_replacement_df,
  min_age_replacement_df,
  max_age_replacement_df
)
findreplaces_1990_to_1991 <- merge(data.frame(Year = 1990:1991), findreplaces_1990_to_1991)

#### ----- 1992-2002------
# Define replacements for the 'Year' column
year_replacement_df <- data.frame(
  replacecolumns = "Year",
  from = c(90:99, 0:16, 1990:2016),
  to = c(1990:2016, 1990:2016)
)

# Define replacements for the 'Hispanic.Origin' column
hispanic_origin_replacement_df <- data.frame(
  replacecolumns = "Hispanic.Origin",
  from = c(0, 1, 2, 3, 4, 5, 99),
  to = c("Not Hispanic or Latino", rep("Hispanic or Latino", 5), "Unknown")
)

# Define replacements for the 'Gender.Code' column
gender_code_replacement_df <- data.frame(
  replacecolumns = "Gender.Code",
  from = c(1, 2),
  to = c("M", "F")
)

# Define replacements for the 'Race' column
race_replacement_df <- data.frame(
  replacecolumns = "Race",
  from = c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 68, 78),
  to = c("White", "Black or African American", "American Indian or Alaska Native", rep("Asian or Pacific Islander", 8), "Unknown", rep("Asian or Pacific Islander", 2))
)

# Define replacements for the 'STATEFP' column
state_fp_replacement_df <- data.frame(
  replacecolumns = "STATEFP",
  from = c(1:51, 52:62, NA),
  to = c(states$STATEFP, rep(0, 12))
)

# Define replacements for the 'interested_state' column
interested_state_replacement_df <- data.frame(
  replacecolumns = "interested_state",
  from = c(1:51, sprintf("%02d", c(1:51)), 52:62, NA),
  to = c(rep(1, 2*51), rep(0, 12))
)

# Define replacements for the 'min_age' column
min_age_replacement_df <- data.frame(
  replacecolumns = "min_age",
  from = c(1:199, sprintf("2%02d", c(0:11, 99)), sprintf("3%02d", c(0:3, 99)), sprintf("4%02d", c(0:27, 99)), sprintf("5%02d", c(0:23, 99)), sprintf("6%02d", c(0:59, 99)), 999),
  to = c(1:199, rep("0", 13 + 5 + 29 + 25 + 61), "Unknown")
)

# Define replacements for the 'max_age' column
max_age_replacement_df <- data.frame(
  replacecolumns = "max_age",
  from = c(1:199, sprintf("2%02d", c(0:11, 99)), sprintf("3%02d", c(0:3, 99)), sprintf("4%02d", c(0:27, 99)), sprintf("5%02d", c(0:23, 99)), sprintf("6%02d", c(0:59, 99)), 999),
  to = c(1:199, rep("0", 13 + 5 + 29 + 25 + 61), "Unknown")
)

# Combine all replacements into a single dataframe
findreplaces_1992_to_2002 <- rbind(
  year_replacement_df,
  hispanic_origin_replacement_df,
  gender_code_replacement_df,
  race_replacement_df,
  state_fp_replacement_df,
  interested_state_replacement_df,
  min_age_replacement_df,
  max_age_replacement_df
)

# Merge with the year data frame
findreplaces_1992_to_2002 <- distinct(findreplaces_1992_to_2002)
findreplaces_1992_to_2002 <- merge(data.frame(Year = 1992:2002), findreplaces_1992_to_2002)

#### ----- 2003-2016 -------
# Define replacements for the 'Hispanic.Origin' column
hispanic_origin_replacement_df <- data.frame(
  replacecolumns = "Hispanic.Origin",
  from = c(1:5, 6:8, 9),
  to = c(rep("Hispanic or Latino", 5), rep("Not Hispanic or Latino", 3), "Unknown")
)

# Define replacements for the 'STATEFP' column
state_fp_replacement_df <- data.frame(
  replacecolumns = "STATEFP",
  from = c(states$STUSPS, not_interested_states, NA),
  to = c(states$STATEFP, rep(0, length(not_interested_states) + 1))
)

# Define replacements for the 'interested_state' column
interested_state_replacement_df <- data.frame(
  replacecolumns = "interested_state",
  from = c(states$STUSPS, not_interested_states, NA),
  to = c(rep(1, nrow(states)), rep(0, length(not_interested_states) + 1))
)

# Define replacements for the 'Race' column
race_replacement_df <- data.frame(
  replacecolumns = "Race",
  from = c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 68, 78),
  to = c("White", "Black or African American", "American Indian or Alaska Native", rep("Asian or Pacific Islander", 8), "Asian or Pacific Islander", rep("Asian or Pacific Islander", 2))
)

# Define replacements for the 'min_age' column
min_age_replacement_df <- data.frame(
  replacecolumns = "min_age",
  from = c(sprintf("1%03d", 1:135), sprintf("2%03d", c(0:11, 999)), sprintf("4%03d", c(0:27, 999)), sprintf("5%03d", c(0:23, 999)), sprintf("6%03d", c(0:59, 999)), "1999", "9999"),
  to = c(sprintf("%03d", 1:135), rep("0", 13 + 29 + 25 + 61), "Unknown", "Unknown")
)

# Define replacements for the 'max_age' column
max_age_replacement_df <- data.frame(
  replacecolumns = "max_age",
  from = c(sprintf("1%03d", 1:135), sprintf("2%03d", c(0:11, 999)), sprintf("4%03d", c(0:27, 999)), sprintf("5%03d", c(0:23, 999)), sprintf("6%03d", c(0:59, 999)), "1999", "9999"),
  to = c(sprintf("%03d", 1:135), rep("0", 13 + 29 + 25 + 61), "Unknown", "Unknown")
)

# Define replacements for the 'Education1989' column
education1989_replacement_df <- data.frame(
  replacecolumns = "Education1989",
  from = c(0:17, 99, NA),
  to = c(rep("lower", 13), rep("middle", 3), rep("higher", 2), "Unknown", 101)
)

# Define replacements for the 'Education2003' column
education2003_replacement_df <- data.frame(
  replacecolumns = "Education2003",
  from = c(1:9, NA),
  to = c(rep("lower", 3), rep("middle", 2), rep("higher", 3), "Unknown", 101)
)

# Combine all replacements into a single dataframe
findreplaces_2003_to_2016 <- rbind(
  hispanic_origin_replacement_df,
  state_fp_replacement_df,
  interested_state_replacement_df,
  race_replacement_df,
  min_age_replacement_df,
  max_age_replacement_df,
  education1989_replacement_df,
  education2003_replacement_df
)
rm(
  hispanic_origin_replacement_df,
  state_fp_replacement_df,
  interested_state_replacement_df,
  race_replacement_df,
  min_age_replacement_df,
  max_age_replacement_df,
  education1989_replacement_df,
  education2003_replacement_df
)

findreplaces_2003_to_2016 <- merge(data.frame(Year = 2003:2016), findreplaces_2003_to_2016)

## --- county code---

# maximum number of counties per state
maximum_number_counties <- 840
concat <- function(state, county) sprintf("%s%03d", state, county)

findreplaces4 <- rbind(
  data.frame(
    replacecolumns = "county",
    from = c(outer(states$STATEFP, 1:maximum_number_counties, FUN = concat)),
    to = c(outer(states$STATEFP, 1:maximum_number_counties, FUN = concat))
  ),
  data.frame(
    replacecolumns = "county",
    from = c(
      c(outer(1:62, c(0, 999), FUN = concat)),
      57:62,
      c(outer(57:62, 0:maximum_number_counties, FUN = concat)),
      paste0(states$STATEFP, "999"),
      NA,
      0
    ),
    to = "Unknown"
  )
)

findreplaces4 <- rbind(
  findreplaces4 %>% mutate(from = str_pad(from, 5, pad = "0")),
  findreplaces4
) %>% distinct()



findreplaces4 <- rbind(
  findreplaces4 %>% mutate(from = str_pad(from, 5, pad = "0")),
  findreplaces4
) %>% distinct()

findreplaces4 <- merge(
  data.frame(Year = 1990:2002),
  findreplaces4
)

findreplaces5 <- rbind(
  data.frame(
    replacecolumns = "county",
    from = c(outer(states$STUSPS, 1:maximum_number_counties, FUN = concat)),
    to = c(outer(states$STATEFP, 1:maximum_number_counties, FUN = concat))
  ),
  data.frame(
    replacecolumns = "county",
    from = c(
      c(outer(states$STUSPS, c(0, 999), FUN = concat)),
      not_interested_states,
      c(outer(not_interested_states, 0:maximum_number_counties, FUN = concat)),
      paste0(states$STUSPS, "999"),
      NA,
      0
    ),
    to = "Unknown"
  )
) %>% distinct()

findreplaces5 <- merge(
  data.frame(Year = 2003:2009),
  findreplaces5
)

findreplaces6 <- rbind(
  data.frame(
    replacecolumns = "county",
    from = c(outer(states$STUSPS, 1:maximum_number_counties, FUN = concat)),
    to = c(outer(states$STATEFP, 1:maximum_number_counties, FUN = concat))
  ),
  data.frame(
    replacecolumns = "county",
    from = c(
      c(outer(states$STUSPS, c(0, 999), FUN = concat)),
      not_interested_states,
      c(outer(not_interested_states, 0:maximum_number_counties, FUN = concat)),
      paste0(states$STUSPS, "999"),
      NA,
      0
    ),
    to = "Unknown"
  )
) %>% distinct()

# https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
findreplaces6$to[findreplaces6$to == 15009] <- 15005
findreplaces6$to[findreplaces6$to == 51515] <- 51019
findreplaces6$to[findreplaces6$to == 46113] <- 46102
findreplaces6$to[findreplaces6$to == 02270] <- 02158

findreplaces6 <- merge(
  data.frame(Year = 2010:2016),
  findreplaces6
)

findreplaces_county <- rbind(findreplaces4, findreplaces5, findreplaces6)
rm(findreplaces4, findreplaces5, findreplaces6, maximum_number_counties, concat)

### ---- rural_urban_class---
#TODO
#findreplaces_rural_urban_class <- findreplaces_county %>%
#  select(Year, from) %>%
#  filter(grepl("^\\d", from)) %>%
  #mutate() %>%
#  add_rural_urban_class(FIPS.code.column = "from") %>%
#  mutate(replacecolumns = "rural_urban_class")

findreplaces_rural_urban_class <- rural_urban_class %>%
  mutate(FIPS.code = as.character(FIPS.code)) %>%
  left_join(findreplaces_county, by = c("FIPS.code" = "to")) %>%
  filter(fromYear <= Year) %>%
  group_by(FIPS.code, Year) %>%
  filter(fromYear == max(fromYear)) %>%
  ungroup() %>%
  select(Year, from, to = rural_urban_class) %>%
  mutate(replacecolumns = "rural_urban_class")

findreplaces_rural_urban_class <- rbind(
  findreplaces_county %>%
    filter(to == "Unknown") %>%
    mutate(replacecolumns = "rural_urban_class"),
  merge(
    data.frame(Year = 1990:1999),
    data.frame(
      replacecolumns = "rural_urban_class",
      from = c("02232", "02282"), # corrected manually, somehow missing
      to = c(5, 6)
    )
  ),
  findreplaces_rural_urban_class
)

##-----svi_bin------

findreplaces_svi_bin_after_2000 <- findreplaces_county %>%
  select(Year, from) %>%
  filter(Year >= 2000) %>%
  #filter(can_be_numeric(from)) %>%
  filter(grepl("^\\d", from)) %>%
  #mutate() %>%
  add_social_vuln_index(FIPS.code.column = "from") %>%
  dplyr::mutate(replacecolumns = "svi_bin") %>%
  select(Year, replacecolumns, from, to = svi_bin)

findreplaces_svi_bin_before_2000 <- findreplaces_county %>%
  select(Year, from) %>%
  filter(Year < 2000) %>%
  mutate(replacecolumns = "svi_bin", to = "Unknown") %>%
  select(Year, replacecolumns, from, to)

findreplaces <- rbind(findreplaces_1990_to_1991, findreplaces_1992_to_2002, findreplaces_2003_to_2016,
                      findreplaces_county, findreplaces_rural_urban_class, findreplaces_svi_bin_after_2000, findreplaces_svi_bin_before_2000)

write.csv(findreplaces, findreplaceDir, row.names = FALSE)
rm(findreplaces_1990_to_1991, findreplaces_1992_to_2002, findreplaces_2003_to_2016, findreplaces_rural_urban_class)
