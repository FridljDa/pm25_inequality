library(dplyr)
library(magrittr)
library(tidyverse)
library(readxl)

#directory to the svi files
svi.dir <- "data/08_svi"
svi.lookup.table <- file.path(svi.dir, "svi_lookup_table.csv")

if(file.exists(svi.lookup.table)){
  #quit()
}

#https://www.atsdr.cdc.gov/placeandhealth/svi/
#read svi indices
svi_county_2000 <- readr::read_csv(file.path(svi.dir, "SVI_2000_US_county.csv"), show_col_types = FALSE)
svi_county_2010 <- readr::read_csv(file.path(svi.dir, "SVI_2010_US_county.csv"), show_col_types = FALSE)
svi_county_2014 <- readr::read_csv(file.path(svi.dir, "SVI_2014_US_county.csv"), show_col_types = FALSE)
svi_county_2016 <- readr::read_csv(file.path(svi.dir, "SVI_2016_US_county.csv"), show_col_types = FALSE)
svi_county_2018 <- readr::read_csv(file.path(svi.dir, "SVI_2018_US_county.csv"), show_col_types = FALSE) #TODO
svi_county_2020 <- readr::read_csv(file.path(svi.dir, "SVI_2020_US_county.csv"), show_col_types = FALSE)

#add year
#https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/pdf/SVI2000Documentation-H.pdf
#USG1TP Socioeconomic
#XXG2TP
#XXG3TP
#XXG4TP

svi_county_2000 <- svi_county_2000 %>%
  select(county = STCOFIPS, svi= USTP,
         svi1 = USG1TP, svi2 = USG2TP, svi3 = USG3TP, svi4 = USG4TP) %>%
  mutate(fromYear = 2000)
#TOTAL PERCENTILE
#https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/pdf/SVI-2010-Documentation-H.pdf
#R_PL_THEME1 Socioeconomic theme

svi_county_2010 <- svi_county_2010 %>%
  select(county = FIPS, svi = R_PL_THEMES,
         svi1 = R_PL_THEME1, svi2 = R_PL_THEME2, svi3 = R_PL_THEME3, svi4 = R_PL_THEME4) %>%
  mutate(fromYear = 2010) %>%
  filter(svi != -999)

#https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/pdf/SVI2014Documentation_01192022.pdf
#Socioeconomic theme - RPL_THEME1
# Household Composition and Disability - RPL_THEME2
# Minority Status & Language - RPL_THEME3
# Housing & Transportation - RPL_THEME4
svi_county_2014 <- svi_county_2014 %>%
  select(county = FIPS, svi = RPL_THEMES,
         svi1 = RPL_THEME1, svi2 = RPL_THEME2, svi3 = RPL_THEME3, svi4 = RPL_THEME4) %>%
  mutate(fromYear = 2014) %>%
  filter(svi != -999)

#https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/pdf/SVI2016Documentation_01192022.pdf
#Socioeconomic - RPL_THEME1
# Household Composition & Disability - RPL_THEME2
# Minority Status & Language - RPL_THEME3
# Housing Type & Transportation - RPL_THEME4
svi_county_2016 <- svi_county_2016 %>%
  select(county = FIPS, svi = RPL_THEMES,
         svi1 = RPL_THEME1, svi2 = RPL_THEME2, svi3 = RPL_THEME3, svi4 = RPL_THEME4) %>%
  mutate(fromYear = 2016) %>%
  filter(svi != -999)

#https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/pdf/SVI2020Documentation_08.05.22.pdf
#Socioeconomic Status - RPL_THEME1
# Household Characteristics - RPL_THEME2
# Racial & Ethnic Minority Status - RPL_THEME3
# Housing Type & Transportation - RPL_THEME4
svi_county_2020 <- svi_county_2020 %>%
  select(county = STCNTY, svi = RPL_THEMES,
         svi1 = RPL_THEME1, svi2 = RPL_THEME2, svi3 = RPL_THEME3, svi4 = RPL_THEME4) %>%
  mutate(fromYear = 2020)
#TODO which county version?
#TODO cross walk as in rural_urban_class?
#load("data/ihme_fips.rda")
n_breaks <- 3
svi_county_list <- list(svi_county_2000, svi_county_2010, svi_county_2014, svi_county_2016, svi_county_2020)

svi_county_list <- lapply(svi_county_list, function(svi_county_i){
  svi_county_i <- svi_county_i %>%
    mutate(svi_bin = cut(svi, breaks = n_breaks, labels = FALSE) %>% as.factor(),
           svi_bin1 = cut(svi1, breaks = n_breaks, labels = FALSE) %>% as.factor(),
           svi_bin2 = cut(svi2, breaks = n_breaks, labels = FALSE) %>% as.factor(),
           svi_bin3 = cut(svi3, breaks = n_breaks, labels = FALSE) %>% as.factor(),
           svi_bin4 = cut(svi4, breaks = n_breaks, labels = FALSE) %>% as.factor())
})
#discretize

svi_county <- data.table::rbindlist(svi_county_list)

svi_county <- svi_county %>%
  select(fromYear, county, svi_bin, svi_bin1, svi_bin2, svi_bin3, svi_bin4)

write.csv(svi_county, file = svi.lookup.table, row.names = FALSE)

table(svi_county$fromYear, svi_county$svi_bin)
