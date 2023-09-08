library(dplyr)
library(magrittr)
#library(tidyverse)
library(readxl)

#directory to the svi files
svi.dir <- "data/08_svi"
svi.lookup.table <- file.path(svi.dir, "svi_lookup_table.csv")

if(file.exists(svi.lookup.table)){
  quit()
}

#read svi indices
svi_county_2000 <- readr::read_csv(file.path(svi.dir, "SVI_2000_US_county.csv"), show_col_types = FALSE)
svi_county_2010 <- readr::read_csv(file.path(svi.dir, "SVI_2010_US_county.csv"), show_col_types = FALSE) #TODO
svi_county_2014 <- readr::read_csv(file.path(svi.dir, "SVI_2014_US_county.csv"), show_col_types = FALSE)
svi_county_2016 <- readr::read_csv(file.path(svi.dir, "SVI_2016_US_county.csv"), show_col_types = FALSE)
svi_county_2018 <- readr::read_csv(file.path(svi.dir, "SVI_2018_US_county.csv"), show_col_types = FALSE)
svi_county_2020 <- readr::read_csv(file.path(svi.dir, "SVI_2020_US_county.csv"), show_col_types = FALSE)

#add year
svi_county_2000 <- svi_county_2000 %>%
  select(county = STCOFIPS, svi= USTP) %>% #RPL_THEMES TODO
  mutate(fromYear = 2000)
#TOTAL PERCENTILE
svi_county_2010 <- svi_county_2010 %>%
  select(county = FIPS, svi = R_PL_THEMES) %>%
  mutate(fromYear = 2010) %>%
  filter(svi != -999)

svi_county_2014 <- svi_county_2014 %>%
  select(county = FIPS, svi = RPL_THEMES) %>%
  mutate(fromYear = 2014) %>%
  filter(svi != -999)

svi_county_2016 <- svi_county_2016 %>%
  select(county = FIPS, svi = RPL_THEMES) %>%
  mutate(fromYear = 2016) %>%
  filter(svi != -999)

svi_county_2020 <- svi_county_2020 %>%
  select(county = STCNTY, svi = RPL_THEMES) %>%
  mutate(fromYear = 2020)
#TODO which county version?
#TODO cross walk as in rural_urban_class?
#load("data/ihme_fips.rda")

#discretize
n_breaks <- 3
svi_county_2000 <- svi_county_2000 %>%
  mutate(svi_bin = cut(svi, breaks = n_breaks, labels = FALSE) %>% as.factor())

svi_county_2010 <- svi_county_2010 %>%
  mutate(svi_bin = cut(svi, breaks = n_breaks, labels = FALSE) %>% as.factor())

svi_county_2014 <- svi_county_2014 %>%
  mutate(svi_bin = cut(svi, breaks = n_breaks, labels = FALSE) %>% as.factor())

svi_county_2016 <- svi_county_2016 %>%
  mutate(svi_bin = cut(svi, breaks = n_breaks, labels = FALSE) %>% as.factor())

svi_county_2020 <- svi_county_2020 %>%
  mutate(svi_bin = cut(svi, breaks = n_breaks, labels = FALSE) %>% as.factor())

svi_county <- rbind(svi_county_2000, svi_county_2010, svi_county_2014, svi_county_2016, svi_county_2020)

svi_county <- svi_county %>%
  select(fromYear, county, svi_bin)

write.csv(svi_county, file = svi.lookup.table, row.names = FALSE)

table(svi_county$fromYear, svi_county$svi_bin)
