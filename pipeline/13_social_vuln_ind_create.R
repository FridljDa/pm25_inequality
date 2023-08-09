library(dplyr)
library(magrittr)
library(tidyverse)
library(readxl)

#directory to the svi files
svi.dir <- "data/08_svi"
svi.lookup.table <- file.path(svi.dir, "svi_lookup_table.csv")

if(file.exists(svi.lookup.table)){
  quit()
}

#read svi indices
#svi_county_2000 <- readr::read_csv(file.path(svi.dir, "SVI_2000_US_county.csv"), show_col_types = FALSE)
#svi_county_2010 <- readr::read_csv(file.path(svi.dir, "SVI_2010_US_county.csv"), show_col_types = FALSE)
svi_county_2020 <- readr::read_csv(file.path(svi.dir, "SVI_2020_US_county.csv"), show_col_types = FALSE)

#TODO which county version?
#TODO cross walk as in rural_urban_class?
#load("data/ihme_fips.rda")

svi_county_2020 <- svi_county_2020 %>%
  select(county = STCNTY, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES)

#discretize
n_breaks <- 3
svi_county_2020 <- svi_county_2020 %>%
  mutate(
    RPL_THEME1_bins = cut(RPL_THEME1, breaks = n_breaks, labels = FALSE),
    RPL_THEME2_bins = cut(RPL_THEME2, breaks = n_breaks, labels = FALSE),
    RPL_THEME3_bins = cut(RPL_THEME3, breaks = n_breaks, labels = FALSE),
    RPL_THEME4_bins = cut(RPL_THEME4, breaks = n_breaks, labels = FALSE),
    RPL_THEMES_bins = cut(RPL_THEMES, breaks = n_breaks, labels = FALSE) %>% as.factor()
  )

svi_county_2020 <- svi_county_2020 %>%
  select(county, RPL_THEMES_bins)

write.csv(svi_county_2020, file = svi.lookup.table, row.names = FALSE)


