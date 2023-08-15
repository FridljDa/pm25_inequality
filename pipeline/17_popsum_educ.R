#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize population by education from granular data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
# rm(list = ls(all = TRUE))

suppressMessages({
  library(magrittr)
  #library(data.table)
  library(tidyverse)
  library(tictoc)
  library(readxl)
  library(dplyr)
  library(foreach)
})

devtools::load_all()


options(scipen = 10000)
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  year <- 2001
  agr_by <- "nation"

  dataDir <- "data"
  tmpDir <- "data/tmp"
  censDir <- "data/05_demog"
  pop.summary.dir <- "data/12_population_summary"
} else {
  year <- args[1]
  dataDir <- args[2]
  agr_by <- args[10]
  tmpDir <- args[3]
  censDir <- args[8]
  pop.summary.dir <- args[16]
}

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>% read.csv()

plotDir <- file.path(pop.summary.dir, "plot", agr_by)
dir.create(plotDir, recursive = T, showWarnings = F)
# load meta data

pop.summary.dir <- file.path(pop.summary.dir, agr_by)
dir.create(pop.summary.dir, recursive = T, showWarnings = F)
pop.summary.dirX <- file.path(pop.summary.dir, paste0("pop_sum_", year, ".csv"))

if (file.exists(pop.summary.dirX)) {
  quit()
}

census_meta <- file.path(censDir, "meta", paste0("cens_meta_", toString(year), ".csv")) %>% read_data()

#states <- states[1:5,]
# Begin a loop to summarize population data for each state in the given year
# agr_by specifies the level at which to aggregate the data (e.g., "county")
tic(paste("summarized population data in", year, "by", agr_by))
pop.summary = foreach(i = seq_len(nrow(states)), .combine = rbind) %do% {
  state <- states[i,]

  STUSPS <- state[["STUSPS"]]
  name <- state[["NAME"]]
  STATEFP <- state[["STATEFP"]]

  # Read demographic census data by tract
  pop.summary <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>% read_data() %>%
    distinct() %>%
    mutate(FIPS.code = paste0(state, stringr::str_pad(county, 3, pad = "0")) %>% as.integer())

  # Aggregate by county if specified
  if (agr_by == "county") {
    pop.summary <- pop.summary %>%
      ungroup() %>%
      dplyr::group_by(state, county, FIPS.code, variable) %>%
      dplyr::summarize(Population = sum(pop_size))
  }

  return(pop.summary)
}

# Add additional columns for rural/urban classification and social vulnerability index
pop.summary <- pop.summary %>%
  mutate(Year = year) %>%
  add_rural_urban_class(FIPS.code.column = "FIPS.code") %>%
  add_social_vuln_index(FIPS.code.column = "FIPS.code") %>%
  mutate(Year = NULL)

# Summarize population by different categories
pop.summary.all <- pop.summary %>%
  group_by(state, variable) %>%
  summarize(Population = sum(pop_size)) %>%
  mutate(rural_urban_class = as.factor(666), svi_bin = as.factor(666)) # TODO

pop.summary.rural_urban_class <- pop.summary %>%
  group_by(state, variable, rural_urban_class) %>%
  summarize(Population = sum(pop_size)) %>%
  mutate(svi_bin = as.factor(666))

pop.summary.svi_bin <- pop.summary %>%
  group_by(state, variable, svi_bin) %>%
  summarize(Population = sum(pop_size)) %>%
  mutate(rural_urban_class = as.factor(666))

# Combine all summary data into one object
pop.summary <- rbind(pop.summary.all, pop.summary.rural_urban_class, pop.summary.svi_bin)


if (agr_by == "county") {
  pop.summary <- pop.summary %>%
    filter(rural_urban_class == 666, svi_bin == 666) #TODO
  #  dplyr::mutate(county = paste0(state, str_pad(county, 3, pad = "0")) %>% as.integer()) %>%
  #  dplyr::group_by(county, variable) %>% # state,
  #  dplyr::summarize(Population = sum(Population)) %>%
  #  dplyr::mutate(rural_urban_class = as.factor(666))
} else {
  pop.summary <- states %>%
    right_join(pop.summary, by = c("STATEFP" = "state")) %>%
    dplyr::group_by_at(vars(all_of(c(agr_by, "variable", "rural_urban_class")))) %>%
    summarize(Population = sum(Population)) %>%
    as.data.frame()
}

pop.summary <- pop.summary %>%
  left_join(census_meta, by = "variable") %>%
  select(-c(variable))

pop.summary <- pop.summary %>% tibble::add_column(source2 = "Census")
# only consider 25+ population
pop.summary <- pop.summary %>% filter(min_age >= 25)
write.csv(pop.summary, pop.summary.dirX, row.names = FALSE)
toc()
