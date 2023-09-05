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
  #library(tidyverse)
  library(tictoc)
  library(readxl)
  library(dplyr)
  library(foreach)
  library(purrr)
})

suppressMessages({pkgload::load_all()})


options(scipen = 10000)
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  year <- 2016
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

#if(rlang::is_empty(args) & TRUE){
#  states <- states[1,]
#}

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

#if(rlang::is_empty(args) & FALSE){
#  pop.summary <- pop.summary %>% sample_n(20)
#}



# Add additional columns for rural/urban classification and social vulnerability index
cat("add svi bin and rural urban class to pop.summary-start")
tic("added svi bin and rural urban class to pop.summary")
pop.summary <- pop.summary %>%
  mutate(Year = year) %>%
  add_rural_urban_class(FIPS.code.column = "FIPS.code") %>%
  add_social_vuln_index(FIPS.code.column = "FIPS.code") %>%
  mutate(Year = NULL)
toc()
#print(paste("column names in pop.summary:", colnames(pop.summary)))
# Summarize population by different categories

cat("add summarise svi_bin and rural_urban_class out in pop.summary-start")
tic("add summarise svi_bin and rural_urban_class out in pop.summary")
# For the 'all' summary
# Determine the population column name
# Determine the population column name
pop_col <- if ("pop_size" %in% colnames(pop.summary)) "pop_size" else "Population"

# Create the 'all' summary
pop.summary.all <- pop.summary %>%
  group_by(across(-all_of(c(pop_col, "rural_urban_class", "svi_bin")))) %>%
  summarize(Population = sum(!!sym(pop_col)), .groups = "drop") %>%
  mutate(rural_urban_class = as.factor(666), svi_bin = as.factor(666))

# Create the 'rural_urban_class' summary
pop.summary.rural_urban_class <- pop.summary %>%
  group_by(across(-all_of(c(pop_col, "svi_bin")))) %>%
  summarize(Population = sum(!!sym(pop_col)), .groups = "drop") %>%
  mutate(svi_bin = as.factor(666))

# Create the 'svi_bin' summary
pop.summary.svi_bin <- pop.summary %>%
  group_by(across(-all_of(c(pop_col, "rural_urban_class")))) %>%
  summarize(Population = sum(!!sym(pop_col)), .groups = "drop") %>%
  mutate(rural_urban_class = as.factor(666))

toc()

# Combine all summary data into one object
pop.summary <- rbind(pop.summary.all, pop.summary.rural_urban_class, pop.summary.svi_bin)
rm(pop.summary.all, pop.summary.rural_urban_class, pop.summary.svi_bin)

#sum up
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
    dplyr::group_by_at(vars(all_of(c(agr_by, "variable", "rural_urban_class", "svi_bin")))) %>%
    summarize(Population = sum(Population)) %>%
    as.data.frame()
}

pop.summary <- pop.summary %>%
  left_join(census_meta, by = "variable") %>%
  select(-c(variable))

#browser()
# Create the 'all' summary
#pop.summary.education <- pop.summary %>%
#  filter(Education != "666")

#pop.summary.all.education <- pop.summary %>%
#  group_by(across(-all_of(c("Population", "Education")))) %>%
#  summarize(Population = sum(Population), .groups = "drop") %>%
#  mutate(Education = as.factor(666))

#pop.summary <- rbind(pop.summary.education, pop.summary.all.education)
#rm(pop.summary.education, pop.summary.all.education)

pop.summary <- pop.summary %>% tibble::add_column(source2 = "Census")
# only consider 25+ population
pop.summary <- pop.summary %>% filter(min_age >= 25)

pop_col <- if ("pop_size" %in% colnames(pop.summary)) "pop_size" else "Population"
#check if is partition
sanity_check <- pop.summary %>%
    group_by(across(-all_of(c(pop_col, "min_age", "max_age")))) %>%
    nest() %>%
    mutate(has_overlaps = map(data, ~ has_overlaps(.x))) %>%
    unnest(cols = c(has_overlaps))

if(any(sanity_check$has_overlaps)){
  stop("in 17_popsum_educ.R, pop.summary has overlaps")
}
#result <- pop.summary %>%
#  group_by(across(-all_of(c(pop_col, "min_age", "max_age")))) %>%
#  nest() %>%
#  mutate(is_partition = map(data, ~ is_partition(.x))) %>%
#  unnest(cols = c(is_partition))

print(paste("written file to", pop.summary.dirX))
write.csv(pop.summary, pop.summary.dirX, row.names = FALSE)
toc()
