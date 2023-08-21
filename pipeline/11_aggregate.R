#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: aggregate
#
#***************************************************************************
#*

# clear memory
# rm(list = ls(all = TRUE))

# load packages
library(dplyr)
library(magrittr)
library(data.table)
library(testthat)
library(tidyverse)
library(readxl)
library(stringr)
library(tictoc)
pkgload::load_all()

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  year <- 2001
  agr_by <- "nation"

  dataDir <- "data"
  tmpDir <- "data/tmp"
  exp_tracDir <- "data/03_exp_tracts"
  censDir <- "data/05_demog"
  cens_agrDir <- "data/06_dem.agr"
} else {
  year <- args[1]
  dataDir <- args[2]
  tmpDir <- args[3]
  exp_tracDir <- args[7]
  censDir <- args[8]
  cens_agrDir <- args[9]
  agr_by <- args[10]
}

if (!agr_by %in% c("county", "Census_Region", "Census_division", "hhs_region_number", "STATEFP", "nation")) {
  print(paste(agr_by, "is an invalid agr_by argument"))
  quit()
}

cens_agrDirC <- file.path(cens_agrDir, "county", year)
dir.create(cens_agrDirC, recursive = T, showWarnings = F)

cens_agrDir <- file.path(cens_agrDir, agr_by, year)
dir.create(cens_agrDir, recursive = T, showWarnings = F)

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>%
  read_csv() %>%
  as.data.frame()


## ---- calculate county-------

# calculate first on county level, even if agr_by != "county"
# loop over all states
for (i in seq_len(nrow(states))) {
  state <- states[i, ]
  STUSPS <- state[["STUSPS"]]
  name <- state[["NAME"]]

  cens_agrDirCX <- file.path(cens_agrDirC, paste0("cens_agr_", toString(year), "_", STUSPS, ".csv"))

  if (file.exists(cens_agrDirCX)) {
    next
  }

  # if not calculated for this state yet
  # if (!file.exists(cens_agrDirCX)) {
  tic(paste("Aggregated Census data in", name, "in year", year, "by pm and county"))

  # read demographic census data by tract
  trac_censData <- file.path(censDir, year, paste0("census_", toString(year), "_", STUSPS, ".csv")) %>%
    fread() %>%
    mutate(GEO_ID = as.character(GEO_ID))

  # read pm exposure data by tract
  exp_tracDataDir <- file.path(exp_tracDir, year, paste0("exp_trac_", toString(year), "_", STUSPS, ".csv"))
  if (!file.exists(exp_tracDataDir) & year < 2000 & STUSPS %in% c("AK", "HI")) {
    return()
  }

  exp_tracData <- fread(exp_tracDataDir) %>% mutate(GEO_ID = as.character(GEO_ID))

  # stylized scenarios
  exp_tracData <- exp_tracData %>% mutate(scenario = "real")
  if (agr_by == "nation") {
    exp_tracData <- rbind(
      exp_tracData,
      exp_tracData %>% mutate(
        scenario = "NAAQS",
        pm = pmin(pm, 12)
      ),
      exp_tracData %>% mutate(
        scenario = "WHO",
        pm = pmin(pm, 10)
      ) # ,
      # exp_tracData %>% mutate(scenario = "future threshold",
      #                        pm = pmin(pm, 8))
    )
  }

  # tigris does not provide all tract boundaries
  # TODO
  anti <- anti_join(trac_censData, exp_tracData, by = "GEO_ID") %>% filter(pop_size > 0)

  if (nrow(anti) > 0) {
    # 2; 13; 1; 20130001
    # 12, 1, 2, 120010002
    anti <- anti %>%
      group_by(GEO_ID) %>%
      summarise(pop_size = sum(pop_size))

    print(paste(nrow(anti), "GEO_ID worth", sum(anti$pop_size), "persons missing in exposure-tract data in", year, "in", name))
    print(anti$GEO_ID)


    anti2 <- anti_join(exp_tracData, trac_censData, by = "GEO_ID")
  }

  cens_agr <- inner_join(trac_censData,
    exp_tracData,
    by = "GEO_ID",
    multiple = "all" # matching multiple because of multiple scenarios
  ) %>%
    group_by(state, county, variable, scenario, pm) %>%
    # calculate number of persons of exposed to particulare level of exposure,
    # in particulare county by sex, age group, ethinicity, hispanic origin
    summarise(pop_size = sum(pop_size)) %>%
    filter(pop_size != 0)

  cens_agr <- cens_agr %>%
    group_by(state, county, variable, scenario) %>% # no pm
    mutate(prop = pop_size / sum(pop_size)) %>%
    ungroup()

  cens_agr <- cens_agr %>% mutate(county = sprintf("%s%03d", state, county) %>% as.numeric())


  # add rural classification and svi
  cens_agr <- cens_agr %>%
    mutate(Year = year) %>%
    add_rural_urban_class(FIPS.code.column = "county") %>%
    add_social_vuln_index(FIPS.code.column = "county") %>%
    mutate(Year = NULL)


  fwrite(cens_agr, cens_agrDirCX)
  toc()
}
# })

## ------ calculate not county -----
# if agr_by != "county", aggregate data from above according to agr_by

if (agr_by != "county") {
  regions <- states[, agr_by] %>% unique()

  for (region in regions) {
    cens_agrDirX <- paste0("cens_agr_", toString(year), "_", region, ".csv") %>%
      file.path(cens_agrDir, .)

    if (file.exists(cens_agrDirX)) {
      next
    }
    # if (!file.exists(cens_agrDirX)) {
    tic(paste("Aggregated Census data in", agr_by, region, "in year", year, "by pm"))
    statesX <- states[states[, agr_by] == region, "STUSPS"]

    # rbind all states from this region
    cens_agr <- lapply(statesX, function(STUSPS) {
      cens_agrDir <- file.path(cens_agrDirC, paste0("cens_agr_", toString(year), "_", STUSPS, ".csv"))
      if (!file.exists(cens_agrDir) & year < 2000 & STUSPS %in% c("AK", "HI")) { #
        return(NULL)
      } else {
        return(read_data(cens_agrDir))
      }
    }) %>%
      rbindlist() %>%
      as.data.frame()

    # cens_agr$rural_urban_class <- NULL
    if (nrow(cens_agr) <= 0) {
      return(NULL)
    }
    cens_agr_all <- cens_agr %>%
      group_by(variable, scenario, pm) %>%
      summarise(pop_size = sum(pop_size)) %>%
      mutate(rural_urban_class = "666", svi_bin = "666") %>%
      group_by(variable, rural_urban_class, scenario, svi_bin) %>%
      mutate(prop = pop_size / sum(pop_size))

    cens_agr_rural_urban_class <- cens_agr %>%
      filter(!is.na(rural_urban_class) & rural_urban_class != "Unknown") %>%
      group_by(variable, rural_urban_class, scenario, pm) %>%
      summarise(pop_size = sum(pop_size)) %>%
      mutate(svi_bin = "666") %>%
      group_by(variable, rural_urban_class, scenario, svi_bin) %>%
      mutate(prop = pop_size / sum(pop_size))

    cens_agr_svi <- cens_agr %>%
      filter(!is.na(svi_bin) & svi_bin != "Unknown") %>%
      group_by(variable, svi_bin, scenario, pm) %>%
      summarise(pop_size = sum(pop_size)) %>%
      mutate(rural_urban_class = "666") %>%
      group_by(variable, rural_urban_class, scenario, svi_bin) %>%
      mutate(prop = pop_size / sum(pop_size))

    cens_agr <- rbind(cens_agr_all, cens_agr_rural_urban_class, cens_agr_svi)
    rm(cens_agr_all, cens_agr_rural_urban_class, cens_agr_svi)

    # add proportions

    cens_agr_check <- cens_agr %>%
      group_by(variable, rural_urban_class, svi_bin, scenario) %>%
      summarise(sum_prop = sum(prop))

    assertthat::are_equal(cens_agr_check$sum_prop, rep(1, nrow(cens_agr_check)), tol = 0.01)

    # add region
    cens_agr[, agr_by] <- region

    write.csv(cens_agr, cens_agrDirX, row.names = FALSE)


    toc()
  }
}
