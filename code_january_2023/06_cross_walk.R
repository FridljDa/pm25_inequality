#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "tidyr", "testthat", "magrittr", "stringr", "data.table", "tictoc", "foreign", "tigris")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1] %>% as.numeric()
dataDir <- args[2]
tmpDir <- args[3]
censDir <- args[8]

if (!year %in% c(1990, 2000)) {
  print(paste("can not crosswalk census tracts in", year))
  quit()
}

censDirFrom <- file.path(censDir, year)
censDirTo <- file.path(censDir, paste0(year, "_in_2010"))
dir.create(censDirTo, recursive = T, showWarnings = F)

states <- file.path(tmpDir, "states.csv") %>% read.csv()
possible_states <- states$STATEFP

crosswalk <- read.csv(file.path(dataDir, paste0("crosswalk_", year, "_2010.csv")))
if (year == 1990) {
  crosswalk <- crosswalk %>% select(trtidFrom = trtid90, trtidTo = trtid10, weight)
} else if (year == 2000) {
  crosswalk <- crosswalk %>% select(trtidFrom = trtid00, trtidTo = trtid10, weight)
}

crosswalk <- crosswalk %>%
  mutate(
    stateFrom = str_sub(trtidFrom, 1, -10),
    trtidTo = as.character(trtidTo),
    stateTo = str_sub(trtidTo, 1, -10),
    countyTo = str_sub(trtidTo, -9, -7), 
    tractTo = str_sub(trtidTo, -6, -1), 
    
    trtidTo = as.numeric(trtidTo)
  )

crosswalk <- crosswalk %>% filter(stateFrom %in% possible_states)

# states, for which 2000 and 2010 still needs to be calculated
missing_statesDir <- file.path(censDirTo, "missing_states.csv")
if (!file.exists(missing_statesDir)) write.csv(states, missing_statesDir)
missing_states <- read.csv(missing_statesDir)

## -----calculate 2010 data in 2000 boundaries and meta data -----
apply(missing_states, 1, function(state) {
  STATEFP <- state["STATEFP"] %>% as.numeric()
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  if (!is.na(STUSPS)) {
    tic(paste("calculated,", year, "demographic census data in 2010 boundaries in", name))
    # read demographic census data by tract,
    censDataFrom <- file.path(censDirFrom, paste0("census_", year, "_", STUSPS, ".csv")) %>%
      fread(colClasses = c(pop_size = "numeric")) %>%
      select(GEO_ID,state,county, tract, variable, pop_size) %>%
      mutate_at(c("state","county","tract"), as.character)

    #if (year == 1990) {
    #  censDataFrom <- censDataFrom %>% 
    #    mutate(GEO_ID = as.character(GEO_ID),
    #           GEO_ID = case_when(str_sub(GEO_ID,-2,-1) == "00"~ str_sub(GEO_ID,1,-3), #,"99"
    #                              str_sub(GEO_ID,-2,-1) == "99"~ paste0(str_sub(GEO_ID,1,-3)), #,"01"
    #                               TRUE ~ GEO_ID)
    #           )
    #  
    #  censDataFrom <- censDataFrom %>% 
    #    group_by(GEO_ID, state, county, tract, variable) %>%
    #    summarise(pop_size = sum(pop_size))
    #}
    censDataFrom <- censDataFrom %>%  mutate_at(c("state","GEO_ID"), as.character)
    crosswalk <- crosswalk %>% mutate_at(c("stateFrom","trtidFrom"), as.character) #TODO why not integer
    
    censDataFrom_old <- censDataFrom


    # translate tracts
    censDataFrom <- censDataFrom %>%
      inner_join(crosswalk, by = c("GEO_ID" = "trtidFrom", "state"="stateFrom")) %>%
      mutate(
        pop_size = pop_size * weight
      ) %>%
      group_by(stateTo,countyTo,tractTo,trtidTo, variable) %>%
      summarise(pop_size = sum(pop_size)) %>%
      rename(GEO_ID = trtidTo, state = stateTo, county = countyTo, tract = tractTo) %>%
      as.data.frame()

    
    test <- lapply(unique(censDataFrom$state), function(statefp) {
      censDataFrom_sub <- censDataFrom %>%
        filter(state == statefp) 
      
      STUSPS_corresponding <- states[states[, "STATEFP"] == as.numeric(statefp), "STUSPS"]
      if (STUSPS_corresponding == "") browser()
      censDirToX <- file.path(censDirTo, paste0("census_", year, "_", STUSPS_corresponding, ".csv"))

      suppressWarnings(
        write.table(censDataFrom_sub,
          censDirToX,
          sep = ",",
          col.names = !file.exists(censDirToX),
          append = T,
          row.names = F
        )
      )

      return(censDataFrom_sub)
    }) %>% rbindlist()

    # delete this state from missing_states
    STUSPS_copy <- STUSPS
    missing_statesDir %>%
      read.csv() %>%
      filter(STUSPS != STUSPS_copy) %>%
      write.csv(missing_statesDir)
  }
})
