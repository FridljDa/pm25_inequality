
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "data.table", "tidyverse",
  "tictoc",  "testthat" #"cdcfluview" , "rlang"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
censDir <- args[8]

# quits, if not downloadable year
if (!year %in% c(2000, 2009:2016)) {
  print(paste("can not download census data for", year))
  quit()
}

# intense computation
if (Sys.info()["sysname"] == "Windows") memory.limit(size = 500000)

## ----------read useful data to tmp-------------------------------------------------------------------------------

states <- read.csv(file.path(tmpDir, "states.csv"))

### ------------------------download demographic data-----------------------------------------------------------------
# Add key to .Renviron
key <- "your key"
Sys.setenv(CENSUS_KEY = key)

census_meta <- read.csv(file.path(censDir, "meta_down", paste0("cens_meta_", toString(year), ".csv")))
census_metan_new <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", toString(year), ".csv")))
cross_bridge <- read.csv(file.path(censDir, "cross_bridge", paste0("cross_meta_", year, ".csv")))
# identify relevant variables
if (year %in% 2001:2009) {
  census_meta <- census_meta %>% filter(tablename != "dec/sf1")
}

relevant_variables <- census_meta$variable %>% unique()
table_groups <- census_meta %>%
  select(group, tablename) %>%
  distinct()
## ---------------- download sex by age for each race----------------------
censDir <- file.path(censDir, year)
dir.create(censDir, recursive = T, showWarnings = F)

tic(paste("Downloaded census data in year", toString(year)))
# loop over all states
apply(states, 1, function(state) {
  STATEFP <- state["STATEFP"] %>% as.numeric()
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  dem.state.dir <- paste0("census_", toString(year), "_", STUSPS, ".csv") %>%
    file.path(censDir, .)

  # download if does not exist yet
  if (!file.exists(dem.state.dir)) {
    tic(paste("Downloaded census data in year", toString(year), "in", name))

    # loop over all groups, download data
    dem.state.data <- apply(table_groups, 1, function(row) {
      group <- row[["group"]]
      tic(paste("Downloaded census data in year", toString(year), "in", name, "for group", group))
      dem.state.data <- getCensus(
        name = row[["tablename"]],
        vintage = year,
        vars = paste0("group(", group, ")"),
        region = "tract:*",
        regionin = sprintf("state:%02d", STATEFP)
      )

      toc()
      return(dem.state.data)
    }) %>%
      rbindlist(fill = TRUE) %>%
      as.data.frame()
    
    ### ----- harmonize GEO----
    dem.state.data <- dem.state.data %>% mutate(GEO_ID = sub(".*US", "", GEO_ID))
    if(year == 2000){
    }else if(year == 2010){
    }else if(year %in%c(2009,2011:2016)){
    }
    ##---make longer----
    dem.state.data <- dem.state.data %>%
      select(all_of(c(relevant_variables, "state", "county", "tract", "GEO_ID"))) %>%
      pivot_longer(
        cols = !c("state", "county", "tract", "GEO_ID"),
        names_to = "variable",
        values_to = "pop_size",
        values_drop_na = F
      )
    dem.state.data$pop_size[is.na(dem.state.data$pop_size)] <- 0
    toc()

    ## ---- make additional calculations-----
    tic(paste("Made additional calculations with census data in year", toString(year), "in", name))

    dem.state.data <- dem.state.data %>% right_join(cross_bridge, by = c("variable" = "variable.y"))

    dem.state.data <- dem.state.data %>%
      mutate(pop_size = pop_size * coeff) %>%
      group_by(state, county, tract, GEO_ID, variable.x) %>%
      summarize(pop_size = sum(pop_size)) %>%
      rename(variable = variable.x) %>%
      ungroup()

      toc()

    # save demographic data in seperate file for each state
    fwrite(dem.state.data, dem.state.dir, row.names = FALSE)
    toc()
  }
})
toc()
""
