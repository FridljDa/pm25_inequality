#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 06/14/2021
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "tidyr", "testthat", "magrittr", "stringr", "data.table", "tictoc", "foreign")

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

if (year == 1990) {
  ## ---- read census data ----
  states <- read.csv(file.path(tmpDir, "states.csv"))

  dem.state.dir <- file.path(censDir, year)
  dir.create(dem.state.dir, recursive = T, showWarnings = F)
  missing_files <- setdiff(
    paste0("census_", toString(year), "_", states$STUSPS, ".csv"),
    list.files(dem.state.dir)
  )
  if (length(missing_files) > 0) {
    tic("read 1990 population counts")
    cross_bridge <- read.csv(file.path(censDir, "cross_bridge", paste0("cross_meta_", year, ".csv")))
    cens1990 <- fread(file.path(dataDir, "nhgis0002_ds120_1990_tract.csv"))
    
    cols <- colnames(cens1990)[startsWith(colnames(cens1990), "ET")]
    cols <- c("state" = "STATEA", "county" = "COUNTYA", "tract" = "TRACTA", "ANPSADPI", cols) #"GEO_ID" = "GISJOIN", 
    cens1990 <- cens1990 %>% select(all_of(cols))
    
    cens1990 <- cens1990 %>%
      mutate(tract = ifelse(str_detect(ANPSADPI, "\\."),
                            tract,
                            tract*100),
             GEO_ID = paste0( 
               state,
               sprintf("%03d", county),
               sprintf("%06d", tract)
             ),
             GEO_ID = as.character(GEO_ID), 
             ANPSADPI = NULL
             )
    
    cens1990 <- cens1990 %>%
      pivot_longer(
        cols = -c("GEO_ID", "state", "county", "tract"),
        names_to = "variable",
        values_to = "pop_size"
      )

    cens1990 <- cens1990 %>% right_join(cross_bridge, by = c("variable" = "variable.y"))
    cens1990 <- cens1990 %>%
      mutate(pop_size = pop_size * coeff) %>%
      group_by(state, county, tract, GEO_ID, variable.x) %>%
      summarize(pop_size = sum(pop_size)) %>%
      rename(variable = variable.x) %>%
      ungroup()

    apply(states, 1, function(state) {
      STATEFP <- state["STATEFP"] %>% as.numeric()
      STUSPS <- state["STUSPS"]
      dem.state.dirX <- file.path(dem.state.dir, paste0("census_", toString(year), "_", STUSPS, ".csv"))

      if (!file.exists(dem.state.dirX)) {
        cens1990_sub <- cens1990 %>% filter(state == STATEFP)
        fwrite(cens1990_sub, dem.state.dirX)
      }
    })
    toc()
  }
}
