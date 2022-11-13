#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
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

states <- file.path(tmpDir, "states.csv") %>% read.csv()
censDirTo <- file.path(censDir, year)
dir.create(censDirTo, recursive = T, showWarnings = F)

if (year %in% 2001:2008) {
  year_lower <- 2000
  censDirLower <- file.path(censDir, "2000_in_2010")
  year_upper <- 2010
  censDirUpper <- file.path(censDir, 2010)
  
  metaUpper <- read.csv(file.path(censDir, "meta", "cens_meta_2010.csv")) %>%
    filter(Education == 666)
} else if (year %in% 1991:1999) {
  year_lower <- 1990
  censDirLower <- file.path(censDir, "1990_in_2010")
  year_upper <- 2000
  censDirUpper <- file.path(censDir, "2000_in_2010")
  metaUpper <- read.csv(file.path(censDir, "meta", "cens_meta_2000.csv")) %>%
    filter(Hispanic.Origin == "All Origins")
  
} else {
  print(paste("can not interpolate census data for", year))
  quit()
}


## ---calculation----
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  censDirToX <- file.path(censDirTo, paste0("census_", toString(year), "_", STUSPS, ".csv"))

    
  if (!file.exists(censDirToX) ) {
    tic(paste("interpolated data in", year, "in", name))
    censDataLower <- fread(file.path(censDirLower, paste0("census_", year_lower, "_", STUSPS, ".csv"))) %>%
      complete(nesting(GEO_ID, state, county, tract), variable, fill = list(pop_size = 0)) %>%
      rename(pop_sizeLower = pop_size) %>%
      mutate(GEO_ID = as.character(GEO_ID))
      
    censDataUpper <- fread(file.path(censDirUpper, paste0("census_", year_upper, "_", STUSPS, ".csv"))) %>%
      complete(nesting(GEO_ID, state, county, tract), variable, fill = list(pop_size = 0)) %>%
      rename(pop_sizeUpper = pop_size)%>%
      mutate(GEO_ID = as.character(GEO_ID))
    
    censDataLower <- censDataLower %>% filter(variable %in% censDataUpper$variable)
    censDataUpper <- censDataUpper %>% filter(variable %in% censDataLower$variable)
    
    #censDataUpper <- censDataUpper %>% filter(variable %in% metaUpper$variable)
    censData_joined <- full_join(censDataLower, censDataUpper, by = c("state","GEO_ID", "variable")) %>% #TODO "county","tract",
      mutate(county = coalesce(county.y, county.x), county.y = NULL, county.x = NULL,
             tract = coalesce(tract.y, tract.x), tract.y = NULL, tract.x = NULL,
             pop_sizeLower = coalesce(pop_sizeLower,0), pop_sizeUpper = coalesce(pop_sizeUpper,0))
      #filter(!(pop_sizeLower == 0 & is.na(pop_sizeUpper) |
      #  is.na(pop_sizeLower) & pop_sizeUpper == 0 |
      #  is.na(pop_sizeLower) & is.na(pop_sizeUpper)))

    
    
    censData_joined <- as.data.frame(censData_joined)
    
    # convex combination/interpolation
    t <- (year - year_lower) / (year_upper - year_lower)
    censDataTo <- censData_joined %>%
      mutate(pop_size = (1 - t) * pop_sizeLower + t * pop_sizeUpper) %>%
      select(state, county, tract, GEO_ID, variable, pop_size)

    fwrite(censDataTo, censDirToX)

  }
})
