#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 15/02/2021
# Purpose: assign PM exposure to each census tract in Hawaii and Alaska
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
# rm(list = ls(all = TRUE))

# load packages, install if missing

packages <- c("data.table", "plyr", "magrittr", "testthat", "tigris", "sf",  "sp", "tictoc", "units")

options(tidyverse.quiet = TRUE)
options(tigris.quiet = TRUE)
options(tigris_use_cache = FALSE)
for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}
library(dplyr)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)
if (rlang::is_empty(args)) {
  agr_by <- "county"
  year <- 2016
  tmpDir <- "data/tmp"
  expDir <- "data/01_exposure"
  tracDir <- "data/02_tracts"
  exp_tracDir <- "data/03_exp_tracts"

} else {
  year <- args[1]
  tmpDir <- args[3]
  expDir <- args[4]
  tracDir <- args[5]
  exp_tracDir <- args[7]
}

expDir <- file.path(expDir, "epa")
dir.create(expDir, recursive = T, showWarnings = F)

tracts_locationsDir <- file.path(exp_tracDir, "epa_tmp")
dir.create(tracts_locationsDir, recursive = T, showWarnings = F)

exp_tracDir <- file.path(exp_tracDir, toString(year))
dir.create(exp_tracDir, recursive = T, showWarnings = F)

exposure_locationsDir <- file.path(tmpDir, "epa_locations.csv")
## ---------------load data---------------
# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  dplyr::filter(STUSPS %in% c("AK", "HI"))


##### ------------assign measurement location to tracts--------
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  tracts_locationsDirX <- file.path(tracts_locationsDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv"))
  # quit execution, if already calculated
  if (file.exists(tracts_locationsDirX)) {
    return()
  }

  tic(paste("assigned measurement location to all tracts in", name, "in", toString(year)))
  # read exposure
  exposure_locations <- file.path(expDir, paste0("annual_conc_by_monitor_", toString(year), ".csv")) %>%
    read.csv()

  if(year %in% 1990:1996){
    exposure_locations <- exposure_locations %>%
      filter(
        grepl('PM2.5',Parameter.Name) &
        #Parameter.Name %in% c("OC PM2.5 LC TOR" , "OC3 PM2.5 LC", "EC1 PM2.5 LC", "EC PM2.5 LC TOR", "OC2 PM2.5 LC", "Acceptable PM2.5 AQI & Speciation Mass")
          State.Code %in% c("02","15") #&
         #   Parameter.Name == "Acceptable PM2.5 AQI & Speciation Mass"
         )
  }else if(year >= 1997){
    exposure_locations <- exposure_locations %>%
      filter(
        Parameter.Name == "PM2.5 - Local Conditions",
        Pollutant.Standard == "PM25 Annual 2012",
        Units.of.Measure == "Micrograms/cubic meter (LC)",
        Event.Type %in% c("No Events", "Events Excluded")
      )
  }

  if(nrow(exposure_locations) == 0){
    print(paste("no exposure data for PM2.5 in Alaska and Hawaii for year",year,"available"))
    quit()
  }

  # load shape files
  tracts <- file.path(tracDir, toString(year), paste0("tracts_", toString(year), "_", STUSPS, ".rds")) %>%
    readRDS()

  exposure_locations <- st_as_sf(exposure_locations,
    coords = c("Longitude", "Latitude"),
    crs = ifelse(is.na(st_crs(tracts)$input),
      4326,
      st_crs(tracts)$input
    ),
    agr = "constant"
  )

  tracts_locations <- list()
  for(i in 1:nrow(tracts)){
    row <- tracts[i,]
    geometry <- row$geometry[[1]]
    # subset points, which are inside of the tract
    suppressMessages(points_in_tract <- exposure_locations[geometry, , op = st_within])

    # if there are points inside of the tract, the tract is assigned the mean of pm of those points
    # if there are none, the pm of the closest point
    if (nrow(points_in_tract) > 0) {
      df <- data.frame(
        GEO_ID = row$GEO_ID,
        Location.State.Code = points_in_tract$State.Code,
        Location.County.Code = points_in_tract$County.Code,
        Location.Site.Num = points_in_tract$Site.Num,
        Location.POC = points_in_tract$POC,
        distance = 0
      )
      tracts_locations[[i]] <- df
    } else {
      tract_centroid <- geometry %>% st_centroid()
      tract_centroid <- data.frame(
        longitude = tract_centroid[1],
        latitude = tract_centroid[2]
      )

      tract_centroid <- tract_centroid %>%
        st_as_sf(
          coords = c("longitude", "latitude"),
          crs = st_crs(exposure_locations),
          agr = "constant"
        )

      # plot(geometry)
      # plot(tract_centroid, add = TRUE, pch = 3, col = 'red')
      exposure_locations$dist <- st_distance(x = exposure_locations, y = tract_centroid) %>%
        set_units(1, "km")

      closest_measurement <- exposure_locations[which.min(exposure_locations$dist), ]

      df <- data.frame(
        GEO_ID = row$GEO_ID,
        Location.State.Code = closest_measurement$State.Code,
        Location.County.Code = closest_measurement$County.Code,
        Location.Site.Num = closest_measurement$Site.Num,
        Location.POC = closest_measurement$POC,
        distance = closest_measurement$dist
      )
      exposure_locations$dist <- NULL
      tracts_locations[[i]] <- df
    }
  }

  tracts_locations <- do.call(rbind, tracts_locations)

  # save as csv
  fwrite(tracts_locations, tracts_locationsDirX, row.names = FALSE)
  toc()
})
##### ------------assign annual pm to tracts--------
apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  exp_tracDirX <- file.path(exp_tracDir, paste0("exp_trac_", toString(year), "_", STUSPS, ".csv"))

  # quit execution, if already calculated
   if (file.exists(exp_tracDirX)) {
    return()
   }

  tic(paste("assigned PM exposure to all tracts in", name, "in", toString(year)))
  tracts_locations <- file.path(tracts_locationsDir, paste0("trac_loc_", toString(year), "_", STUSPS, ".csv")) %>%
    read.csv() %>%
    mutate(Location.State.Code = Location.State.Code %>% as.character() %>% str_pad(width = 2, pad = "0"))

  exposureDir <- file.path(expDir, paste0("annual_conc_by_monitor_", toString(year), ".csv"))

  if(!file.exists(exposureDir)) return()

  exposure<-  read.csv(exposureDir) %>%
    mutate(State.Code = State.Code %>% as.character() %>% str_pad(width = 2, pad = "0"))

  if(year %in% 1990:1996){
    exposure <- exposure %>% filter(Parameter.Name == "Acceptable PM2.5 AQI & Speciation Mass")
  }else if(year >= 1997){
    exposure <- exposure %>%
      filter(
        Parameter.Name == "PM2.5 - Local Conditions",
        Pollutant.Standard == "PM25 Annual 2012",
        Units.of.Measure == "Micrograms/cubic meter (LC)",
        Event.Type %in% c("No Events", "Events Excluded")
      )
  }

  tract_exposure <- left_join(tracts_locations, exposure,
    by = c(
      "Location.State.Code" = "State.Code",
      "Location.County.Code" = "County.Code",
      "Location.Site.Num" = "Site.Num",
      "Location.POC" = "POC"
    )
  )

  tract_exposure <- tract_exposure %>%
    group_by(GEO_ID) %>%
    summarise(pm = mean(Arithmetic.Mean))

  ## -----save as csv--------
  write.csv(tract_exposure, exp_tracDirX, row.names = FALSE)
  toc()
})
