
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: assign PM exposure to each census tract
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
#rm(list = ls(all = TRUE))

# load packages, install if missing
library(dplyr)
library(magrittr)
library(sf)
library(sp)
library(tictoc)
library(rhdf5)

options(tidyverse.quiet = TRUE)

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

## ---------------load data---------------

# load exposure data
filenameExp <- paste0(toString(year), ".h5")
filepathExp <- file.path(expDir, filenameExp)
hdf_file <- H5Fopen(filepathExp)
exp_data <- as.matrix(hdf_file$CorrectedPM2.5)
long_vec <- c(as.matrix(hdf_file$longitude))
lat_vec <- c(as.matrix(hdf_file$latitude))

# load some useful estimates to optimize code
filenameM <- paste0("m_exp_", toString(year), ".RData")
filepathM <- file.path(tmpDir, filenameM)
load(filepathM)

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>%
                read.csv %>%
                filter(!(STUSPS %in% c("AK", "HI")))

# create folder, where calculations will be stored
exp_tracDir <- file.path(exp_tracDir, toString(year))
dir.create(exp_tracDir, recursive = T, showWarnings = F)


## -----------------calculation---------------
tic(paste("Assigned pm exposure to each tract for year", toString(year), "for all states"))
apply(states, 1, function(state) {
  #state <- states[1,]
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  exp_tracDirX <- paste0("exp_trac_", toString(year), "_", STUSPS, ".csv") %>%
    file.path(exp_tracDir, .)

  # quit execution, if already calculated
  if (file.exists(exp_tracDirX)) {
    return()
  }

  # load shape files
  tracts <- paste0("tracts_", toString(year), "_", STUSPS, ".rds") %>%
    file.path(tracDir, toString(year), .) %>%
    readRDS(.)


  tracts <- tracts %>% filter(sapply(tracts$geometry, length) > 0)


  tic(paste("Assigned pm exposure to each tract for year", toString(year), "in", name))
  library(purrr)

  pm_df <- map_dfr(tracts$geometry, function(geometry) {
    if(length(geometry) == 0) return(NA)
    # get enclosing box, make sure in range of exposure data
    bbox <- st_bbox(geometry)
    long_min <- bbox$xmin %>%
      max(., long_vec[1])
    lat_min <- bbox$ymin %>%
      max(., lat_vec[1])
    long_max <- bbox$xmax %>%
      min(., long_vec[length(long_vec)])
    lat_max <- bbox$ymax %>%
      min(., lat_vec[length(lat_vec)])

    # estimate corresponding grid in pm exposure data
    long_row_min <- -1 + ((long_min - long_vec[1]) / m_max_long) %>%
      floor()
    lat_row_min <- -1 + ((lat_min - lat_vec[1]) / m_max_lat) %>%
      floor()
    long_row_max <- 1 + ((long_max - long_vec[1]) / m_min_long) %>%
      ceiling()
    lat_row_max <- 1 + ((lat_max - lat_vec[1]) / m_min_lat) %>%
      ceiling()

    if(is.na(long_row_min)) browser()
    if(is.na(long_row_max)) browser()
    long_subset <- long_vec[long_row_min:long_row_max]
    lat_subset <- lat_vec[lat_row_min:lat_row_max]
    pm_subset <- exp_data[long_row_min:long_row_max, lat_row_min:lat_row_max]

    # estimates, where to look for the pm data for this particular tract to improve run time
    points_subset <- data.frame(
      lat = rep(lat_subset, times = length(long_subset)),
      lng = rep(long_subset, each = length(lat_subset)),
      pm = as.vector(t(pm_subset))
    ) %>%
      st_as_sf(.,
               coords = c("lng", "lat"),
               crs = 4326,
               agr = "constant"
      )

    # subset points, which are inside of the tract
    # if there are points inside of the tract, the tract is assigned the mean of pm of those points
    # if there are none, the pm of the closest point


    tract_centroid <- geometry %>% st_centroid()
    tract_centroid <- data.frame(
      longitude = tract_centroid[1],
      latitude = tract_centroid[2]
    ) %>%
      st_as_sf(
        coords = c("longitude", "latitude"),
        crs = st_crs(points_subset),
        agr = "constant"
      )

    # Your existing code to get the top 10 pm values
    top_10_pm <- points_subset %>%
      mutate(distance = as.vector(st_distance(., y = tract_centroid))) %>%
      arrange(distance) %>%
      slice_head(n = 50) %>%
      pull(pm)

    top_10_pm <- top_10_pm * 0.01
    #top_10_pm <- top_10_pm %>%
    #  prod(0.01) %>%
    #  round(digits = 2)

    # Closest pm value (the first in the sorted list)
    pm_value <- top_10_pm[1]

    # Calculate the mean of the top 10 pm values
    mean_val <- mean(top_10_pm)

    # Calculate the standard deviation of the top 10 pm values
    sd_val <- sd(top_10_pm)

    # Calculate the sample size (n)
    n <- length(top_10_pm)

    # Calculate the standard error of the mean
    se_val <- sd_val / sqrt(n)

    # Calculate the Z-value for a 95% confidence interval
    z_value <- 1.96

    # Calculate the margin of error
    margin_of_error <- z_value * se_val

    # Calculate the lower and upper bounds of the 95% confidence interval
    pm_lower_value <- mean_val - margin_of_error
    pm_upper_value <- mean_val + margin_of_error

    pm_lower_value <- min(pm_lower_value, pm_value)
    pm_upper_value <- max(pm_upper_value, pm_value)
    # Return the closest pm value and the 95% CI bounds
    return(list(pm = pm_value, pm_lower = pm_lower_value, pm_upper = pm_upper_value))

  })

  tracts <- bind_cols(tracts, pm_df)

  # estimate pm exposure for each tract
  toc()

  ## --------------plot-----------
  # save everything as interactive map via tmap
  if (FALSE) {
    tm <- tm_shape(tracts) +
      tm_polygons("pm", alpha = 0.6)

    filepathExpTrac_plot <- paste0("exp_trac_", toString(year), "_", STUSPS, ".html") %>% # png/html möglich
      file.path(exp_tracDir, .)

    tmap_save(tm, filename = filepathExpTrac_plot)
  }

  ## -----save as csv--------

  tracts <- tracts %>%
    as.data.frame() %>%
    dplyr::select(c("GEO_ID", "pm"))


  write.csv(tracts, exp_tracDirX, row.names = FALSE)
})
toc()

