#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------


# load packages, install if missing
library(tidyr)
packages <- c("dplyr", "magrittr", "data.table", "testthat", "tictoc", "stats", "matrixStats")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)

suppressMessages({
  pkgload::load_all()
})

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
censDir <- args[2]
dem_agrDir <- args[3]
pop.summary.dir <- args[4]
summaryDir <- args[7]

# TODO delete
#if (rlang::is_empty(args)) {
  tmpDir <- "data/tmp"
  censDir <- "data/05_demog"
  dem_agrDir <- "data/06_dem.agr"
  pop.summary.dir <- "data/12_population_summary"
  summaryDir <- "data/17_summary"
#}

findreplace <- read.csv("data/final_findreplace.csv")

states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  select(NAME, STATEFP)

## --- read pm data ----
pm_summDir <- file.path(summaryDir, "pm_summary.csv")
if (file.exists(pm_summDir)) {
  quit()
}

tic(paste("summarized pm data"))
#agr_bys <- setdiff("STATEFP", "nation")
agr_bys <- list.files(dem_agrDir)
# agr_bys <- "nation" #TODO löschen
pm_summ <- lapply(agr_bys, function(agr_by) {
  tic(paste("summarized pm data by", agr_by))
  #years <- list.files(file.path(dem_agrDir, agr_by))
  years <- 1990:2016
  pm_summ <- lapply(years, function(year) {
    meta <- read.csv(paste0("data/05_demog/meta/cens_meta_", year, ".csv"))
    #meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year)))
    files <- list.files(file.path(dem_agrDir, agr_by, year))

    pm_summ <- lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist(fill = TRUE)

    if (agr_by != "nation") pm_summ <- pm_summ %>% filter(scenario == "real")

    if (nrow(pm_summ) == 0) {
      return(NULL)
    }

    pm_summ <- pm_summ %>% left_join(meta, by = "variable")

    pm_summ_over25 <- pm_summ %>%
      filter(min_age >= 25) %>%
      group_by_at(vars(all_of(setdiff(colnames(pm_summ), c("variable", "pop_size", "prop", "min_age", "max_age"))))) %>%
      summarise(pop_size = sum(pop_size)) %>%
      mutate(min_age = 25, max_age = 150)

    pm_summ_over65 <- pm_summ %>%
      filter(min_age >= 65) %>%
      group_by_at(vars(all_of(setdiff(colnames(pm_summ), c("variable", "pop_size", "prop", "min_age", "max_age"))))) %>%
      summarise(pop_size = sum(pop_size)) %>%
      mutate(min_age = 65, max_age = 150)

    pm_summ <- rbind(pm_summ_over25, pm_summ_over65)

    #pm_summ <- pm_summ %>%
    #  group_by_at(vars(all_of(setdiff(colnames(pm_summ), c("variable", "pop_size", "prop", "pm"))))) %>%
      # group_by(Year,Region, agr_by, Race, Hispanic.Origin,Gender.Code, scenario, Education) %>%
    #  summarise(
    #    mean = weighted.mean(pm, pop_size),
    #    median = matrixStats::weightedMedian(pm, pop_size)
    #  )

    pm_summ <- pm_summ %>%
      group_by_at(vars(all_of(setdiff(colnames(pm_summ), c("variable", "pop_size", "prop", "pm"))))) %>%
      do({
        pm = .$pm
        pop_size = .$pop_size
        result = calculate_weighted_mean_ci(pm, pop_size)
        data.frame(mean = result$pop_weight_pm_exp,
                   mean_lower = result$lower,
                   mean_upper = result$upper)
      }) %>%
      ungroup()


    toc()
    return(pm_summ)

  }) %>% rbindlist(fill = TRUE)
  # make compatible
  pm_summ <- pm_summ %>% rename("Region" := !!agr_by)
  pm_summ <- pm_summ %>% tibble::add_column(agr_by = agr_by)

  return(pm_summ)
}) %>% rbindlist(fill = TRUE)

# pm_summ <- pm_summ %>%
#  group_by_at(vars(all_of(setdiff(colnames(pm_summ),c("variable","pop_size","prop","pm"))))) %>%
# group_by(Year,Region, agr_by, Race, Hispanic.Origin,Gender.Code, scenario, Education) %>%
#  summarise(mean = weighted.mean(pm, pop_size),
#            median = matrixStats::weightedMedian(pm, pop_size))

#pm_summ <- pm_summ %>%
#  tidyr::pivot_longer(c(mean, median),
#    names_to = "pm_metric"
#  )

pm_summ <- pm_summ %>% filter(!is.na(Gender.Code)) # TODO
pm_summ <- pm_summ %>%
  mutate(pm_metric = "mean") %>%
  rename(value = mean)


## --- find and replcase---
pm_summ <- pm_summ %>% mutate_at(
  setdiff(colnames(pm_summ), c("value", "mean_lower", "mean_upper")),
  as.factor
)

rindreplace1 <- data.frame(
  agr_by = c("nation", rep("STATEFP", nrow(states))),
  RegionFrom = c("us", states$STATEFP),
  RegionTo = c("United States", states$NAME)
)
pm_summ <- pm_summ %>%
  left_join(rindreplace1, by = c("Region" = "RegionFrom", "agr_by")) %>%
  mutate(
    Region = coalesce(RegionTo, Region),
    RegionTo = NULL
  )

rindreplace2 <- list(
  "High school graduate or lower" = "lower",
  "Some college education but no 4-year college degree" = "middle",
  "4-year college graduate or higher" = "higher",
  "666" = "666"
)
levels(pm_summ$Education) <- rindreplace2

rindreplace3 <- list("All genders" = "A", "Male" = "M", "Female" = "F")
levels(pm_summ$Gender.Code) <- rindreplace3

pm_summ <- pm_summ %>%
  unite("Ethnicity", Race, Hispanic.Origin, sep = ", ") %>%
  mutate(Ethnicity = as.factor(Ethnicity))
rindreplace7 <- list(
  "Black American" = "Black or African American, All Origins",
  "American Indian or Alaska Native" = "American Indian or Alaska Native, All Origins",
  "Asian or Pacific Islander" = "Asian or Pacific Islander, All Origins",
  "Hispanic or Latino White" = "White, Hispanic or Latino",
  "NH White" = "White, Not Hispanic or Latino",
  "White" = "White, All Origins",
  "All, All Origins" = "All, All Origins"
)
levels(pm_summ$Ethnicity) <- rindreplace7

# rindreplace8 <- list("large central metro" = 1, "large fringe metro" = 2, "medium metro" = 3, "small metro" = 4, "micropolitan" = 5, "non-core" = 6,"All" = 666,"Unknown" = "Unknown")
rindreplace8 <- list("Large metro" = 1, "Small-medium metro" = 2, "Non metro" = 3, "All" = 666, "Unknown" = "Unknown")
levels(pm_summ$rural_urban_class) <- rindreplace8

cat("written pm_summ to", pm_summDir, "\n")

cat("total_burden findreplace-start")
tic("total_burden findreplace")
pm_summ <- pm_summ %>% replace_values(findreplace)
toc()

fwrite(pm_summ, pm_summDir)
rm(rindreplace1, rindreplace2, rindreplace3, rindreplace7, rindreplace8)
toc()
