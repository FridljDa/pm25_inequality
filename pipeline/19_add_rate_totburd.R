#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 03/27/2021
# Purpose: add rate total burden
#
#***************************************************************************
#*

# clear memory
# rm(list = ls(all = TRUE))

# load packages, install if missing
suppressMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  library(tidyverse)
  library(tictoc)
})


devtools::load_all()

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  year <- 2002

  tmpDir <- "data/tmp"
  agr_by <- "county"
  totalBurdenParsedDir <- "data/09_total_burden_parsed"
  source <- "nvss"
  pop.summary.dir <- "data/12_population_summary"
  totalBurdenParsed2Dir <- "data/13_total_burden_rate"
} else {
  year <- args[1]
  tmpDir <- args[3]
  agr_by <- args[10]
  # pafDir <- args[11]
  totalBurdenParsedDir <- args[13]
  source <- args[14]
  pop.summary.dir <- args[16]
  totalBurdenParsed2Dir <- args[17]
}

totalBurdenParsed2Dir <- file.path(totalBurdenParsed2Dir, agr_by, source)
dir.create(totalBurdenParsed2Dir, recursive = T, showWarnings = F)
totalBurdenParsed2Dir <- file.path(totalBurdenParsed2Dir, paste0("total_burden_", year, ".csv"))

if (file.exists(totalBurdenParsed2Dir)) {
  quit()
}

## ---read total burden parsed data----
total_burden <- file.path(totalBurdenParsedDir, agr_by, "nvss", paste0("total_burden_nvss_", year, ".csv")) %>%
  read_data()

# }
total_burden <- total_burden %>%
  filter(rural_urban_class != "Unknown" & svi_bin != "Unknown") %>%
  mutate_at(c("rural_urban_class", "Education"), as.factor)

if (agr_by == "nation") {
  total_burden <- total_burden %>%
    complete(Year, nation, source,
             nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education),
             rural_urban_class, nesting(label_cause, attr),
      fill = list(Deaths = 0)
    ) %>%
    mutate_at(c("nation"), as.factor)
} else if (agr_by == "STATEFP") {
  total_burden <- total_burden %>%
    # complete(Year, STATEFP, source, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class, nesting(label_cause, attr),
    #  fill = list(Deaths = 0)
    # ) %>%
    filter(STATEFP != "oth") %>%
    mutate_at(c("STATEFP"), as.factor)
} else if (agr_by == "county") {
  total_burden <- total_burden %>%
    filter(county != "oth") %>%
    mutate_at(c("county"), as.factor)

  # total_burden <- total_burden %>% filter(label_cause == "all-cause" & rural_urban_class == "666")
}

## --- measure 1: Deaths and YLL-----
tic(paste("added YLL and age-adjusted rate to total burden in", year, agr_by))
# Deaths
total_burden$measure1 <- "Deaths"
total_burden <- total_burden %>% dplyr::rename(value = Deaths)

#------measure 2: absolute number, crude rate and age-standartised rates-----
# absolute number
total_burden$measure2 <- "absolute number"

#---read population data----
pop_summary <- get_population_data(agr_by, year)
# age-standartised rates
total_burden_age_adj <- add_age_adjusted_rate(total_burden, pop_summary)

total_burden <- rbind(total_burden, total_burden_crude, total_burden_age_adj)

## ----finish------
if (agr_by != "nation") {
  total_burden <- total_burden %>%
    filter(measure1 == "Deaths" & measure2 %in% c("absolute number", "age-adjusted rate"))
}

# restrict everything to age_group 25+
total_burden <- total_burden %>%
  filter(min_age >= 25)

total_burden <- total_burden %>% distinct()

fwrite(total_burden, totalBurdenParsed2Dir)
toc()
