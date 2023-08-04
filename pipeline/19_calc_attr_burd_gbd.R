#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: calculate attributable burden with GBD CRF
#
#***************************************************************************
#*


# load packages, install if missing
suppressMessages(library("dplyr", character.only = T, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library("magrittr", character.only = T, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library("data.table", character.only = T, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library("testthat", character.only = T, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library("tidyverse", character.only = T, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library("tictoc", character.only = T, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library("MALDIquant", character.only = T, warn.conflicts = FALSE, quietly = TRUE))

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
exp_rrDir <- args[6]
censDir <- args[8]
dem_agrDir <- args[9]
agr_by <- args[10]
source <- args[14]
totalBurdenParsed2Dir <- args[17]
attr_burdenDir <- args[18]

if (rlang::is_empty(args)) {
  year <- 2012
  agr_by <- "county"
  source <- "nvss"

  tmpDir <- "data/tmp"
  exp_rrDir <- "data/04_exp_rr"
  censDir <- "data/05_demog"
  dem_agrDir <- "data/06_dem.agr"
  pafDir <- "data/07_gbd_paf"

  totalBurdenParsed2Dir <- "data/13_total_burden_rate"
  attr_burdenDir <- "data/14_attr_burd"
}

attr_burdenDir <- file.path(attr_burdenDir, agr_by, source)
dir.create(attr_burdenDir, recursive = T, showWarnings = F)
attr_burdenDir <- file.path(attr_burdenDir, paste0("attr_burd_gbd_", toString(year), ".csv"))

if (agr_by != "county") {
  quit()
}

if (file.exists(attr_burdenDir)) {
  quit()
}
tic(paste("calculated burden with GBD", year, agr_by, source))
# read some data
total_burden <- file.path(totalBurdenParsed2Dir, agr_by, source, paste0("total_burden_", year, ".csv")) %>%
  fread() %>%
  filter(label_cause %in% c("cvd_ihd", "cvd_stroke", "neo_lung", "resp_copd", "lri", "t2_dm"))

total_burden <- total_burden %>%
  filter(county != "Unknown") %>%
  mutate(county = as.integer(county))

#to save time, delete if necessary
total_burden <- total_burden %>% filter(Education == "666")
## ----determine join variables
join_variables <- c("Year", "Race", "Hispanic.Origin", "Education", "rural_urban_class", "Gender.Code", "label_cause", "min_age", "max_age", agr_by)
group_variables <- c("Year", "Race", "Hispanic.Origin", "Education", "rural_urban_class", "Gender.Code", agr_by)

## ---read and summarise pm data ----
files <- list.files(file.path(dem_agrDir, agr_by, year))
pm_summ <- lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist()
pm_summ <- pm_summ %>% filter(scenario == "real")

meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
pm_summ <- pm_summ %>% left_join(meta, by = "variable")

pm_summ <- pm_summ %>%
  filter(min_age >= 25) %>%
  group_by_at(vars(all_of(setdiff(colnames(pm_summ), c("variable", "pop_size", "prop", "min_age", "max_age"))))) %>%
  summarise(pop_size = sum(pop_size)) %>%
  ungroup()

pm_summ <- pm_summ %>%
  group_by_at(vars(all_of(setdiff(colnames(pm_summ), c("variable", "pop_size", "prop", "pm"))))) %>%
  summarise(pm_mean = weighted.mean(pm, pop_size)) %>%
  ungroup()

## --- match pm levels ----
example_exp_rr <- file.path(exp_rrDir, "cvd_ihd_25.csv") %>% read.csv()
pm_levels <- example_exp_rr$exposure_spline

pm_summ <- pm_summ %>%
  mutate(matched_pm_level = sapply(pm_mean, function(x) {
    pm_levels[
      MALDIquant::match.closest(x, pm_levels)
    ]
  })) %>%
  select(-c("state", "rural_urban_class", "pm_mean"))

## ---join with total burden-----
attr_burden_gbd <- inner_join(
  total_burden,
  pm_summ,
  by = c("Year", "Gender.Code", "Race", "Hispanic.Origin", "county", "Education")
)
## ---- add age group to toal burden ----
causes_ages <- file.path(tmpDir, "causes_ages.csv") %>% read.csv()
attr_burden_gbd <- attr_burden_gbd %>%
  mutate(
    age_group_id = c(0, seq(25, 95, 5))[
      findInterval(
        max_age,
        c(0, seq(25, 95, 5)),
        left.open =  F
      )
    ],
    age_group_id = as.character(age_group_id)
  )

causes_for_all_ages <- causes_ages$label_cause[causes_ages$age_group_id == "all ages"]

attr_burden_gbd$age_group_id[attr_burden_gbd$label_cause %in% causes_for_all_ages] <-"all ages"
## ----load rr---
exp_rr <- apply(causes_ages, 1, function(cause_age) {
  label_cause_i <- cause_age[["label_cause"]]
  age_group_id_i <- cause_age[["age_group_id"]]

  # load rr file
  exp_rr <- ifelse(age_group_id_i == "all ages",
    paste0(label_cause_i, ".csv"),
    paste0(label_cause_i, "_", age_group_id_i, ".csv")
  ) %>%
    file.path(exp_rrDir, .) %>%
    fread()

  exp_rr <- exp_rr %>%
    mutate(
      label_cause = label_cause_i,
      age_group_id = age_group_id_i
    )
  exp_rr
})

exp_rr <- exp_rr %>% rbindlist()
exp_rr <- exp_rr %>%
  pivot_longer(
    cols = starts_with("draw_"),
    names_to = "draw",
    values_to = "rr"
  )

exp_paf <- exp_rr %>%
  mutate(
    paf = (rr - 1) / rr,
    rr = NULL
  )

#computationally too expensive to take all draws
exp_paf <- exp_paf %>%
  group_by(exposure_spline, label_cause, age_group_id)%>%
  slice(10) %>%
  ungroup()
rm(exp_rr)

##---join speratly, calculate seperatly---
#county

group_variable <- setdiff(colnames(attr_burden_gbd), c("matched_pm_level","attr", "label_cause", "age_group_id", "value", "paf"))

#attr_burden_gbd <- attr_burden_gbd %>%
#  split(list(attr_burden_gbd$Race, attr_burden_gbd$min_age, attr_burden_gbd$max_age, 
#             attr_burden_gbd$Hispanic.Origin, attr_burden_gbd$Education, attr_burden_gbd$measure1,
#             attr_burden_gbd$measure2), drop=TRUE)

attr_burden_gbd <- attr_burden_gbd %>%
  split(list(attr_burden_gbd$county), drop=TRUE)
#, 
print("expensive calculation in GBD")
attr_burden_gbd <- lapply(attr_burden_gbd, function(attr_burden_gbd_i){
  #nrow(attr_burden_gbd %>%
  #anti_join(exp_paf, by =c("matched_pm_level" = "exposure_spline", "label_cause"   ,  "age_group_id"))
  #)
  attr_burden_gbd_i <- attr_burden_gbd_i %>%
    left_join(exp_paf, by =c("matched_pm_level" = "exposure_spline", "label_cause",  "age_group_id")) %>%
    mutate(attr = paf*value)
  
  attr_burden_gbd_i <- attr_burden_gbd_i %>%
    dplyr::group_by_at(vars(one_of(group_variable))) %>%
    dplyr::summarise(
      mean = mean(attr),
      lower = quantile(attr, p = .025),
      upper = quantile(attr, p = .975)
    )%>%
    dplyr::ungroup()

  attr_burden_gbd_i
})
print("done with expensive calculation in GBD")

attr_burden_gbd <- attr_burden_gbd %>% rbindlist()
###----
attr_burden_gbd <- attr_burden_gbd %>% 
  mutate(method = "GBD",
         attr = "attributable")


fwrite(attr_burden_gbd, attr_burdenDir)

toc()