#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 09/17/2021
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "data.table", "testthat", "tidyverse", "tictoc"
)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
censDir <- args[8]
dem_agrDir <- args[9]
agr_by <- args[10]
source <- args[14]
totalBurdenParsed2Dir <- args[17]
attr_burdenDir <- args[18]

# TODO delete
if (rlang::is_empty(args)) {
  year <- 2016
  agr_by <- "nation"
  source <- "nvss"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  dem_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  attr_burdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"

  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  dem_agrDir <- "C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr"
  totalBurdenParsed2Dir <- "C:/Users/Daniel/Desktop/paper2021/data/12_total_burden_parsed2"
  attr_burdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/13_attr_burd"

  tmpDir <- "/Volumes/fridljand/R/HIGH/data/tmp"
  censDir <- "/Volumes/fridljand/R/HIGH/data/05_demog"
  dem_agrDir <- "/Volumes/fridljand/R/HIGH/data/06_dem.agr"
  totalBurdenParsed2Dir <- "/Volumes/fridljand/R/HIGH/data/12_total_burden_parsed2"
  attr_burdenDir <- "/Volumes/fridljand/R/HIGH/data/13_attr_burd"

  tmpDir <- "data/tmp"
  censDir <- "data/05_demog"
  dem_agrDir <- "data/06_dem.agr"
  totalBurdenParsed2Dir <- "data/13_total_burden_rate"
  attr_burdenDir <- "data/14_attr_burd"
}

attr_burdenDir <- file.path(attr_burdenDir, agr_by, source)
dir.create(attr_burdenDir, recursive = T, showWarnings = F)
attr_burdenDir <- file.path(attr_burdenDir, paste0("attr_burd3_", toString(year), ".csv"))

if (!file.exists(attr_burdenDir)) {
  tic(paste("calculated 3rd burden alternative way", year, agr_by, source))
  #----read some data-----
  total_burden <- file.path(totalBurdenParsed2Dir, agr_by, source, paste0("total_burden_", year, ".csv")) %>%
    fread()

  meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
  files <- list.files(file.path(dem_agrDir, agr_by, year))
  pm_summ <- lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist()
  pm_summ <- pm_summ %>% left_join(meta, by = "variable")
  pm_summ <- pm_summ %>% filter(min_age >= 25)
  if(agr_by != "nation") pm_summ <- pm_summ %>% filter(scenario == "real")
  # pm_summ <- pm_summ %>% mutate(min_age = min(min_age), max_age = max(max_age))

  pm_summ <- pm_summ %>% mutate_at(c("rural_urban_class", "Education"), as.factor)
  total_burden <- total_burden %>% mutate_at(c("rural_urban_class", "Education"), as.factor)
  total_burden <- total_burden %>% filter(label_cause == "all-cause")

  #restrict everything to age_group 25+
  total_burden <- total_burden %>%
    filter(min_age >= 25)
  
  pm_summ <- pm_summ %>%
    dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education", "rural_urban_class", "scenario", "pm", "min_age", "max_age"))) %>%
    dplyr::summarize(pop_size = sum(pop_size))

  rm(meta, files)
  test_that("basic check pm summ", {
    pm_summ_dupl <- pm_summ %>% select(setdiff(colnames(pm_summ), c("pop_weight_pm_exp")))
    pm_summ_dupl <- pm_summ_dupl[duplicated(pm_summ_dupl), ]
    expect_equal(nrow(pm_summ_dupl), 0)
  })
  ## ---paf calculations----

  # DI 2017, SI, Table S3
  # hazard ratio
  # https://www.nejm.org/doi/suppl/10.1056/NEJMoa1702747/suppl_file/nejmoa1702747_appendix.pdf

  # Increases of 10 ??g per cubic meter in PM2.5 were associated with increases in all-cause mortality

  hr_race_specific <- tibble::tribble( 
    ~method, ~Race, ~Hispanic.Origin, ~label_cause,~hr_mean, ~hr_lower, ~hr_upper, ~min_age,
    "di_gee", "White", "All Origins", "all-cause",1.063,1.06, 1.065, 25,
    "di_gee", "White", "Not Hispanic or Latino", "all-cause",1.063, 1.06, 1.065, 25,
    "di_gee", "White", "Hispanic or Latino", "all-cause",1.116, 1.1, 1.133, 25,
    
    "di_gee", "Black or African American", "All Origins", "all-cause",1.208, 1.199, 1.217, 25,
    "di_gee", "Asian or Pacific Islander", "All Origins", "all-cause",1.096, 1.075,  1.117, 25,
    "di_gee", "American Indian or Alaska Native", "All Origins", "all-cause",1.1, 1.06, 1.14, 25,
    "di_gee", "All", "All Origins", "all-cause",1.073, 1.071, 1.075, 25
  )
  hr_race_specific <- as.data.frame(hr_race_specific)

  hr_race_uniform <- data.frame(
    method = "di_gee_white",
    Race = c("White", "White", "Black or African American", "Asian or Pacific Islander", "White", "American Indian or Alaska Native", "All"), # TODO
    Hispanic.Origin = c("All Origins", "Not Hispanic or Latino", "All Origins", "All Origins", "Hispanic or Latino", "All Origins", "All Origins"), # TODO
    label_cause = "all-cause",
    hr_mean = 1.063,
    hr_lower = 1.06,
    hr_upper = 1.065,
    min_age = 25
  )

  hr <- rbind(hr_race_specific, hr_race_uniform)
  rm(hr_race_specific, hr_race_uniform)

  paf_di <- inner_join(pm_summ, hr, by = c("Race", "Hispanic.Origin"))
  paf_di <- paf_di %>%
    filter(min_age.x >= min_age.y) %>%
    mutate(
      min_age = min_age.y,
      max_age = 150,
      min_age.x = NULL, min_age.y = NULL
    )

  paf_di <- paf_di %>%
    dplyr::group_by_at(vars(one_of(setdiff(colnames(paf_di), c("pm", "pop_size"))))) %>%
    summarise(pop_weight_pm_exp = weighted.mean(pm, pop_size)) %>%
    ungroup()


  rm(pm_summ, hr)
  paf_di <- paf_di %>%
    mutate(
      # PAF = 1-1/HR #TODO
      paf_mean = case_when(
        pop_weight_pm_exp < 5 ~ 0,
        pop_weight_pm_exp >= 5 ~ (pop_weight_pm_exp - 5) * (hr_mean - 1) / 10
      ),
      paf_lower = case_when(
        pop_weight_pm_exp < 5 ~ 0,
        pop_weight_pm_exp >= 5 ~ (pop_weight_pm_exp - 5) * (hr_lower - 1) / 10
      ),
      paf_upper = case_when(
        pop_weight_pm_exp < 5 ~ 0,
        pop_weight_pm_exp >= 5 ~ (pop_weight_pm_exp - 5) * (hr_upper - 1) / 10
      ),
      pop_weight_pm_exp = NULL, hr_upper = NULL, hr_mean = NULL, hr_lower = NULL
    )

  if (agr_by == "county") paf_di$rural_urban_class <- as.factor(666) # TODO change total burden

 
  attr_burden_di <- inner_join(
    total_burden,
    paf_di,
    by = c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education", "rural_urban_class", "label_cause")
  )

  attr_burden_di <- attr_burden_di %>%
    filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
    mutate(
      min_age = min_age.y, max_age = max_age.y,
      #min_age = min_age.x, max_age = min_age.x, for age specific
      min_age.y = NULL, min_age.x = NULL, max_age.x = NULL, max_age.y = NULL
    )

  attr_burden_di <- attr_burden_di %>%
    dplyr::group_by_at(vars(one_of(setdiff(colnames(attr_burden_di), "value")))) %>%
    summarise(value = sum(value)) %>%
    ungroup()


  attr_burden_di <- attr_burden_di %>%
    mutate(
      mean = value * paf_mean,
      lower = value * paf_lower,
      upper = value * paf_upper,
      paf_mean = NULL, paf_lower = NULL, paf_upper = NULL,
      value = NULL, label_cause = NULL,
      attr = "attributable",
      # min_age = min(min_age.x, min_age.y),
      # max_age = max(max_age.x, max_age.y),
      # min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL
    )

  
  attr_burden_di <- attr_burden_di %>% filter(method %in% c("di_gee", "di_coxme", "di_gee_white"))
  fwrite(attr_burden_di, attr_burdenDir)
  toc()
}
