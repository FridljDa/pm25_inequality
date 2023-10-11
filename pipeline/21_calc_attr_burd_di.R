#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 09/17/2021
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# load packages, install if missing
suppressMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  #
  library(tictoc)
})

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
suppressMessages({pkgload::load_all()})

# Pass in arguments
args <- commandArgs(trailingOnly = T)


# TODO delete
if (rlang::is_empty(args)) {
  year <- 2016
  agr_by <- "county"
  source <- "nvss"

  tmpDir <- "data/tmp"
  censDir <- "data/05_demog"
  dem_agrDir <- "data/06_dem.agr"
  totalBurdenParsed2Dir <- "data/13_total_burden_rate"
  attr_burdenDir <- "data/14_attr_burd"
} else {
  year <- args[1]
  tmpDir <- args[3]
  censDir <- args[8]
  dem_agrDir <- args[9]
  agr_by <- args[10]
  source <- args[14]

  totalBurdenParsed2Dir <- args[17]
  attr_burdenDir <- args[18]
}

if (agr_by != "county") {
  quit()
}

attr_burdenDir <- file.path(attr_burdenDir, agr_by, source)
dir.create(attr_burdenDir, recursive = T, showWarnings = F)
attr_burdenDir <- file.path(attr_burdenDir, paste0("attr_burd_di_", toString(year), ".csv"))
if (file.exists(attr_burdenDir)) {
  quit() #TODO
}


tic(paste("calculated attr burden with di et al", year, agr_by, source))
#----read some data-----
total_burden <- paste0("data/09_total_burden_parsed/county/nvss/total_burden_nvss_", year, ".csv") %>%
  read_data()
#total_burden <- file.path(totalBurdenParsed2Dir, agr_by, source, paste0("total_burden_", year, ".csv")) %>%
#  read_data()

if("Deaths" %in% colnames(total_burden)) total_burden <- total_burden %>% rename(value = Deaths)

meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", year, ".csv")))
files <- list.files(file.path(dem_agrDir, agr_by, year))
pm_summ <- lapply(files, function(file) fread(file.path(dem_agrDir, agr_by, year, file))) %>% rbindlist()
#browser()

pm_summ <- pm_summ %>% left_join(meta, by = "variable")
pm_summ <- pm_summ %>% filter(min_age >= 25)
# if(agr_by != "nation") pm_summ <- pm_summ %>% filter(scenario == "real")
# pm_summ <- pm_summ %>% mutate(min_age = min(min_age), max_age = max(max_age))


pm_summ <- pm_summ %>% mutate_at(c("rural_urban_class", "Education"), as.factor)
total_burden <- total_burden %>% mutate_at(c("rural_urban_class", "Education"), as.factor)
total_burden <- total_burden %>% filter(label_cause == "all-cause")

# restrict everything to age_group 25+
total_burden <- total_burden %>%
  filter(min_age >= 25)

pm_summ <- pm_summ %>%
  dplyr::group_by_at(vars(one_of("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education",
                                 "rural_urban_class", "scenario", "pm", "pm_lower", "pm_upper", "min_age", "max_age"))) %>%
  dplyr::summarize(pop_size = sum(pop_size))

# rm(meta, files)

## ---paf calculations----

# DI 2017, SI, Table S3
# hazard ratio
# https://www.nejm.org/doi/suppl/10.1056/NEJMoa1702747/suppl_file/nejmoa1702747_appendix.pdf

# Increases of 10 μg per cubic meter in PM2.5 were associated with increases in all-cause mortality

hr_race_specific <- tibble::tribble(
  ~method, ~Race, ~Hispanic.Origin, ~label_cause, ~hr_mean, ~hr_lower, ~hr_upper, ~min_age,
  "di_gee", "White", "All Origins", "all-cause", 1.063, 1.06, 1.065, 25,
  "di_gee", "White", "Not Hispanic or Latino", "all-cause", 1.063, 1.06, 1.065, 25,
  "di_gee", "White", "Hispanic or Latino", "all-cause", 1.116, 1.1, 1.133, 25,
  "di_gee", "Black or African American", "All Origins", "all-cause", 1.208, 1.199, 1.217, 25,
  "di_gee", "Asian or Pacific Islander", "All Origins", "all-cause", 1.096, 1.075, 1.117, 25,
  "di_gee", "American Indian or Alaska Native", "All Origins", "all-cause", 1.1, 1.06, 1.14, 25,
  "di_gee", "All", "All Origins", "all-cause", 1.073, 1.071, 1.075, 25
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

hr$max_age <- 150

anti_join <- diagnose_join_issues(df1 = pm_summ,
                                  df2 = hr,
                                  join_cols = c("Race", "Hispanic.Origin"))
if(nrow(anti_join) > 0){
  warning("diagnose_join_issues() in 21_calc_attr_burd_di.R: pm_summ, hr")
}

paf_di <- inner_join_age_right_outer(pm_summ,
  hr,
  by = c("Race", "Hispanic.Origin")
)

#paf_di <- paf_di %>%
#  dplyr::group_by_at(vars(one_of(setdiff(colnames(paf_di), c("pm", "pop_size"))))) %>%
#  summarise(pop_weight_pm_exp = weighted.mean(pm, pop_size)) %>%
#  ungroup()
#paf_di <- paf_di[1:200, ]
# Assuming delta_method_weighted_avg function is already defined

#paf_di <- paf_di %>% #TODO
#  ungroup() %>%
#  sample_n(2000)

paf_di <- paf_di %>%
  dplyr::group_by_at(vars(one_of(setdiff(colnames(paf_di), c("pm", "pm_lower", "pm_upper", "pop_size"))))) %>%
  do({
    pm = .$pm
    pm_lower = .$pm_lower
    pm_upper = .$pm_upper
    pop_size = .$pop_size
    result = delta_method_weighted_avg(pm, pm_lower, pm_upper, pop_size)
    data.frame(pop_weight_pm_exp = result$point_estimate,
               pop_weight_pm_exp_lower = result$ci_lower,
               pop_weight_pm_exp_upper = result$ci_upper)
  }) %>%
  ungroup()

# colnames(paf_di)
#[1] "Year"                    "county"                  "Race"                    "Hispanic.Origin"
#[5] "Gender.Code"             "Education"               "rural_urban_class"       "scenario"
#[9] "method"                  "label_cause"             "hr_mean"                 "hr_lower"
#[13] "hr_upper"                "min_age"                 "max_age"                 "pop_weight_pm_exp"
#[17] "pop_weight_pm_exp_lower" "pop_weight_pm_exp_upper"
rm(pm_summ, hr)
# Assuming delta_method_product is a function defined somewhere in your code
#browser()

paf_di <- paf_di %>%
  rowwise() %>%
  do({
    mean_x = .$pop_weight_pm_exp - 5
    lb_x = .$pop_weight_pm_exp_lower - 5
    ub_x = .$pop_weight_pm_exp_upper - 5

    mean_y = .$hr_mean - 1
    lb_y = .$hr_lower - 1
    ub_y = .$hr_upper - 1

    result = delta_method_product(mean_x, lb_x, ub_x, mean_y, lb_y, ub_y, alpha = 0.05)

    data.frame(., paf_mean = result$mean, paf_lower = result$lb, paf_upper = result$ub)
  })

paf_di <- paf_di %>%
   mutate(paf_mean = pmax(0, paf_mean),
           paf_lower = pmax(0, paf_lower),
           paf_upper = pmax(0, paf_upper))
#"hr_mean"                 "hr_lower"                "hr_upper"
#"pop_weight_pm_exp"       "pop_weight_pm_exp_lower" "pop_weight_pm_exp_upper"
#if (agr_by == "county") paf_di$rural_urban_class <- as.factor(666) # TODO change total burden
# Remove specific columns using the subset function
paf_di <- paf_di %>%
  subset(select = -c(hr_mean, hr_lower, hr_upper, pop_weight_pm_exp, pop_weight_pm_exp_lower, pop_weight_pm_exp_upper))

if (agr_by == "county") {
  total_burden <- total_burden %>%
    filter(county != "Unknown") %>%
    mutate(county = as.numeric(county))
}

##---drop some stuff---
#if ("rural_urban_class" %in% names(total_burden)) {
#  total_burden <- total_burden %>% select(-rural_urban_class)
#}

#if ("svi_bin" %in% names(total_burden)) {
#  total_burden <- total_burden %>% select(-svi_bin)
#}

if ("rural_urban_class" %in% names(paf_di)) {
  paf_di <- paf_di %>% select(-rural_urban_class)
}

if ("svi_bin" %in% names(paf_di)) {
  paf_di <- paf_di %>% select(-svi_bin)
}

if ("svi_bin1" %in% names(paf_di)) {
  paf_di <- paf_di %>% select(-svi_bin1)
}
if ("svi_bin2" %in% names(paf_di)) {
  paf_di <- paf_di %>% select(-svi_bin2)
}
if ("svi_bin3" %in% names(paf_di)) {
  paf_di <- paf_di %>% select(-svi_bin3)
}
if ("svi_bin4" %in% names(paf_di)) {
  paf_di <- paf_di %>% select(-svi_bin4)
}
#paf_di
##---join---
paf_di <- paf_di %>%
  select(-min_age) %>%
  select(-max_age)

attr_burden_di <- left_join(
  total_burden,
  paf_di,
  by = c("Year", agr_by, "Race", "Hispanic.Origin", "Gender.Code", "Education", "label_cause"),
  multiple = "all"#,
  #relationship = "many-to-many" #paf_di %>% filter(method == "di_gee" & scenario == "real")
)


#browser()
attr_burden_di <- attr_burden_di %>%
  mutate(
    mean = value * paf_mean,
    lower = value * paf_lower,
    upper = value * paf_upper,
    paf_mean = NULL, paf_lower = NULL, paf_upper = NULL,
    value = NULL, label_cause = NULL,
    attr = "attributable"#,
    # min_age = min(min_age.x, min_age.y),
    # max_age = max(max_age.x, max_age.y),
    # min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL
  )

attr_burden_di <- attr_burden_di %>%
  dplyr::group_by_at(vars(one_of(setdiff(colnames(attr_burden_di),
                                         c("mean", "lower", "upper"))))) %>%
  summarise(mean = sum(mean),
            lower = sum(lower),
            upper = sum(upper)) %>%
  ungroup()

attr_burden_di <- attr_burden_di %>% filter(method %in% c("di_gee", "di_gee_white"))
fwrite(attr_burden_di, attr_burdenDir)
toc()
