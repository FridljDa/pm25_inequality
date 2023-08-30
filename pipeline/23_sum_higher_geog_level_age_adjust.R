suppressMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  # library(tidyverse)
  library(tictoc)
})

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
pkgload::load_all()

# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  agr_by <- "nation"
  year <- 2009
} else {
  year <- args[1]
  dataDir <- args[2]
  agr_by <- args[10]
  pop.summary.dir <- args[16]
  attr_burdenDir <- args[18]
  summaryHigherDir <- args[19]
}

dataDir <- "data"
pop.summary.dir <- "data/12_population_summary"
attr_burdenDir <- "data/14_attr_burd"
summaryHigherDir <- "data/15_sum_higher_geog_level"

# should have #min_age = min_age.x, max_age = min_age.x, for age specific
# in l184 in 21_calc_attr_burd_di.R


summaryHigherDir <- file.path(summaryHigherDir, agr_by)
dir.create(summaryHigherDir, recursive = T, showWarnings = F)
summaryHigherDir <- file.path(summaryHigherDir, paste0("attr_burden_age_adjusted_", year, ".csv"))

if (file.exists(summaryHigherDir)) {
  quit()
}
## --- read attr burden----
attr_burdenDir <- file.path(attr_burdenDir, "county", "nvss")
files <- list.files(file.path(attr_burdenDir))
files <- files[grepl(year, files)]

if(rlang::is_empty(files)){
  stop(paste("in",attr_burdenDir, "year", year, "missing"))
}

attr_burden <- lapply(files, function(file) {
  attr_burden_i <- read_data(file.path(attr_burdenDir, file))

  if ("rural_urban_class.x" %in% colnames(attr_burden_i)) {
    attr_burden_i <- attr_burden_i %>%
      mutate(
        rural_urban_class = as.factor(rural_urban_class.x),
        rural_urban_class.x = NULL,
        rural_urban_class.y = NULL
      )
  }
  attr_burden_i
}) %>% rbindlist(use.names = TRUE, fill = TRUE)

if(year <= 2008){
  attr_burden <- attr_burden %>%
    filter(Education == "666")
}
#attr_burden <- attr_burden %>% sample_n(20)
## --sum up geographic levels from county----
#attr_burden <- attr_burden %>% sample_n(20)
if (agr_by != "county") {
  # tic("added rural_urban class info to attr_burden")
  # attr_burden_with_svi_rural_urban_class <- attr_burden %>%
  #  add_rural_urban_class(FIPS.code.column = "county", silent = FALSE)
  # toc()

  # tic("added svi info to attr_burden")
  # attr_burden_with_svi_rural_urban_class <- attr_burden_with_svi_rural_urban_class %>%
  #  add_social_vuln_index(FIPS.code.column = "county", silent = FALSE)
  # toc()
  print("head of attr_burden in 23_sum_higher_geog_level_age_adj")
  print(head(attr_burden))

  tic("marginalised svi and rural_urban class info to attr_burden")
  cat("marginalised svi and rural_urban class info to attr_burden 1-starting\n")
  tic("marginalised svi and rural_urban class info to attr_burden 1")
  cols_to_group_by <- setdiff(names(attr_burden), c("rural_urban_class", "mean", "lower", "upper"))
  attr_burden_with_rural_urban_class <- attr_burden %>%
    group_by(across(all_of(cols_to_group_by))) %>%
    summarise(
      mean = sum(mean),
      lower = sum(lower),
      upper = sum(upper),
      .groups = 'drop'
    ) %>%
    mutate(rural_urban_class = as.factor(666))
  toc()

  cat("marginalised svi and rural_urban class info to attr_burden 2-starting\n")
  tic("marginalised svi and rural_urban class info to attr_burden 2")
  cols_to_group_by <- setdiff(names(attr_burden), c("svi_bin", "mean", "lower", "upper"))
  attr_burden_with_svi_bin <- attr_burden %>%
    group_by(across(all_of(cols_to_group_by))) %>%
    summarise(
      mean = sum(mean),
      lower = sum(lower),
      upper = sum(upper),
      .groups = 'drop'
    ) %>%
    mutate(svi_bin = as.factor(666))
  toc()

  cat("marginalised svi and rural_urban class info to attr_burden 3-starting\n")
  tic("marginalised svi and rural_urban class info to attr_burden 3")
  cols_to_group_by <- setdiff(names(attr_burden), c("rural_urban_class", "svi_bin", "mean", "lower", "upper"))
  attr_burden_with_all <- attr_burden %>%
    group_by(across(all_of(cols_to_group_by))) %>%
    summarise(
      mean = sum(mean),
      lower = sum(lower),
      upper = sum(upper),
      .groups = 'drop'
    ) %>%
    mutate(rural_urban_class = as.factor(666), svi_bin = as.factor(666))
  toc()

  attr_burden <- rbind(
    attr_burden_with_rural_urban_class,
    attr_burden_with_svi_bin,
    attr_burden_with_all
  )

  rm(
    attr_burden_with_rural_urban_class,
    attr_burden_with_svi_bin,
    attr_burden_with_all
  )
  toc()
} else {
  # attr_burden$rural_urban_class <- NA
  # attr_burden$svi_bin <- NA
}




## ----group out counties---

if (agr_by == "STATEFP") {
  attr_burden <- attr_burden %>%
    mutate(
      STATEFP = stringr::str_sub(county, 1, -4) %>%
        as.integer() %>%
        as.factor()
    )
  group_variables <- setdiff(
    colnames(attr_burden),
    c("lower", "mean", "upper", "county")
  )
} else if (agr_by == "nation") {
  attr_burden <- attr_burden %>%
    mutate(nation = "us")
  group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper", "county"))
} else if (agr_by == "county") {
  group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper"))
}

tic(paste("summed up county level estimates to ", agr_by, "in year ", year))
attr_burden <- attr_burden %>%
  group_by_at(vars(all_of(c(group_variables)))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup()

toc()

#------ age-standartised rates-------
attr_burden_absolute_number <- attr_burden %>%
  filter(measure1 == "Deaths" &
    measure2 == "absolute number")

tic("age standardised attributable burden")
attr_burden <- add_age_adjusted_rate(attr_burden_absolute_number,
  year,
  agr_by,
  pop.summary.dir = "data/12_population_summary"
)



toc()

### ----sum out age ----

group_variables <- setdiff(colnames(attr_burden), c("mean", "lower", "upper", "min_age", "max_age"))
attr_burden_over_25 <- attr_burden %>%
  filter(min_age >= 25) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup() %>%
  mutate(
    min_age = 25,
    max_age = 150
  )

attr_burden_over_65 <- attr_burden %>%
  filter(min_age >= 65) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup() %>%
  mutate(
    min_age = 65,
    max_age = 150
  )

attr_burden <- rbind(attr_burden_over_25, attr_burden_over_65)

fwrite(attr_burden, summaryHigherDir)
