suppressMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  library(tidyverse)
  library(tictoc)
})

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
pkgload::load_all()

# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  dataDir <- "data"
  pop.summary.dir <- "data/12_population_summary"
  attr_burdenDir <- "data/14_attr_burd"
  summaryHigherDir <- "data/15_sum_higher_geog_level"
  agr_by <- "nation"
  year <- 2002
} else {
  year <- args[1]
  dataDir <- args[2]
  agr_by <- args[10]
  pop.summary.dir <- args[16]
  attr_burdenDir <- args[18]
  summaryHigherDir <- args[19]
}
# should have #min_age = min_age.x, max_age = min_age.x, for age specific
# in l184 in 21_calc_attr_burd_di.R


summaryHigherDir <- file.path(summaryHigherDir, agr_by)
dir.create(summaryHigherDir, recursive = T, showWarnings = F)
summaryHigherDir <- file.path(summaryHigherDir, paste0("attr_burden_age_adjusted_", year, ".csv"))

if (file.exists(summaryHigherDir)) {
  quit()
}
## --- read attr burden----
attr_burdenDir <- file.path(attr_burdenDir, "county")
files <- list.files(file.path(attr_burdenDir, "nvss"))
files <- files[grepl(year, files)]

attr_burden <- lapply(files, function(file) {
  attr_burden_i <- read_data(file.path(attr_burdenDir, "nvss", file))
}) %>% rbindlist(use.names = TRUE, fill = TRUE)

## --sum up geographic levels from county----

#if (agr_by == "county") {
#  attr_burden <- attr_burden %>%
#    filter(measure1 == "Deaths" &
#      measure2 == "age-adjusted rate")

#  group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper", "min_age", "max_age"))

#  attributable_burden_age_adj_over_25 <- attr_burden %>%
#    filter(min_age >= 25) %>%
#    group_by_at(vars(all_of(group_variables))) %>%
#    summarise(
#      mean = sum(mean),
#      lower = sum(lower),
#      upper = sum(upper)
#    ) %>%
#    ungroup() %>%
#    mutate(
#      min_age = 25,
#      max_age = 150
#    )

#  attributable_burden_age_adj_over_65 <- attr_burden %>%
#    filter(min_age >= 65) %>%
#    group_by_at(vars(all_of(group_variables))) %>%
#    summarise(
#      mean = sum(mean),
#      lower = sum(lower),
#      upper = sum(upper)
#    ) %>%
#    ungroup() %>%
#    mutate(
#      min_age = 65,
#      max_age = 150
#    )

#  attributable_burden_age_adj <- rbind(attributable_burden_age_adj_over_25, attributable_burden_age_adj_over_65)

#  fwrite(attributable_burden_age_adj, summaryHigherDir)
#  quit()
#}
# continue if State or nation
### ---- add rural urban class----
# rural_urban_class_find_replace <- set_names(rural_urban_class$rural_urban_class,
#                                            rural_urban_class$FIPS.code)
# rural_urban_class_find_replace <- as.character(rural_urban_class_find_replace)
# attr_burden$rural_urban_class <- recode(attr_burden$rural_urban_class, rural_urban_class_find_replace, .default = "Unknown")

if(agr_by != "county"){
  attr_burden_with_svi_rural_urban_class <- attr_burden %>%
    add_rural_urban_class(FIPS.code.column = "county") %>%
    add_social_vuln_index(FIPS.code.column = "county")

  attr_burden_with_rural_urban_class <- attr_burden_with_svi_rural_urban_class %>%
    group_by_at(setdiff(colnames(attr_burden_with_svi_rural_urban_class),
                        c("rural_urban_class", "mean", "lower", "upper"))) %>%
    summarise(mean = sum(mean),
              lower = sum(lower),
              upper = sum(upper)) %>%
    mutate(rural_urban_class = as.factor(666))

  attr_burden_with_svi_bin <- attr_burden_with_svi_rural_urban_class %>%
    group_by_at(setdiff(colnames(attr_burden_with_svi_rural_urban_class),
                        c("svi_bin", "mean", "lower", "upper"))) %>%
    summarise(mean = sum(mean),
              lower = sum(lower),
              upper = sum(upper)) %>%
    mutate(svi_bin = as.factor(666))

  attr_burden_with_all <- attr_burden_with_svi_rural_urban_class %>%
    group_by_at(setdiff(colnames(attr_burden_with_svi_rural_urban_class),
                        c("rural_urban_class", "svi_bin", "mean", "lower", "upper"))) %>%
    summarise(mean = sum(mean),
              lower = sum(lower),
              upper = sum(upper)) %>%
    mutate(rural_urban_class = as.factor(666), svi_bin = as.factor(666))

  attr_burden <- rbind(attr_burden_with_rural_urban_class,
                       attr_burden_with_svi_bin,
                       attr_burden_with_all)

  rm(attr_burden_with_rural_urban_class,
     attr_burden_with_svi_bin,
     attr_burden_with_all)
}else{
  #TODO
}


## ----group out counties---

if (agr_by == "STATEFP") {
  attr_burden <- attr_burden %>%
    mutate(
      STATEFP = str_sub(county, 1, -4) %>%
        as.integer() %>%
        as.factor()
    )
  group_variables <- setdiff(colnames(attr_burden),
                             c("lower", "mean", "upper", "county"))

} else if (agr_by == "nation") {
  attr_burden <- attr_burden %>%
    mutate(
      nation = "us"
    )
  group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper", "county"))

} else if (agr_by == "county") {
  group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper"))

}
tic(paste("summed up county level estimates to ", agr_by, " and age adjusted in year ", year))

attr_burden <- attr_burden %>%
  group_by_at(vars(all_of(c(group_variables)))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup()

#------ age-standartised rates-------
attr_burden_absolute_number <- attr_burden %>%
  filter(measure1 == "Deaths" &
           measure2 == "absolute number")

pop_summary <- get_population_data(agr_by, year)
attributable_burden_age_adj <- add_age_adjusted_rate(attr_burden_absolute_number, pop_summary, path_to_standartpopulation = "data/standartpopulation.xlsx")

attr_burden <- rbind(attributable_burden_age_adj,
                     attr_burden_absolute_number)

rm(attributable_burden_age_adj, attr_burden_absolute_number)
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

attr_burden <- rbind(attr_burden_over_25,  attr_burden_over_65)

fwrite(attr_burden, summaryHigherDir)
toc()
