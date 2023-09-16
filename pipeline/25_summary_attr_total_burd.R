suppressMessages({
  library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
  library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
  library(data.table, warn.conflicts = FALSE, quietly = TRUE)
  # library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
  library(tictoc, warn.conflicts = FALSE, quietly = TRUE)

  library(purrr, warn.conflicts = FALSE, quietly = TRUE)
  library(tibble, warn.conflicts = FALSE, quietly = TRUE)
})


suppressMessages({
  pkgload::load_all()
})
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 100000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)
# agr_by <- args[10]
# tmpDir <- args[1]
# summaryHigherTotalDir <- args[5]


# if (rlang::is_empty(args)) {
tmpDir <- "data/tmp"
summaryHigherTotalDir <- "data/16_sum_higher_geog_level_total"
propOfAttrBurdDir <- "data/16_prop_of_attr_burd"
summaryDir <- "data/17_summary"
# }

create_directory(summaryDir)

states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  select(NAME, STATEFP)

## ---- read total burden, attr burden, join----
tic("summarized all burden and attributable burden data")

agr_bys <- list.files(summaryHigherTotalDir) # TODO , "STATEFP", "county"
agr_bys <- "nation"
## --- read and bind attr burden----
cat("row-bind attr_burden-start")
tic("row-bind attr_burden")
attr_burden <- lapply(agr_bys, function(agr_by) {
  files <- list.files(file.path(propOfAttrBurdDir, agr_by))
  attr_burden <- lapply(files, function(file) read_data(file.path(propOfAttrBurdDir, agr_by, file)))

  attr_burden <- attr_burden %>% rbindlist(use.names = TRUE, fill = TRUE)
  if (nrow(attr_burden) == 0) {
    return(tibble()) # Return an empty tibble
  }

  # make compatible
  attr_burden <- attr_burden %>% rename("Region" := !!agr_by)
  attr_burden <- attr_burden %>% tibble::add_column(agr_by = agr_by)
  # attr_burden <- attr_burden %>% filter(scenario == "A") # TODO delete
  if (agr_by == "county") {
    attr_burden <- attr_burden %>%
      filter(measure2 %in% c("absolute number", "age-adjusted rate") &
        scenario == "real") # TODO delete
  }
  return(attr_burden)
})
toc()
attr_burden <- attr_burden %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  as.data.frame()

if (nrow(attr_burden) == 0) {
  stop(paste("attr_burd still missing in year", year, "in 25_summary_attr_total_burd.R"))
}
## --- read and bind all burden----
cat("row-bind total_burden-start")
tic("row-bind total_burden")
agr_bys <- list.files(summaryHigherTotalDir)
agr_bys <- "nation"
# Main operation
total_burden <- map_dfr(agr_bys, function(agr_by) {
  files <- list.files(file.path(summaryHigherTotalDir, agr_by))

  total_burden <- map_dfr(files, ~ read_data(file.path(summaryHigherTotalDir, agr_by, .x)))

  if (nrow(total_burden) == 0) {
    return(tibble()) # Return an empty tibble
  }

  total_burden <- total_burden %>%
    filter(label_cause == "all-cause") %>%
    rename("Region" = !!agr_by) %>%
    mutate(Region = as.factor(Region)) %>%
    add_column(agr_by = agr_by)

  return(total_burden)
}) %>%
  as.data.frame()
toc()

# Optionally, remove rows where all columns have NAs or are empty
# total_burden <- total_burden %>% filter(!complete.cases(sapply(., is.na)))
if (nrow(total_burden) == 0) {
  stop(paste("total_burden still missing in year", year, "in 25_summary_attr_total_burd.R"))
}

#-----filter, summarise total burden-----
total_burden <- total_burden %>% filter(label_cause == "all-cause" & attr == "overall" &
  measure1 == "Deaths") # & measure2 == "age-adjusted rate"

# group_variables <- setdiff(colnames(total_burden), c("value", "min_age", "max_age"))
# total_burden <- total_burden %>%
#  dplyr::group_by_at(vars(all_of(group_variables))) %>%
#  dplyr::summarise(
#    overall_value = sum(value),
#    min_age = min(min_age),
#    max_age = max(max_age)
#  ) %>%
#  dplyr::ungroup()

total_burden <- total_burden %>%
  select(-c(label_cause, attr)) # , attr

## ---find and replace in total burden ----

rindreplace1 <- data.frame(
  agr_by = c("nation", rep("STATEFP", nrow(states))),
  RegionFrom = c("us", states$STATEFP),
  RegionTo = c("United States", states$NAME)
)
total_burden <- total_burden %>%
  left_join(rindreplace1, by = c("Region" = "RegionFrom", "agr_by")) %>%
  mutate(
    Region = coalesce(RegionTo, Region),
    RegionTo = NULL
  )

attr_burden <- attr_burden %>%
  left_join(rindreplace1, by = c("Region" = "RegionFrom", "agr_by")) %>%
  mutate(
    Region = coalesce(RegionTo, Region),
    RegionTo = NULL
  )


total_burden <- total_burden %>%
  unite("Ethnicity", Race, Hispanic.Origin, sep = ", ") %>%
  mutate(Ethnicity = as.factor(Ethnicity))
attr_burden <- attr_burden %>%
  unite("Ethnicity", Race, Hispanic.Origin, sep = ", ") %>%
  mutate(Ethnicity = as.factor(Ethnicity))

findreplace <- read.csv("data/final_findreplace.csv")

cat("total_burden findreplace-start")
tic("total_burden findreplace")
total_burden <- total_burden %>% replace_values(findreplace)
toc()

cat("attr_burden findreplace-start")
tic("attr_burden findreplace")
attr_burden <- attr_burden %>% replace_values(findreplace)
toc()

## --- test final---
total_burden <- total_burden %>% filter(!is.na(rural_urban_class)) # TODO

#--write---
# attr_burden<- attr_burden %>% ungroup %>% select(Year, Ethnicity, Education, rural_urban_class,measure1, measure2, measure3, Region, scenario, mean, lower, upper, method)
# total_burden<- total_burden %>% select(Year, Ethnicity, Education, rural_urban_class,measure1, measure2, Region, overall_value)
measure3_all <- attr_burden$measure3 %>% unique()
method_all <- attr_burden$method %>% unique()

total_burden_county <- total_burden %>% filter(agr_by == "county")
total_burden_not_county <- total_burden %>% filter(agr_by != "county")
attr_burden_county <- attr_burden %>% filter(agr_by == "county")
attr_burden_not_county <- attr_burden %>% filter(agr_by != "county")

# write nation and STATEFP
fwrite(
  total_burden_not_county,
  file.path(summaryDir, paste0("all_burd.csv"))
)

attr_burden_not_county_split <- attr_burden_not_county %>%
  split(list(attr_burden_not_county$measure3, attr_burden_not_county$agr_by, attr_burden_not_county$min_age), drop = TRUE)

for (i in seq_along(attr_burden_not_county_split)) {
  agr_by_i <- attr_burden_not_county_split[[i]]$agr_by[[1]]
  fwrite(
    attr_burden_not_county_split[[i]],
    file.path(summaryDir, paste0("attr_burd_", agr_by_i, "_", i, ".csv"))
  )
}

dir.create(file.path(summaryDir, "county"), recursive = T, showWarnings = F)

fwrite(
  total_burden_county,
  file.path(summaryDir, "county", paste0("all_burd.csv"))
)

# write county
attr_burden_county_split <- attr_burden_county %>%
  split(list(attr_burden_county$method, attr_burden_county$measure3, attr_burden_county$agr_by, attr_burden_county$min_age), drop = TRUE)

for (i in seq_along(attr_burden_county_split)) {
  fwrite(
    attr_burden_county_split[[i]],
    file.path(summaryDir, "county", paste0("attr_burd_", i, ".csv"))
  )
}

toc()
