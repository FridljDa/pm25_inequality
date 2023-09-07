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

## --- read and bind attr burden----
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
attr_burden <- attr_burden %>%
  rbindlist(use.names = TRUE, fill = TRUE) %>%
  as.data.frame()

if (nrow(attr_burden) == 0) {
  stop(paste("attr_burd still missing in year", year, "in 25_summary_attr_total_burd.R"))
}
## --- read and bind all burden----
agr_bys <- list.files(summaryHigherTotalDir)
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

attr_burden <- attr_burden %>%
  mutate(Education = forcats::fct_recode(Education,
    "High school graduate or lower" = "lower",
    "Some college education but no 4-year college degree" = "middle",
    "4-year college graduate or higher" = "higher",
    "666" = "666"
  ))

total_burden <- total_burden %>%
  mutate(Education = forcats::fct_recode(Education,
    "High school graduate or lower" = "lower",
    "Some college education but no 4-year college degree" = "middle",
    "4-year college graduate or higher" = "higher",
    "666" = "666"
  ))

attr_burden <- attr_burden %>%
  mutate(Gender.Code = forcats::fct_recode(Gender.Code, "All genders" = "A"))
total_burden <- total_burden %>%
  mutate(Gender.Code = forcats::fct_recode(Gender.Code, "All genders" = "A"))

total_burden <- total_burden %>%
  mutate(source = forcats::fct_recode(source,
    "National Vital Statistics System" = "nvss"
  ))
attr_burden <- attr_burden %>%
  mutate(source = forcats::fct_recode(source,
    "National Vital Statistics System" = "nvss"
  ))

# rindreplace5 <- setNames(c("Years of Life Lost (YLL)", "Deaths"), c("YLL","Deaths"))
# total_burden$measure1 <- sapply(total_burden$measure1 , function(x) rindreplace5[[x]])
# attr_burden$measure1 <- sapply(attr_burden$measure1 , function(x) rindreplace5[[x]])

attr_burden <- attr_burden %>%
  mutate(measure2 = forcats::fct_recode(measure2,
    # "crude rate per 100,000" = "crude rate",
    "age-adjusted rate per 100,000" = "age-adjusted rate",
    # "absolute number" = "absolute number"
  ))
total_burden <- total_burden %>%
  mutate(measure2 = forcats::fct_recode(measure2,
    # "crude rate per 100,000" = "crude rate",
    "age-adjusted rate per 100,000" = "age-adjusted rate",
    # "absolute number" = "absolute number"
  ))

total_burden <- total_burden %>%
  unite("Ethnicity", Race, Hispanic.Origin, sep = ", ") %>%
  mutate(Ethnicity = as.factor(Ethnicity))
attr_burden <- attr_burden %>%
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
levels(total_burden$Ethnicity) <- rindreplace7
levels(attr_burden$Ethnicity) <- rindreplace7

total_burden <- total_burden %>%
  mutate(
    rural_urban_class = as.factor(rural_urban_class),
    rural_urban_class = forcats::fct_recode(rural_urban_class,
      "Large metro" = "1",
      "Small-medium metro" = "2",
      "Non metro" = "3",
      "All" = "666"
    )
  )
attr_burden <- attr_burden %>%
  mutate(
    rural_urban_class = as.factor(rural_urban_class),
    rural_urban_class = forcats::fct_recode(rural_urban_class,
      "Large metro" = "1",
      "Small-medium metro" = "2",
      "Non metro" = "3",
      "All" = "666"
    )
  )
attr_burden <- attr_burden %>%
  mutate(
    svi_bin = as.factor(svi_bin),
    svi_bin = forcats::fct_recode(svi_bin,
      "high svi" = "1",
      "middle svi" = "2",
      "low svi" = "3",
      "All" = "666"
    )
  )
total_burden <- total_burden %>%
  mutate(
    svi_bin = as.factor(svi_bin),
    svi_bin = forcats::fct_recode(svi_bin,
      "high svi" = "1",
      "middle svi" = "2",
      "low svi" = "3",
      "All" = "666"
    )
  )
# rm(rindreplace1, rindreplace2, rindreplace3, rindreplace4, rindreplace6, rindreplace7, rindreplace8)


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
