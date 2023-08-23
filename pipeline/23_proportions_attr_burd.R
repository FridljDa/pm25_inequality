suppressMessages(library(dplyr, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(magrittr, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(data.table, warn.conflicts = FALSE, quietly = TRUE))

suppressMessages(library(tictoc, warn.conflicts = FALSE, quietly = TRUE))

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)
# agr_by <- args[10]
year <- args[1]
source <- "nvss"
agr_by <- args[10]
totalBurdenParsed2Dir <- args[17]
summaryHigherDir <- args[19]
propOfAttrBurdDir <- args[20]

if (rlang::is_empty(args)) {
  dataDir <- "data"
  totalBurdenParsed2Dir <- "data/13_total_burden_rate"
  summaryHigherDir <- "data/15_sum_higher_geog_level"
  propOfAttrBurdDir <- "data/16_prop_of_attr_burd"
  agr_by <- "nation"
  source <- "nvss"
  year <- 1996
}

propOfAttrBurdDir <- file.path(propOfAttrBurdDir, agr_by)
dir.create(propOfAttrBurdDir, recursive = T, showWarnings = F)
propOfAttrBurdDir <- file.path(propOfAttrBurdDir, paste0("attr_burden_prop_", year, ".csv"))

if (file.exists(propOfAttrBurdDir)) {
  quit()
}

## ---- read total burden, attr burden, join----
tic("calculated proportions of attributable burden and parsed")
summaryHigherDir <- file.path(summaryHigherDir, agr_by, paste0("attr_burden_age_adjusted_", year, ".csv"))
attributable_burden <- fread(summaryHigherDir)
attributable_burden <- attributable_burden %>% mutate(Gender.Code = as.character(Gender.Code))

total_burden <- file.path(totalBurdenParsed2Dir, agr_by, source, paste0("total_burden_", year, ".csv")) %>%
  fread()

#-----filter, summarise total burden-----
total_burden <- total_burden %>% filter(label_cause == "all-cause" & attr == "overall" &
  measure1 == "Deaths") #& 

if(agr_by == "county") total_burden <- total_burden %>% filter(measure2 == "age-adjusted rate")

total_burden <- total_burden %>%
  filter(min_age >= 25) # to be replaced for 65+

group_variables <- setdiff(colnames(total_burden), c("value", "min_age", "max_age"))
total_burden_over_25 <- total_burden %>%
  filter(min_age >= 25) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(overall_total_burden = sum(value)) %>%
  ungroup() %>%
  mutate(
    min_age = 25,
    max_age = 150
  )

total_burden_over_65 <- total_burden %>%
  filter(min_age >= 65) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(overall_total_burden = sum(value)) %>%
  ungroup() %>%
  mutate(
    min_age = 65,
    max_age = 150
  )

total_burden <- rbind(total_burden_over_25, total_burden_over_65)
total_burden <- total_burden %>%
  select(-c(label_cause, attr))

if(agr_by == "county"){
  total_burden <- total_burden %>%
    filter(county != "Unknown") %>%
    mutate(county = as.numeric(county))
}

## ---join---
test <- anti_join(attributable_burden, total_burden,
                                by = c(
                                  "Year", "Gender.Code", "Race", "Hispanic.Origin", "rural_urban_class", "Education",
                                  "source", "measure1", "measure2", agr_by, "min_age", "max_age"
                                )
)

attr_total_burden <- inner_join(attributable_burden, total_burden,
  by = c(
    "Year", "Gender.Code", "Race", "Hispanic.Origin", "rural_urban_class", "Education",
    "source", "measure1", "measure2", agr_by, "min_age", "max_age"
  )
)

#--simple age adjusted---
attr_total_burden_value <- attr_total_burden %>%
  mutate(
    overall_total_burden = NULL,
    measure3 = "value"
  )
#---proportion of overall burden---
attr_total_burden_prop_overall_burden <- attr_total_burden %>%
  mutate(
    mean = coalesce(100 * mean / overall_total_burden, 0),
    lower = coalesce(100 * lower / overall_total_burden, 0),
    upper = coalesce(100 * upper / overall_total_burden, 0),
    overall_total_burden = NULL,
    measure3 = "prop. of overall burden"
  )

### ---disparity to race-ethnicity----
# Race == "Black or African American" & Hispanic.Origin == "All Origins"
group_variables <- setdiff(colnames(attr_total_burden), c("mean", "lower", "upper", "overall_total_burden", "Race", "Hispanic.Origin"))
attr_total_burden_prop_of_difference <- attr_total_burden %>%
  group_by_at(vars(all_of(group_variables))) %>%
  filter("Black or African American" %in% c(Race)) %>%
  mutate(
    mean = 100 * (mean - mean[Race == "Black or African American" & Hispanic.Origin == "All Origins"]) /
      (overall_total_burden - overall_total_burden[Race == "Black or African American" & Hispanic.Origin == "All Origins"]),
    lower = 100 * (lower - lower[Race == "Black or African American" & Hispanic.Origin == "All Origins"]) /
      (overall_total_burden - overall_total_burden[Race == "Black or African American" & Hispanic.Origin == "All Origins"]),
    upper = 100 * (upper - upper[Race == "Black or African American" & Hispanic.Origin == "All Origins"]) /
      (overall_total_burden - overall_total_burden[Race == "Black or African American" & Hispanic.Origin == "All Origins"])
  ) %>%
  ungroup() %>%
  mutate(
    overall_total_burden = NULL,
    measure3 = "proportion of disparity to Black or African American attributable"
  ) %>%
  filter(!(Race == "Black or African American" & Hispanic.Origin == "All Origins"))

attr_total_burden_combined <- rbind(attr_total_burden_value, attr_total_burden_prop_overall_burden, attr_total_burden_prop_of_difference)

fwrite(attr_total_burden_combined, propOfAttrBurdDir)




toc()
