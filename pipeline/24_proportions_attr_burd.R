suppressMessages(library(dplyr, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(magrittr, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(data.table, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(tictoc, warn.conflicts = FALSE, quietly = TRUE))

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

suppressMessages({pkgload::load_all()})
# Pass in arguments
args <- commandArgs(trailingOnly = T)
# agr_by <- args[10]

if (rlang::is_empty(args)) {
  dataDir <- "data"

  agr_by <- "STATEFP"
  source <- "nvss"
  year <- 2016
} else {
  year <- args[1]
  source <- "nvss"
  agr_by <- args[10]
  totalBurdenParsed2Dir <- args[17]
  summaryHigherDir <- args[19]
  summaryHigherTotalDir <- args[20]
}


propDir <- file.path("data/16_prop_of_attr_burd", agr_by)
dir.create(propDir, recursive = T, showWarnings = F)
propDir <- file.path(propDir, paste0("attr_burden_prop_", year, ".csv"))

if (file.exists(propDir)) {
  quit()
}

## ---- read total burden, attr burden, join----
tic("calculated proportions of attributable burden and parsed")
summaryHigherDir <- file.path("data/15_sum_higher_geog_level", agr_by, paste0("attr_burden_age_adjusted_", year, ".csv"))
attributable_burden <- read_data(summaryHigherDir)
#attributable_burden <- attributable_burden %>% mutate(Gender.Code = as.character(Gender.Code))

total_burden <- paste0("data/16_sum_higher_geog_level_total/",agr_by,"/total_burden_age_adjusted_", year, ".csv") %>%
                        read_data()
#-----filter, summarise total burden-----
total_burden <- total_burden %>% filter(label_cause == "all-cause" & attr == "overall" &
  measure1 == "Deaths") # &

#if (agr_by == "county") total_burden <- total_burden %>% filter(measure2 == "age-adjusted rate")

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

if (agr_by == "county") {
  total_burden <- total_burden %>%
    filter(county != "Unknown") %>%
    mutate(county = as.numeric(county))
}

## ---join---
anti_join <- diagnose_join_issues(df1 = total_burden,
                                  df2 = attributable_burden,
                                  join_cols = c(
                                    "Year", "Gender.Code", "Race", "Hispanic.Origin", "rural_urban_class","svi_bin",  "Education",
                                    "source", "measure1", "measure2", agr_by, "min_age", "max_age", "svi_bin1", "svi_bin2", "svi_bin3", "svi_bin4"
                                  ))
#if(nrow(anti_join) > 0){
#  warning("diagnose_join_issues() in 24_proportions_attr_burd.R: total_burden, attributable_burden")
#}

#"svi_bin",
attr_total_burden <- inner_join(
  attributable_burden,
  total_burden,
  by = c(
    "Year", "Gender.Code", "Race", "Hispanic.Origin", "svi_bin", "rural_urban_class", "Education",
    "source", "measure1", "measure2", agr_by, "min_age", "max_age",
    "svi_bin1", "svi_bin2", "svi_bin3", "svi_bin4"
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

group_variables <- setdiff(colnames(attr_total_burden), c("mean", "lower", "upper", "overall_total_burden", "Race", "Hispanic.Origin"))
attr_total_burden_prop_of_difference <- attr_total_burden %>%
  group_by(across(all_of(group_variables))) %>%
  filter("Black or African American" %in% c(Race))
if(nrow(attr_total_burden_prop_of_difference) == 0){
  warning(paste("1: in 24_proportions_attr_burd.R, attr_total_burden_prop_of_difference has 0 rows ", year, agr_by))
}

attr_total_burden_prop_of_difference <- attr_total_burden_prop_of_difference%>%
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
  )
if(nrow(attr_total_burden_prop_of_difference) == 0){
  warning(paste("2: in 24_proportions_attr_burd.R, attr_total_burden_prop_of_difference has 0 rows ", year, agr_by))
}

attr_total_burden_prop_of_difference <- attr_total_burden_prop_of_difference %>%
  filter(!(Race == "Black or African American" & Hispanic.Origin == "All Origins"))

if(nrow(attr_total_burden_prop_of_difference) == 0){
  warning(paste("3: in 24_proportions_attr_burd.R, attr_total_burden_prop_of_difference has 0 rows ", year, agr_by))
}

attr_total_burden_combined <- rbind(attr_total_burden_value, attr_total_burden_prop_overall_burden, attr_total_burden_prop_of_difference)

fwrite(attr_total_burden_combined, propDir)


toc()
