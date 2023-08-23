# if (rlang::is_empty(args)) {

library(dplyr)
library(magrittr)
library(data.table)


options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

dataDir <- "data"
pop.summary.dir <- "data/12_population_summary"
totalBurdenParsed2Dir <- "data/13_total_burden_rate"
attr_burdenDir <- "data/14_attr_burd"
summaryDir <- "data/15_summary"
summaryHigherDir <- "data/16_summary_higher"
# }
# should have #min_age = min_age.x, max_age = min_age.x, for age specific
# in l184 in 21_calc_attr_burd_di.R
agr_bys <- "county"
## --- read attr burden----
attr_burdenDir <- file.path(attr_burdenDir, "county_20221113_with_age")
files <- list.files(file.path(attr_burdenDir, "nvss"))
# files <- files[1]
attr_burden <- lapply(files, function(file) fread(file.path(attr_burdenDir, "nvss", file))) %>% rbindlist(use.names = TRUE)
# TODO

attr_burden <- attr_burden %>%
  filter(measure1 == "Deaths" &
    measure2 == "absolute number")
attr_burden_age_adjusted_county <- attr_burden %>%
  filter(measure1 == "Deaths" &
    measure2 == "age-adjusted rate per 100,000")
##
## --nation level---
attr_burden_nation <- attr_burden %>%
  mutate(
    Region = "us",
    agr_by = "nation"
  )

attr_burden_state <- attr_burden %>%
  mutate(
    Region = str_sub(county, 1, -4) %>% as.integer(),
    agr_by = "STATEFP"
  )

group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper", "county"))
attr_burden <- rbind(attr_burden) %>%
  group_by_at(vars(all_of(c(group_variables)))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  )

# group_variables <- setdiff(colnames(all_burden), c("overall_value", "Region"))
# all_burden_nation <- all_burden %>%
#  group_by_at(vars(all_of(c(group_variables)))) %>%
#  summarise(value = sum(value))
#---read population data----
agr_by <- "nation"
if (file.exists(file.path(pop.summary.dir, paste0("pop_", agr_by, ".csv"))) & agr_by != "county") {
  pop_summary1 <- file.path(pop.summary.dir, paste0("pop_", agr_by, ".csv")) %>%
    read.csv()
} else {
  pop_summary1 <- NULL
}

file_list <- list.files(file.path(pop.summary.dir, agr_by))
pop_summary2 <- lapply(file_list, function(file) fread(file.path(pop.summary.dir, agr_by, file))) %>%
  rbindlist(use.names = TRUE)

if (agr_by != "county") {
  pop_summary2 <- pop_summary2 %>% filter(!(rural_urban_class == 666 & Education == 666))
}

if (agr_by == "nation") {
  pop_summary3 <- file.path(pop.summary.dir, paste0("pop_race_educ_nation.csv")) %>%
    read.csv()
} else {
  pop_summary3 <- NULL
}

pop_summary <- rbind(pop_summary1, pop_summary2, pop_summary3) %>% distinct()

pop_summary <- pop_summary %>%
  mutate_at(c("rural_urban_class", "Education"), as.factor) %>%
  mutate(source2 = NULL)

rm(pop_summary1, pop_summary2)

if (agr_by == "nation") {
  pop_summary <- pop_summary %>%
    complete(Year, nation,
      nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education),
      rural_urban_class,
      fill = list(Population = 0)
    ) %>%
    mutate_at(c("nation"), as.factor)
} else if (agr_by == "STATEFP") {
  pop_summary <- pop_summary %>%
    complete(Year, STATEFP, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
      fill = list(Population = 0)
    ) %>%
    mutate_at(c("STATEFP"), as.factor)
} else if (agr_by == "county") {
  pop_summary <- pop_summary %>%
    # complete(Year, county, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
    #         fill = list(Population = 0)
    # )%>%
    mutate_at(c("county"), as.factor)

  # pop_summary <- pop_summary %>% filter(Race != "All")
}

# age-standartised rates
# see https://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_14.pdf, page 125 for more information, Table VIII
standartpopulation <- readxl::read_excel(file.path(dataDir, "standartpopulation.xlsx"))
full_stand_popsize <- sum(standartpopulation$standard_popsize)

attributable_burden_age_adj <- crossing(pop_summary, standartpopulation)
attributable_burden_age_adj <- attributable_burden_age_adj %>%
  mutate(
    largerInterval = case_when(
      min_age <= standard_min_age & standard_max_age <= max_age ~ 1,
      standard_min_age <= min_age & max_age <= standard_max_age ~ 2
    ),
    min_age = pmin(min_age, standard_min_age), max_age = pmax(max_age, standard_max_age),
    standard_min_age = NULL, standard_max_age = NULL
  )

attributable_burden_age_adj1 <- attributable_burden_age_adj %>%
  filter(largerInterval == 1) %>%
  group_by_at(vars(all_of(setdiff(colnames(attributable_burden_age_adj), "standard_popsize")))) %>%
  summarise(standard_popsize = sum(standard_popsize))

attributable_burden_age_adj2 <- attributable_burden_age_adj %>%
  filter(largerInterval == 2) %>%
  group_by_at(vars(all_of(setdiff(colnames(attributable_burden_age_adj), "Population")))) %>%
  summarise(Population = sum(Population))

attributable_burden_age_adj <- rbind(attributable_burden_age_adj1, attributable_burden_age_adj2) %>%
  distinct() %>%
  ungroup()
attributable_burden_age_adj$largerInterval <- NULL
rm(attributable_burden_age_adj1, attributable_burden_age_adj2)

join_variables <- setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population"))
colnames(attr_burden)
# calculate age-adjusted rate
attributable_burden_age_adj <- attr_burden %>%
  mutate(
    rural_urban_class = as.factor(rural_urban_class),
    Education = as.factor(Education)
  ) %>%
  left_join(attributable_burden_age_adj,
    by = join_variables
  ) %>%
  filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
  mutate(min_age.x = NULL, max_age.x = NULL) %>%
  rename(min_age = min_age.y, max_age = max_age.y)

attributable_burden_age_adj <- attributable_burden_age_adj %>%
  group_by_at(vars(all_of(setdiff(colnames(attributable_burden_age_adj), c("mean", "lower", "upper"))))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup()


attributable_burden_age_adj <- attributable_burden_age_adj %>%
  filter(Population >= 1 & full_stand_popsize >= 1) %>%
  dplyr::mutate(
    mean = (mean * standard_popsize / Population) * (100000 / full_stand_popsize),
    mean = ifelse(is.nan(mean), 0, mean),
    lower = (lower * standard_popsize / Population) * (100000 / full_stand_popsize),
    lower = ifelse(is.nan(lower), 0, lower),
    upper = (upper * standard_popsize / Population) * (100000 / full_stand_popsize),
    upper = ifelse(is.nan(upper), 0, upper),
    measure2 = "age-adjusted rate",
    Population = NULL, standard_popsize = NULL
  )

group_variables <- setdiff(colnames(attributable_burden_age_adj), c("mean", "lower", "upper", "min_age", "max_age"))
attributable_burden_age_adj <- attributable_burden_age_adj %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper),
    min_age = min(min_age),
    max_age = max(max_age)
  ) %>%
  ungroup()
## --Find replace----
attributable_burden_age_adj <- attributable_burden_age_adj %>%
  mutate(
    Education = as.factor(Education),
    Education = forcats::fct_recode(Education,
      "High school graduate or lower" = "lower",
      "Some college education but no 4-year college degree" = "middle",
      "4-year college graduate or higher" = "higher",
      "666" = "666"
    )
  )
browser()
rindreplace8 <- list("Large metro" = 1, "Small-medium metro" = 2, "Non metro" = 3, "All" = 666, "Unknown" = "Unknown")
levels(attributable_burden_age_adj$rural_urban_class) <- rindreplace8


rindreplace3 <- list("All genders" = "A", "Male" = "M", "Female" = "F")
levels(attributable_burden_age_adj$Gender.Code) <- rindreplace3

rindreplace4 <- list("National Vital Statistics System" = "nvss")
levels(attributable_burden_age_adj$source) <- rindreplace4

rindreplace6 <- list("age-adjusted rate per 100,000" = "age-adjusted rate")
levels(attributable_burden_age_adj$measure2) <- rindreplace6

attributable_burden_age_adj <- attributable_burden_age_adj %>%
  unite("Ethnicity", Race, Hispanic.Origin, sep = ", ") %>%
  mutate(Ethnicity = as.factor(Ethnicity))

attributable_burden_age_adj <- attributable_burden_age_adj %>%
  mutate(
    Ethnicity = as.factor(Ethnicity),
    Ethnicity = forcats::fct_recode(Ethnicity,
      "Black American" = "Black or African American, All Origins",
      "American Indian or Alaska Native" = "American Indian or Alaska Native, All Origins",
      "Asian or Pacific Islander" = "Asian or Pacific Islander, All Origins",
      "Hispanic or Latino White" = "White, Hispanic or Latino",
      "NH White" = "White, Not Hispanic or Latino",
      "White" = "White, All Origins",
      "All, All Origins" = "All, All Origins"
    )
  )

attributable_burden_age_adj <- attributable_burden_age_adj %>%
  rename(Region = nation) %>%
  mutate(
    agr_by = "nation",
    measure3 = "value"
  )
## ----save----------

fwrite(
  attributable_burden_age_adj,
  file.path(summaryHigherDir, paste0("attributable_burden_age_adj.csv"))
)
