suppressMessages(library(dplyr, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(magrittr, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(data.table, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(tidyverse, warn.conflicts = FALSE, quietly = TRUE))
suppressMessages(library(tictoc, warn.conflicts = FALSE, quietly = TRUE))

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  dataDir <- "data"
  pop.summary.dir <- "data/12_population_summary"
  totalBurdenParsed2Dir <- "data/13_total_burden_rate"
  attr_burdenDir <- "data/14_attr_burd"
  summaryHigherDir <- "data/15_sum_higher_geog_level"
  agr_by <- "nation"
  year <- 2010
}else{
  year <- args[1]
  dataDir <- args[2]
  agr_by <- args[10]
  pop.summary.dir <- args[16]
  totalBurdenParsed2Dir <- args[17]
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
attr_burden <- lapply(files, function(file) fread(file.path(attr_burdenDir, "nvss", file))) %>% rbindlist(use.names = TRUE)

##---get county -rural urban class---
rural_urban_class_df <- read_csv("data/rural_urban_class.csv", show_col_types = FALSE)
rural_urban_class_df <- rural_urban_class_df %>%
  filter(fromYear <= year) %>%
  filter(fromYear == max(fromYear)) %>%
  select(-fromYear)

## --sum up geographic levels from county----

if (agr_by == "county") {
  attr_burden <- attr_burden %>%
    filter(measure1 == "Deaths" &
      measure2 == "age-adjusted rate")

  group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper", "min_age", "max_age"))

  attributable_burden_age_adj_over_25 <- attr_burden %>%
    filter(min_age >= 25) %>%
    group_by_at(vars(all_of(group_variables))) %>%
    summarise(
      mean = sum(mean),
      lower = sum(lower),
      upper = sum(upper)
    ) %>%
    ungroup() %>%
    mutate(min_age = 25,
           max_age = 150)

  attributable_burden_age_adj_over_65 <- attr_burden %>%
    filter(min_age >= 65) %>%
    group_by_at(vars(all_of(group_variables))) %>%
    summarise(
      mean = sum(mean),
      lower = sum(lower),
      upper = sum(upper)
    ) %>%
    ungroup() %>%
    mutate(min_age = 65,
           max_age = 150)

  attributable_burden_age_adj <- rbind(attributable_burden_age_adj_over_25, attributable_burden_age_adj_over_65)

  fwrite(attributable_burden_age_adj, summaryHigherDir)
  quit()

}
#continue if State or nation
###---- add rural urban class----
#rural_urban_class_find_replace <- set_names(rural_urban_class$rural_urban_class,
#                                            rural_urban_class$FIPS.code)
#rural_urban_class_find_replace <- as.character(rural_urban_class_find_replace)
#attr_burden$rural_urban_class <- recode(attr_burden$rural_urban_class, rural_urban_class_find_replace, .default = "Unknown")

attr_burden_with_rural_urban_class <- attr_burden %>%
  mutate() %>%
  left_join(rural_urban_class_df,
            by = c("county" ="FIPS.code"),
            na.replace = "Unknown"
  ) %>%
  mutate(rural_urban_class= rural_urban_class.y,
         rural_urban_class.x = NULL, rural_urban_class.y = NULL) %>%
  filter(rural_urban_class != "Unknown")

attr_burden <- rbind(attr_burden, attr_burden_with_rural_urban_class)
rm(attr_burden_with_rural_urban_class)

##----group out counties---
attr_burden <- attr_burden %>%
  filter(measure1 == "Deaths" &
           measure2 == "absolute number")

if (agr_by == "STATEFP") {
  attr_burden <- attr_burden %>%
    mutate(
      STATEFP = str_sub(county, 1, -4) %>%
        as.integer() %>%
        as.factor()
    )

} else if (agr_by == "nation") {
  attr_burden <- attr_burden %>%
    mutate(
      nation = "us"
    )
}
tic(paste("summed up county level estimates to ", agr_by," and age adjusted in year ", year))

group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper", "county"))
attr_burden <- attr_burden %>%
  group_by_at(vars(all_of(c(group_variables)))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup()

attr_burden_absolute_number <- attr_burden
#---read population data----
if(file.exists(file.path(pop.summary.dir, paste0("pop_", agr_by, ".csv"))) & agr_by != "county"){
  pop_summary1 <- file.path(pop.summary.dir, paste0("pop_", agr_by, ".csv")) %>%
    read.csv() %>%
    filter(Year == year)
}else{
  pop_summary1 <- NULL
}

pop_summary2 <- file.path(pop.summary.dir, agr_by, paste0("pop_sum_", year, ".csv")) %>%
  read.csv() %>%
  filter(Year == year)

if(agr_by != "county"){
  pop_summary2 <- pop_summary2 %>% filter(!(rural_urban_class == 666 & Education == 666))
}

if(agr_by == "nation"){
  pop_summary3 <- file.path(pop.summary.dir, paste0("pop_race_educ_nation.csv")) %>%
    read.csv() %>%
    filter(Year == year)
}else{
  pop_summary3 <- NULL
}

pop_summary <- rbind(pop_summary1, pop_summary2, pop_summary3) %>% distinct

pop_summary <- pop_summary %>%
  mutate_at(c("rural_urban_class", "Education"), as.factor) %>%
  mutate(source2 = NULL)

rm(pop_summary1, pop_summary2)

if (agr_by == "nation") {
  pop_summary <- pop_summary %>%
    complete(Year, nation, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
             fill = list(Population = 0)
    )%>%
    mutate_at(c("nation"), as.factor)
} else if (agr_by == "STATEFP") {
  pop_summary <- pop_summary %>%
    complete(Year, STATEFP, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
             fill = list(Population = 0)
    )%>%
    mutate_at(c("STATEFP"), as.factor)
}else if (agr_by == "county") {
  pop_summary <- pop_summary %>%
    #complete(Year, county, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
    #         fill = list(Population = 0)
    #)%>%
    mutate_at(c("county"), as.factor)

  #pop_summary <- pop_summary %>% filter(Race != "All")
}

#------ age-standartised rates-------
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

###----sum out age ----

group_variables <- setdiff(colnames(attributable_burden_age_adj), c("mean", "lower", "upper", "min_age", "max_age"))
attributable_burden_age_adj_over_25 <- attributable_burden_age_adj %>%
  filter(min_age >= 25) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup() %>%
  mutate(min_age = 25,
         max_age = 150)

attributable_burden_age_adj_over_65 <- attributable_burden_age_adj %>%
  filter(min_age >= 65) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup() %>%
  mutate(min_age = 65,
         max_age = 150)

attr_burden_absolute_number_over_25 <- attr_burden_absolute_number %>%
  filter(min_age >= 25) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup() %>%
  mutate(min_age = 25,
         max_age = 150)

attr_burden_absolute_number_over_65 <- attr_burden_absolute_number %>%
  filter(min_age >= 65) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(
    mean = sum(mean),
    lower = sum(lower),
    upper = sum(upper)
  ) %>%
  ungroup() %>%
  mutate(min_age = 65,
         max_age = 150)

attributable_burden_age_adj <- rbind(attributable_burden_age_adj_over_25, attributable_burden_age_adj_over_65, attr_burden_absolute_number_over_25, attr_burden_absolute_number_over_65)

fwrite(attributable_burden_age_adj, summaryHigherDir)
toc()
