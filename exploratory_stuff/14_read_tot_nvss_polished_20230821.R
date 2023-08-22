#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 08/25/2021
# Purpose: read and parse mortality counts
#
#***************************************************************************
#*

# clear memory
# rm(list = ls(all = TRUE))

# load packages, install if missing
library(magrittr)
library(data.table)
library(tictoc)
#library(foreach)
#library(doParallel)
library(dplyr)
library(stringr)
require(narcan)
library(readr)
require(tidyr)

pkgload::load_all()
#NCORES <- 3

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

### ---get arguments----

args <- commandArgs(trailingOnly = T)
if (rlang::is_empty(args)) {
  agr_by <- "county"
  year <- 2002

  totalBurdenDir <- "data/08_total_burden/nvss"
  totalBurdenDir <- "/share/pi/mkiang/mcod_restricted"
  totalBurdenParsedDir <- "data/09_total_burden_parsed"
  #totalBurdenParsedDir <- "/share/pi/mkiang/dfridljand_air_pollution/PM2.5-attributable-mortality-analysis-private/data/09_total_burden_parsed"

} else {
  year <- args[1]
  agr_by <- args[10]
}

years <- year
agr_bys <- agr_by
#### ----- Change paths here!---
# Where the files are stored
# totalBurdenDir <- "./raw_restricted_data"
# totalBurdenDir <- "data/raw_restricted_fake"
totalBurdenDir <- "data/08_total_burden/nvss"
totalBurdenDir <- "/share/pi/mkiang/mcod_restricted"
# Where the parsed files should be stored
# totalBurdenParsedDir <- "./Transfer_for_daniel"
totalBurdenParsedDir <- "data/09_total_burden_parsed2"

# totalBurdenDir <- "/Volumes/fridljand/R/HIGH/raw_restricted_fake"
# Where the parsed files should be stored
# totalBurdenParsedDir <- "/Volumes/fridljand/R/HIGH/raw_restricted_fake"

dir.create(sprintf("%s/logs", totalBurdenParsedDir), recursive = TRUE, showWarnings = FALSE)


# if all totalBurdenParsedDirX already exist, quit
#### ----- ---
file_list <- list.files(totalBurdenDir)


# causes <- read.csv("https://raw.github.com/FridljDa/pm25_inequality/master/data/09_total_burden_parsed/causes.csv")
# findreplace <- read.csv("https://raw.github.com/FridljDa/pm25_inequality/master/data/09_total_burden_parsed/findreplace.csv")
findreplace <- read_csv("data/09_total_burden_parsed/findreplace.csv", show_col_types = FALSE)
causes <- read_csv("data/09_total_burden_parsed/causes.csv", show_col_types = FALSE)


#### ----- loop over everything ---
# doParallel::registerDoParallel(cores = NCORES)
# foreach::foreach(year = years, .inorder = FALSE) %dopar% {

totalBurdenDirX <- file.path(totalBurdenDir, file_list[grepl(year, file_list)])
## ----- read total burden ---------
total_burden <- narcan:::.import_restricted_data(totalBurdenDirX, year = year) # , fix_states = FALSE
if(FALSE){
  total_burden <- total_burden %>% sample_n(2000)
}
# total_burden <- narcan:::.import_restricted_data(totalBurdenDirX, year = year)
# filter out rows where everything is just na
#total_burden <- data.table::fread(cmd = paste("unzip -p", totalBurdenDirX))

total_burden <- total_burden %>%
  # filter(rowSums(is.na(.)) != ncol(.))
  filter(!is.na(year))

## Open log -- assume file import went fine.
# sink(sprintf("%s/logs/log_%s.txt", totalBurdenParsedDir, Sys.getpid()), append = TRUE)
print(sprintf("Starting %s: %s", year, basename(totalBurdenDirX)))

causes_year <- causes %>% filter(Year == year)
numberDeaths <- nrow(total_burden)


total_burden_agr_by <- total_burden

totalBurdenParsedDirX <- file.path(totalBurdenParsedDir, agr_by, "nvss")
dir.create(totalBurdenParsedDirX, recursive = T, showWarnings = F)
totalBurdenParsedDirX <- file.path(
  totalBurdenParsedDirX,
  paste0("total_burden_nvss_", year, ".csv")
)

if (file.exists(totalBurdenParsedDirX)) {
  #next
  quit()
}

# if (!file.exists(totalBurdenParsedDirX)) {
tic(paste("read", year, "total burden data"), quiet = FALSE)
print(paste("read", year, "total burden data"))

if (year %in% 1990:1995) {
  selectcolumns <- c(
    "Year" = "datayear",
    "label_cause" = "ucod", # record_1/enum_1
    # "Education" = "educ", # 52-53
    "Gender.Code" = "sex", # 59
    "Race" = "race", # 60
    "min_age" = "age", # 64, Single Year
    "max_age" = "age", # 64
    "Hispanic.Origin" = "hispanic", # 80 - 81
    "interested_state" = "staters"
  )
} else if (year %in% 1996:2002) {
  selectcolumns <- c(
    "Year" = "year",
    "label_cause" = "ucod", # record_1/enum_1
    # "Education" = "educ", # 52-53
    "Gender.Code" = "sex", # 59
    "Race" = "race", # 60
    "min_age" = "age", # 64, Single Year
    "max_age" = "age", # 64
    "Hispanic.Origin" = "hispanic", # 80 - 81
    "interested_state" = "staters"
  )
} else if (year %in% 2003:2005) {
  selectcolumns <- c(
    "Year" = "year",
    "label_cause" = "ucod", # record_1/enum_1
    # "Education" = "educ", # 52-53
    "Gender.Code" = "sex", # 59
    "Race" = "race", # 60
    "min_age" = "age", # 64, Single Year
    "max_age" = "age",
    "Hispanic.Origin" = "hspanicr", # 80 - 81
    "interested_state" = "staters"
  )
} else if (year %in% 2006:2008) {
  selectcolumns <- c(
    "Year" = "year",
    "label_cause" = "ucod", # record_1/enum_1
    "Gender.Code" = "sex", # 59
    "Race" = "race", # 60
    "min_age" = "age", # 64, Single Year
    "max_age" = "age",
    "Hispanic.Origin" = "hspanicr", # 80 - 81
    "interested_state" = "staters"
  )
} else if (year %in% 2009:2016) {
  # For the years 2009:2016 we have population estimates by Education
  selectcolumns <- c(
    "Year" = "year",
    "label_cause" = "ucod", # record_1/enum_1
    "Education1989" = "educ89",
    "Education2003" = "educ", # 52-53
    "Gender.Code" = "sex", # 59
    "Race" = "race", # 60
    "min_age" = "age", # 64, Single Year
    "max_age" = "age",
    "Hispanic.Origin" = "hspanicr", # 80 - 81
    "interested_state" = "staters"
  )
}

# initialize staters column if missing
if (!"staters" %in% colnames(total_burden_agr_by)) total_burden_agr_by$staters <- NA

if ("fipsctyr" %in% colnames(total_burden_agr_by)) {
  selectcolumns <- c(selectcolumns, "rural_urban_class" = "fipsctyr", "svi_bin" = "fipsctyr")
  is_missing <- function(x) {
    return(is.na(x) | x == 0 | grepl("ZZZ$", x))
  }

  total_burden_agr_by <- total_burden_agr_by %>%
    mutate(
      fipsctyr = replace_missing_vectors(fipsctyr, fipsctyo, is_missing, "County FIPS"),
      staters = replace_missing_vectors(staters, str_sub(countyrs, 1, -4), is_missing, "State")
    )

  rm(is_missing)
} else if (!("fipsctyr" %in% colnames(total_burden)) & "countyrs" %in% colnames(total_burden)) {
  # Append "rural_urban_class" column to selectcolumns
  selectcolumns <- c(selectcolumns, "rural_urban_class" = "countyrs", "svi_bin" = "countyrs")

  is_missing <- function(x) {
    return(is.na(x) | x == 0)
  }

  total_burden_agr_by <- total_burden_agr_by %>%
    mutate(
      countyrs = replace_missing_vectors(countyrs, countyoc, is_missing, "County FIPS"),
      staters = replace_missing_vectors(staters, str_sub(countyrs, 1, -4), is_missing, "State")
    )

  rm(is_missing)
} else {
  selectcolumns <- c(selectcolumns, "rural_urban_class" = "rural_urban_class", "svi_bin" = "svi_bin")
  total_burden$rural_urban_class <- NA
  total_burden$svi_bin <- NA
}

if (agr_by == "nation") {
  total_burden_agr_by <- total_burden_agr_by %>% tibble::add_column(nation = "us")
  selectcolumns <- c(selectcolumns, "nation" = "nation")
} else if (agr_by == "STATEFP") {
  selectcolumns <- c(selectcolumns, "STATEFP" = "staters") # residence, not occurrence
} else if (agr_by == "county") {
  if ("fipsctyr" %in% colnames(total_burden_agr_by)) {
    selectcolumns <- c(selectcolumns, "county" = "fipsctyr")
  } else if (!("fipsctyr" %in% colnames(total_burden_agr_by)) & "countyrs" %in% colnames(total_burden_agr_by)) {
    selectcolumns <- c(selectcolumns, "county" = "countyrs")
  }
}

# https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data
total_burden_agr_by <- total_burden_agr_by %>% select(all_of(selectcolumns))


#Don't want rows with too many NA values
total_burden_agr_by <- total_burden_agr_by %>%
  filter(rowSums(is.na(.)) < 4)
# total_burden_agr_by <- total_burden_agr_by[!apply(is.na(total_burden_agr_by), 1, all),]
## Test print
print(total_burden_agr_by, n = 5)

#---------find and replace stuff--------
findreplace_agr_by_year <- findreplace %>%
  filter(
    replacecolumns %in% colnames(total_burden_agr_by),
    Year == year
  )
total_burden_agr_by <- replace_values(total_burden_agr_by, findreplace_agr_by_year)

### --- deal with education---
if ("Education2003" %in% colnames(total_burden_agr_by)) {
  is_missing <- function(x) {
    return(x == "101")
  }

  total_burden_agr_by <- total_burden_agr_by %>%
    mutate(
      Education = replace_missing_vectors(Education2003, Education1989, is_missing, "Education"),
      Education = replace_na(Education, "Unknown"),
      Education1989 = NULL, Education2003 = NULL
    )

  rm(is_missing)
}

total_burden_agr_by <- total_burden_agr_by %>%
  group_by_at(colnames(total_burden_agr_by)) %>%
  summarise(Deaths = n())

# print(1 - sum(as.integer(total_burden_agr_by$interested_state)) / nrow(total_burden_agr_by))
## --- seperate stuff----
# add all rural_urban_class
#TODO
#total_burden_agr_by_all_urb <- total_burden_agr_by %>%
#  group_by_at(setdiff(colnames(total_burden_agr_by), c("rural_urban_class", "Deaths"))) %>%
#  summarise(Deaths = sum(Deaths)) %>%
#  mutate(rural_urban_class = as.factor(666))

#total_burden_agr_by_all <- total_burden_agr_by %>%
#  group_by_at(setdiff(colnames(total_burden_agr_by), c("rural_urban_class", "svi_bin", "Deaths"))) %>%
#  summarise(Deaths = sum(Deaths)) %>%
#  mutate(rural_urban_class = as.factor(666), svi_bin = as.factor(666))

#total_burden_agr_by_svi <- total_burden_agr_by %>%
#  group_by_at(setdiff(colnames(total_burden_agr_by), c("svi_bin", "Deaths"))) %>%
#  summarise(Deaths = sum(Deaths)) %>%
#  mutate(svi_bin = as.factor(666))

#TODO this does not make sense

#if (agr_by == "county") {
#  total_burden_agr_by <- total_burden_agr_by_all %>% distinct()
#} else {
#  total_burden_agr_by <- rbind(total_burden_agr_by_all_urb,
#                               total_burden_agr_by_all,
#                               total_burden_agr_by_svi) %>% distinct()
#}

#rm(total_burden_agr_by_all_urb)

# add Hispanic Origin All Origins
total_burden_agr_by_all_his <- group_summarize_add_column(total_burden_agr_by,
  column = "Hispanic.Origin",
  new_col_value = "All Origins"
)

total_burden_agr_by <- rbind(total_burden_agr_by, total_burden_agr_by_all_his) %>% distinct()
rm(total_burden_agr_by_all_his)

# add Gender A
total_burden_agr_by_all_gend <- group_summarize_add_column(total_burden_agr_by,
  column = "Gender.Code",
  new_col_value = "A"
)

total_burden_agr_by <- rbind(total_burden_agr_by, total_burden_agr_by_all_gend) %>% distinct()
rm(total_burden_agr_by_all_gend)

#--- add all-cause rows---
total_burden_agr_by_all <- total_burden_agr_by %>%
  group_summarize_add_column(
    column = "label_cause",
    new_col_value = "all-cause"
  ) %>%
  mutate(
    attr = "overall"
  )


causes_year2 <- causes_year %>%
  group_by(to) %>%
  summarise(from = list(from))

total_burden_agr_by_cause <- total_burden_agr_by %>% mutate(label_cause = substring(label_cause, 1, 3))
total_burden_agr_by_cause <- apply(causes_year2, 1, function(cause) {
  label_cause1 <- cause$to
  icd10_cause <- cause$from %>% unlist()
  total_burden_agr_by_cause <- total_burden_agr_by_cause %>%
    filter(label_cause %in% icd10_cause) %>%
    group_by_at(setdiff(colnames(total_burden_agr_by), c("label_cause", "Deaths"))) %>%
    summarise(Deaths = sum(Deaths)) %>%
    mutate(
      label_cause = label_cause1,
      attr = "total"
    )
}) %>% rbindlist()

total_burden_agr_by <- rbind(total_burden_agr_by_all, total_burden_agr_by_cause) %>% distinct()
rm(total_burden_agr_by_all, total_burden_agr_by_cause)

# seperate education, add "All Education"
total_burden_agr_by_race <- total_burden_agr_by %>%
  group_summarize_add_column(
    column = "Education",
    new_col_value = as.factor(666)
  )

total_burden_agr_by_all <- total_burden_agr_by %>%
  filter(Hispanic.Origin == "All Origins") %>%
  group_by_at(setdiff(colnames(total_burden_agr_by), c("Race", "Education", "Deaths"))) %>%
  summarise(Deaths = sum(Deaths)) %>%
  mutate(Race = "All", Education = as.factor(666))

# For the years 2009:2016 we have population estimates by Education
if ("Education" %in% colnames(total_burden_agr_by)) {
  total_burden_agr_by_educ <- total_burden_agr_by %>%
    filter(Hispanic.Origin == "All Origins") %>%
    group_summarize_add_column(
      column = "Race",
      new_col_value = "All"
    ) %>%
    mutate(Education = Education %>% as.factor())

  total_burden_agr_by <- rbind(total_burden_agr_by_race, total_burden_agr_by_educ, total_burden_agr_by_all, total_burden_agr_by) %>% distinct()
  rm(total_burden_agr_by_educ)
} else {
  total_burden_agr_by <- rbind(total_burden_agr_by_race, total_burden_agr_by_all) %>% distinct()
}

rm(total_burden_agr_by_race, total_burden_agr_by_all)
#----test----
total_burden_agr_by <- total_burden_agr_by %>%
  as.data.frame() %>%
  ungroup()

#------filter ------
total_burden_agr_by <- total_burden_agr_by %>% distinct()

total_burden_agr_by <- total_burden_agr_by %>% filter(Gender.Code == "A")
total_burden_agr_by <- total_burden_agr_by %>%
  filter(interested_state == 1) %>%
  mutate(interested_state = NULL)

if ("STATEFP" %in% colnames(total_burden_agr_by)) {
  total_burden_agr_by <- total_burden_agr_by %>%
    filter(STATEFP != "oth")
}

total_burden_agr_by <- total_burden_agr_by %>% tidyr::unite("Ethnicity", c("Race", "Hispanic.Origin"), sep = ", ", remove = F)

# if (agr_by == "nation") {
ethnicities_with_education <- c(
  "White, All Origins",
  "White, Not Hispanic or Latino",
  "White, Hispanic or Latino",
  "Black or African American, All Origins",
  "American Indian or Alaska Native, All Origins",
  "Asian or Pacific Islander, All Origins",
  "All, All Origins"
)
# } else {
#  ethnicities_with_education <- c("All, All Origins")
# }
total_burden_agr_by <- total_burden_agr_by %>%
  filter(Education == 666 | Ethnicity %in% ethnicities_with_education)

interested_ethnicities <- c(
  "Black or African American, All Origins",
  "Asian or Pacific Islander, All Origins",
  "American Indian or Alaska Native, All Origins",
  "All, All Origins"
)
if (year %in% 1990:1999) interested_ethnicities <- c(interested_ethnicities, "White, All Origins")
if (year == 2000) interested_ethnicities <- c(interested_ethnicities, "White, All Origins", "White, Not Hispanic or Latino", "White, Hispanic or Latino")
if (year %in% 2001:2016) interested_ethnicities <- c(interested_ethnicities, "White, Not Hispanic or Latino", "White, Hispanic or Latino")

total_burden_agr_by <- total_burden_agr_by %>%
  filter(Ethnicity %in% interested_ethnicities) %>%
  mutate(Ethnicity = NULL)

# Only considering age above 25
suppressWarnings(
  total_burden_agr_by <- total_burden_agr_by %>%
    mutate(min_age = as.numeric(min_age)) %>%
    filter(is.na(min_age) | min_age >= 25) %>%
    mutate(
      min_age = as.character(min_age),
      min_age = replace_na(min_age, "Unknown")
    )
)

#----measure suppression---
numberDeaths_afterfiltering <- total_burden_agr_by %>%
  filter(Gender.Code == "A" & Race == "All" & rural_urban_class == 666 & label_cause == "all-cause" & Education == 666)
numberDeaths_afterfiltering <- sum(numberDeaths_afterfiltering$Deaths)

suppressed_deaths <- total_burden_agr_by %>%
  filter(!(Hispanic.Origin != "Unknown" &
    min_age != "Unknown" &
    Education != "Unknown" &
    rural_urban_class != "Unknown")) %>%
  filter(label_cause == "all-cause") %>%
  group_by_at(setdiff(colnames(total_burden_agr_by), c("min_age", "max_age", "Deaths"))) %>%
  summarize(Deaths = sum(Deaths)) %>%
  ungroup() # %>%
# filter((Race == "All" &Hispanic.Origin == "All Origins" & rural_urban_class == 666) |
#         (Race == "All" &Hispanic.Origin == "All Origins"& Education == 666 )|
#         (rural_urban_class ==  666 & Education == 666))
# filter((Race == "All" &Hispanic.Origin == "All Origins" & rural_urban_class == 666 & Education == "Unknown") |
#         (Race == "All" &Hispanic.Origin == "All Origins"& Education == 666 & rural_urban_class == "Unknown")|
#         (rural_urban_class ==  666 & Education == 666 & (Race == "Unknown"| Hispanic.Origin == "Unknown") ))
# filter((rural_urban_class ==  666 & Education == 666) | (rural_urban_class ==  666 & Education == 666) )

suppressed_deaths <- suppressed_deaths %>%
  mutate(
    grouped1 = case_when(
      Race == "All" & Hispanic.Origin == "All Origins" & Education == 666 & rural_urban_class == 666 ~ "All",
      Race == "All" & Hispanic.Origin == "All Origins" & Education == 666 ~ "Urbanicity",
      Race == "All" & Hispanic.Origin == "All Origins" & Education != 666 ~ "Education",
      !(Race == "All" & Hispanic.Origin == "All Origins") & Education == 666 ~ "Ethnicity",
      TRUE ~ "none"
    ),
    grouped2 = case_when(
      rural_urban_class == 666 ~ "All Urbanicity",
      rural_urban_class != 666 ~ "Not all Urbanicity",
      TRUE ~ "none"
    ),
  )

suppressed_deaths <- suppressed_deaths %>%
  group_by(Year, grouped1, grouped2) %>%
  summarise(Deaths = sum(Deaths))

suppressed_deaths <- suppressed_deaths %>%
  mutate(percent_deleted_deaths = 100 * Deaths / numberDeaths_afterfiltering) %>%
  # select(Year, Race, Hispanic.Origin, rural_urban_class, Education, Deaths, prop) %>%
  as.data.frame()
print(paste("In", year, "rows in the mortality counts were excluded due to missing entries:"))
print(suppressed_deaths)
print(paste0("(total number of deaths considered: ", numberDeaths_afterfiltering, ")"))

total_burden_agr_by <- total_burden_agr_by %>%
  filter(Hispanic.Origin != "Unknown" &
    # Race != "Guama" &
    min_age != "Unknown" &
    Education != "Unknown" &
    rural_urban_class != "Unknown" &
    svi_bin != "Unknown") %>%
  mutate(min_age = as.numeric(min_age), max_age = as.numeric(max_age))

#---write csv---
table(total_burden_agr_by$rural_urban_class, total_burden_agr_by$svi_bin)

total_burden_agr_by <- total_burden_agr_by %>% tibble::add_column(source = "nvss")
fwrite(total_burden_agr_by, totalBurdenParsedDirX)
# fwrite(suppressed_deaths, totalBurdenParsedDirX)
toc()
tic.log()
## Close log
cat("\n\n")
# sink()
# }
