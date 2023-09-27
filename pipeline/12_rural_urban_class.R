# Load required packages and install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat",  "tictoc", "readxl")

for (p in packages) {
  suppressMessages(library(p, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE))
}

# Set display options
options(scipen = 10000, dplyr.summarise.inform = FALSE, dplyr.join.inform = FALSE)

# Pass in arguments from the command line
args <- commandArgs(trailingOnly = TRUE)
dataDir <- args[2]
censDir <- args[8]
dataDir <- "data"
censDir <- "data/05_demog"

# Read rural and urban classification data
rural_urban_class_or <- suppressMessages(
  read_excel(file.path(dataDir, "NCHSURCodes2013.xlsx"), .name_repair = "universal") %>%
    transmute(FIPS.code, rural_urban_class = ..2013.code)
)

# Read population summary data
meta <- file.path(censDir, "meta", paste0("cens_meta_", 2010, ".csv")) %>%
  fread() %>%
  filter(Gender.Code == "A" & Race == "All" & Hispanic.Origin == "All Origins" & Education == 666)

pop.sum <- lapply(list.files(file.path(censDir, 2010)), function(file) {
  fread(file.path(censDir, 2010, file))
}) %>%
  rbindlist() %>%
  filter(variable %in% meta$variable) %>%
  mutate(FIPS.code = paste0(state, str_pad(county, 3, pad = "0")) %>% as.integer()) %>%
  group_by(FIPS.code) %>%
  summarise(pop_size = sum(pop_size))

# Read crosswalk data
crosswalk90 <- read.csv(file.path(dataDir, paste0("crosswalk_", 1990, "_2010.csv"))) %>%
  select(trtidFrom = trtid90, trtidTo = trtid10) %>%
  mutate(fromYear = 1990)

crosswalk00 <- read.csv(file.path(dataDir, paste0("crosswalk_", 2000, "_2010.csv"))) %>%
  select(trtidFrom = trtid00, trtidTo = trtid10) %>%
  mutate(fromYear = 2000)

crosswalk <- rbind(crosswalk00, crosswalk90) %>%
  transmute(
    countyFrom = trtidFrom %>% str_pad(., 11, pad = "0") %>% substr(., 0, 5) %>% as.integer(),
    countyTo = trtidTo %>% str_pad(., 11, pad = "0") %>% substr(., 0, 5) %>% as.integer(),
    fromYear
  ) %>%
  distinct()

# Further processing of crosswalk
# filter the biggest county
crosswalk <- crosswalk %>%
  left_join(pop.sum, by = c("countyTo" = "FIPS.code")) %>%
  group_by(fromYear, countyFrom) %>%
  filter(pop_size == max(pop_size)) %>%
  mutate(pop_size = NULL)

#fill in counties which were not merge with the same one
filler <- merge(
  data.frame(
    countyFrom = setdiff(rural_urban_class_or$FIPS.code, crosswalk$countyFrom),
    countyTo = setdiff(rural_urban_class_or$FIPS.code, crosswalk$countyFrom)
  ),
  data.frame(fromYear = c(1990, 2000, 2010))
)

crosswalk <- rbind(crosswalk, filler) %>% distinct()

# Test rural urban class data
test_that("rural urban class", {
  expect_false(any(is.na(crosswalk)))
  test <- crosswalk %>%
    group_by(countyFrom, fromYear) %>%
    summarise(n = n(), countyTo = list(countyTo)) %>%
    filter(n != 1)
  expect_equal(nrow(test), 0)
})

# Additional tests
test_that("rural urban class", {
  expect_false(any(is.na(rural_urban_class_or)))
  test <- rural_urban_class_or %>%
    group_by(FIPS.code) %>%
    summarise(n = n()) %>%
    filter(n != 1)
  anti_test <- anti_join(crosswalk, rural_urban_class_or, by = c("countyTo" = "FIPS.code"))
  expect_equal(nrow(anti_test), 0)
})

# Final processing of rural_urban_class data
rural_urban_class <- rural_urban_class_or %>%
  right_join(crosswalk, by = c("FIPS.code" = "countyTo")) %>%
  distinct() %>%
  mutate(FIPS.code = countyFrom, countyFrom = NULL)

rural_urban_class <- rbind(
  rural_urban_class %>% filter(!(fromYear == 2010 & FIPS.code %in% rural_urban_class_or$FIPS.code)),
  rural_urban_class_or %>% mutate(fromYear = 2010)
)

# Consolidate rural and urban classes into fewer categories
rural_urban_class <- rural_urban_class %>%
  mutate(rural_urban_class = case_when(
    rural_urban_class %in% 1:2 ~ 1,
    rural_urban_class %in% 3:4 ~ 2,
    rural_urban_class %in% 5:6 ~ 3
  ))

# Write the result to a CSV file
write.csv(rural_urban_class, file = file.path(dataDir, "rural_urban_class.csv"), row.names = FALSE)
