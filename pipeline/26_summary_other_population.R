#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat",  "tictoc", "stats", "matrixStats")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
censDir <- args[2]
dem_agrDir <- args[3]
pop.summary.dir <- args[4]
summaryDir <- args[7]

# TODO delete
if (rlang::is_empty(args)) {
  tmpDir <- "data/tmp"
  censDir <- "data/05_demog"
  dem_agrDir <- "data/06_dem.agr"
  pop.summary.dir <- "data/12_population_summary"
  summaryDir <- "data/17_summary"
}


states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  select(NAME, STATEFP)


### --- read population data-----
pop_summaryDir <- file.path(summaryDir, "pop_summary.csv")
if (file.exists(pop_summaryDir)) {
  quit()
}
tic("summarized population data")
files <- setdiff(list.files(pop.summary.dir), c("plot", "county", "pop_county.csv"))
files1 <- files[endsWith(files, ".csv")]
pop_summary1 <- lapply(files1, function(file) {
  pop_summary1 <- fread(file.path(pop.summary.dir, file))
  if (nrow(pop_summary1) == 0) {
    return(NULL)
  }

  # make compatible
  possible_agr_by <- c("nation", "county", "STATEFP")
  agr_by_pos <- sapply(possible_agr_by, function(agr_by) grepl(agr_by, file, fixed = TRUE))
  agr_by <- possible_agr_by[agr_by_pos]

  colnames(pop_summary1)[colnames(pop_summary1) == agr_by] <- "Region"
  # pop_summary1 <- pop_summary1 %>% rename("Region":=!!agr_by)
  pop_summary1 <- pop_summary1 %>% tibble::add_column(agr_by = agr_by)
})
pop_summary1 <- pop_summary1 %>% rbindlist(use.name = TRUE, fill = TRUE)

agr_bys <- files[!endsWith(files, ".csv")]
agr_bys <- setdiff(agr_bys, "county")
pop_summary2 <- lapply(agr_bys, function(agr_by) {
  files2 <- list.files(file.path(pop.summary.dir, agr_by))
  pop_summary2 <- lapply(files2, function(file) fread(file.path(pop.summary.dir, agr_by, file))) %>% rbindlist(use.name = TRUE, fill = TRUE)

  if (nrow(pop_summary2) == 0) {
    return(NULL)
  }
  # make compatible
  colnames(pop_summary2)[colnames(pop_summary2) == agr_by] <- "Region"
  # pop_summary2 <- pop_summary2 %>% rename("Region":=!!agr_by)
  pop_summary2 <- pop_summary2 %>% tibble::add_column(agr_by = agr_by)
}) %>% rbindlist(use.name = TRUE, fill = TRUE)
# pop_summary2 <- pop_summary2 %>% filter(Education != 666) #TODO

pop_summary <- rbind(pop_summary1, pop_summary2)
rm(pop_summary1, pop_summary2)

pop_summary <- pop_summary %>%
  filter(min_age >= 25) %>%
  dplyr::group_by_at(vars(all_of(setdiff(colnames(pop_summary), c("Population", "min_age", "max_age"))))) %>%
  dplyr::summarize(Population = sum(Population)) %>%
  mutate(min_age = 25, max_age = 150)
### ---find and replace----
pop_summary <- pop_summary %>% mutate_at(
  setdiff(colnames(pop_summary), c("Population")),
  as.factor
)
rindreplace1 <- data.frame(
  agr_by = c("nation", rep("STATEFP", nrow(states))),
  RegionFrom = c("us", states$STATEFP),
  RegionTo = c("United States", states$NAME)
)
pop_summary <- pop_summary %>%
  left_join(rindreplace1, by = c("Region" = "RegionFrom", "agr_by")) %>%
  mutate(
    Region = coalesce(RegionTo, Region),
    RegionTo = NULL
  )

rindreplace2 <- list(
  "High school graduate or lower" = "lower",
  "Some college education but no 4-year college degree" = "middle",
  "4-year college graduate or higher" = "higher",
  "666" = "666"
)
levels(pop_summary$Education) <- rindreplace2

rindreplace3 <- list("All genders" = "A", "Male" = "M", "Female" = "F")
levels(pop_summary$Gender.Code) <- rindreplace3

rindreplace4 <- list("Official Bridged-Race Population Estimates" = "CDC", "Own Interpolation" = "Census")
levels(pop_summary$source2) <- rindreplace4

pop_summary <- pop_summary %>%
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

levels(pop_summary$Ethnicity) <- rindreplace7


# rindreplace8 <- c("large central metro" = 1, "large fringe metro" = 2, "medium metro" = 3, "small metro" = 4, "micropolitan" = 5, "non-core" = 6,"All" = 666,"Unknown" = "Unknown")
#  rindreplace8 <- c("1" ="large central metro" ,"2" = "large fringe metro" ,"3"= "medium metro" ,"4"= "small metro","5" = "micropolitan" ,
# "6"= "non-core","666"= "All" ,"Unknown" = "Unknown")
# rindreplace8 <- c("large metro" = 1, "small-medium metro" = 2,  "non metro" = 3, "All" = 666,"Unknown" = "Unknown")
# levels(pop_summary$rural_urban_class) <- rindreplace8

rindreplace8 <- c("1" = "Large metro", "2" = "Small-medium metro", "3" = "Non metro", "666" = "All", "Unknown" = "Unknown")

pop_summary <- pop_summary %>% mutate(rural_urban_class = sapply(rural_urban_class, function(x) rindreplace8[[x]]))


fwrite(pop_summary, pop_summaryDir)
rm(rindreplace1, rindreplace2, rindreplace3)
toc()
