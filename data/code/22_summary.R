#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr",  "magrittr", "data.table", "testthat",  "tictoc", "viridis")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

tmpDir <- args[1]
totalBurdenParsed2Dir <- args[5]
attr_burdenDir <- args[6]
summaryDir <- args[7]

if (rlang::is_empty(args)) {
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/13_total_burden_rate"
  attr_burdenDir <- "/Users/default/Desktop/paper2021/data/13_attr_burd"
  summaryDir <- "/Users/default/Desktop/paper2021/data/15_summary"

   #tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
   #totalBurdenParsed2Dir <-"C:/Users/Daniel/Desktop/paper2021/data/13_total_burden_rate"
    #attr_burdenDir <- "C:/Users/Daniel/Desktop/paper2021/data/13_attr_burd"
  # summaryDir <- "C:/Users/Daniel/Desktop/paper2021/data/15_summary"
  
  tmpDir <-  "data/tmp"
  totalBurdenParsed2Dir <-  "data/13_total_burden_rate"
  attr_burdenDir <-  "data/14_attr_burd"
  summaryDir <-  "data/15_summary"
}

states <- file.path(tmpDir, "states.csv") %>%
  read.csv() %>%
  select(NAME, STATEFP)

tic("summarized all burden and attributable burden data")
#agr_bys <- list.files(attr_burdenDir)
agr_bys <- c("STATEFP", "nation") #TODO
#agr_bys <- c("nation","county") #TODO
#agr_bys <- setdiff(list.files(attr_burdenDir),"county")
## --- read attr burden----
attr_burden <- lapply(agr_bys, function(agr_by) {
  sources <- list.files(file.path(attr_burdenDir, agr_by))
  attr_burden <- lapply(sources, function(source) {
    files <- list.files(file.path(attr_burdenDir, agr_by, source))
    attr_burden <- lapply(files, function(file) fread(file.path(attr_burdenDir, agr_by, source, file))) %>% rbindlist(use.names = TRUE)
  }) %>% rbindlist(use.names = TRUE)

  # make compatible
  attr_burden <- attr_burden %>% rename("Region" := !!agr_by)
  attr_burden <- attr_burden %>% tibble::add_column(agr_by = agr_by)
  #attr_burden <- attr_burden %>% filter(scenario == "A") # TODO delete
  if (agr_by == "county"){
    attr_burden <- attr_burden %>% filter(measure2 %in% c("absolute number","age-adjusted rate") &
                                            scenario == "real") # TODO delete
  } 
  return(attr_burden)
}) %>%
  rbindlist(use.names = TRUE) %>%
  as.data.frame()

## --- read all burden----
all_burden <- lapply(agr_bys, function(agr_by) {
  sources <- list.files(file.path(totalBurdenParsed2Dir, agr_by))
  all_burden <- lapply(sources, function(source) {
    files <- list.files(file.path(totalBurdenParsed2Dir, agr_by, source))
    all_burden <- lapply(files, function(file) fread(file.path(totalBurdenParsed2Dir, agr_by, source, file))) %>% rbindlist(use.names = TRUE)

    all_burden <- all_burden %>% filter(label_cause == "all-cause")
  }) %>% rbindlist(use.names = TRUE)

  # make compatible
  all_burden <- all_burden %>% rename("Region" := !!agr_by)
  all_burden <- all_burden %>% tibble::add_column(agr_by = agr_by)
  #if (agr_by == "county") all_burden <- all_burden %>% filter(measure2 == "age-adjusted rate") # TODO delete
  return(all_burden)
}) %>%
  rbindlist(use.names = TRUE) %>%
  as.data.frame()


group_variables <- setdiff(colnames(attr_burden), c("lower", "mean", "upper", "method", "min_age", "max_age", "scenario"))
# group_variables <- setdiff(colnames(all_burden), c("min_age", "max_age", "value"))
# setdiff(group_variables2, group_variables)

all_burden <- all_burden %>%
  group_by_at(vars(all_of(c(group_variables)))) %>%
  summarise(
    overall_value = sum(value) # ,
    # min_age = min(min_age), max_age = max(max_age)
  ) %>%
  mutate(label_cause = NULL)

attr_burden <- attr_burden %>% mutate_at(
  setdiff(colnames(attr_burden), c("mean", "lower", "upper", "Region")),
  as.factor
)

all_burden <- all_burden %>% mutate_at(
  setdiff(colnames(all_burden), c("overall_value", "Region")),
  as.factor
)

print("hank1")
attr_burden <- attr_burden %>% mutate_at(
  "Region",
  as.character
)
print("hank2")
all_burden <- all_burden %>% mutate_at(
  "Region",
  as.character
)
print("hank3")
# nrow(attr_burden) / nrow(all_burden)
### ----- add proportion ---

# add "prop. of overall burden", "prop. of total burden"
# join everything
attr_burden_prop <- attr_burden %>% left_join(all_burden, by = setdiff(colnames(all_burden), c("overall_value", "attr")))
# attr_burden_prop <- attr_burden %>% inner_join(all_burden, by = setdiff(colnames(all_burden), c("overall_value", "attr")))

# calculations
attr_burden_prop <- attr_burden_prop %>%
  mutate(
    mean = coalesce(100 * mean / overall_value, 0),
    lower = coalesce(100 * lower / overall_value, 0),
    upper = coalesce(100 * upper / overall_value, 0),
    measure3 = case_when(attr.y == "overall" ~ "prop. of overall burden", TRUE ~ "prop. of total burden"),
    overall_value = NULL, attr.x = NULL, attr.y = NULL,
    attr = "attributable"
  )

test_that(" basic checks", {
  attr_burden_prop_dupl <- all_burden %>% select(setdiff(colnames(all_burden), c("value", "label_cause")))
  attr_burden_prop_dupl <- attr_burden_prop_dupl[duplicated(attr_burden_prop_dupl), ]
  expect_equal(nrow(attr_burden_prop_dupl), 0)
  #if (nrow(attr_burden_prop_dupl) > 0) browser()

  test1 <- attr_burden %>% anti_join(all_burden, by = setdiff(colnames(all_burden), c("overall_value", "attr")))
  #expect_equal(0, nrow(test1)) #TODO

  test <- attr_burden[rowSums(is.na(attr_burden)) > 0, ]
  expect_false(any(is.na(attr_burden)))
  new_DF <- attr_burden_prop[rowSums(is.na(attr_burden_prop)) > 0, ]

  expect_false(any(is.na(attr_burden_prop)))
  expect_false(any(is.na(all_burden)))
})

# add proportion of disparity
attr_burden_disp1 <- inner_join(
  all_burden %>% filter(attr == "overall" & !(Race == "Black or African American" & Hispanic.Origin == "All Origins")),
  all_burden %>% filter(attr == "overall" & (Race == "Black or African American" & Hispanic.Origin == "All Origins")),
  by = setdiff(colnames(all_burden), c("Race", "Hispanic.Origin", "overall_value"))
)

attr_burden_disp2 <- inner_join(
  attr_burden %>% filter(!(Race == "Black or African American" & Hispanic.Origin == "All Origins")),
  attr_burden %>% filter((Race == "Black or African American" & Hispanic.Origin == "All Origins")),
  by = setdiff(colnames(attr_burden), c("Race", "Hispanic.Origin", "lower", "mean", "upper"))
)

attr_burden_disp3 <- inner_join(
  attr_burden_disp1,
  attr_burden_disp2,
  by = setdiff(colnames(attr_burden_disp1), c("attr", "overall_value.x", "overall_value.y"))
)

attr_burden_disp3 <- attr_burden_disp3 %>% mutate(
  mean = 100 * (mean.x - mean.y) / (overall_value.x - overall_value.y),
  lower = mean, upper = mean,
  attr = "attributable", measure3 = "proportion of disparity to Black or African American attributable", # "prop. of disp.",#
  Race = Race.x, Hispanic.Origin = Hispanic.Origin.x,
  Race.x = NULL, Race.y = NULL, Hispanic.Origin.x = NULL, Hispanic.Origin.y = NULL,
  overall_value.x = NULL, overall_value.y = NULL, mean.x = NULL, mean.y = NULL,
  lower.x = NULL, lower.y = NULL, upper.x = NULL, upper.y = NULL,
  attr.x = NULL, attr.y = NULL
)

attr_burden_disp4 <- inner_join(all_burden %>% filter(attr == "overall" & Education != "lower"),
  all_burden %>% filter(attr == "overall" & Education == "lower"),
  by = setdiff(colnames(all_burden), c("Education", "overall_value"))
)

attr_burden_disp5 <- inner_join(attr_burden %>% filter(Education != "lower"),
  attr_burden %>% filter(Education == "lower"),
  by = setdiff(colnames(attr_burden), c("Education", "lower", "mean", "upper"))
)

attr_burden_disp6 <- inner_join(
  attr_burden_disp4,
  attr_burden_disp5,
  by = setdiff(colnames(attr_burden_disp4), c("attr", "overall_value.x", "overall_value.y"))
  # by = c("Education.x", "Education.y", "Race", "Hispanic.Origin","Year", "Gender.Code", "Region", "measure1", "measure2", "source", "agr_by")
)

attr_burden_disp6 <- attr_burden_disp6 %>% mutate(
  mean = 100 * (mean.x - mean.y) / (overall_value.x - overall_value.y),
  lower = mean, upper = mean,
  attr = "attributable", measure3 = "proportion of disparity to lower educational attainment",
  Education = Education.x,
  Education.x = NULL, Education.y = NULL,
  overall_value.x = NULL, overall_value.y = NULL, mean.x = NULL, mean.y = NULL,
  lower.x = NULL, lower.y = NULL, upper.x = NULL, upper.y = NULL,
  attr.x = NULL, attr.y = NULL
)
#rindreplace8 <- list("large metro" = 1, "small-medium metro" = 2, "non metro" = 3, "All" = 666, "Unknown" = "Unknown")
attr_burden_disp7 <- inner_join(all_burden %>% filter(attr == "overall" & rural_urban_class != 1),
  all_burden %>% filter(attr == "overall" & rural_urban_class == 1),
  by = setdiff(colnames(all_burden), c("rural_urban_class", "overall_value"))
)

attr_burden_disp8 <- inner_join(attr_burden %>% filter(rural_urban_class != 1),
  attr_burden %>% filter(rural_urban_class == 1),
  by = setdiff(colnames(attr_burden), c("rural_urban_class", "lower", "mean", "upper"))
)

attr_burden_disp9 <- inner_join(
  attr_burden_disp7,
  attr_burden_disp8,
  by = setdiff(colnames(attr_burden_disp7), c("attr", "overall_value.x", "overall_value.y"))
)

attr_burden_disp9 <- attr_burden_disp9 %>% mutate(
  mean = 100 * (mean.x - mean.y) / (overall_value.x - overall_value.y),
  lower = mean, upper = mean,
  attr = "attributable", measure3 = "proportion of disparity to large metro",
  rural_urban_class = rural_urban_class.x,
  rural_urban_class.x = NULL, rural_urban_class.y = NULL,
  overall_value.x = NULL, overall_value.y = NULL, mean.x = NULL, mean.y = NULL,
  lower.x = NULL, lower.y = NULL, upper.x = NULL, upper.y = NULL,
  attr.x = NULL, attr.y = NULL
)

if(FALSE){
  
  attr_burden_disp10 <- inner_join(all_burden %>% filter(attr == "overall" & rural_urban_class == 666 & Education == 666 & Race == "All" & Hispanic.Origin == "All Origins"),
                                  all_burden %>% filter(attr == "overall" & !(rural_urban_class == 666 & Education == 666 & Race == "All" & Hispanic.Origin == "All Origins")),
                                  by = setdiff(colnames(all_burden), c("rural_urban_class","Education","Race","Hispanic.Origin", "overall_value"))
  )
  
  attr_burden_disp11 <- inner_join(attr_burden %>% filter(rural_urban_class == 666 & Education == 666 & Race == "All" & Hispanic.Origin == "All Origins"),
                                  attr_burden %>% filter(!(rural_urban_class == 666 & Education == 666 & Race == "All" & Hispanic.Origin == "All Origins")),
                                  by = setdiff(colnames(attr_burden), c("rural_urban_class","Education","Race","Hispanic.Origin", "lower", "mean", "upper"))
  )
  
  attr_burden_disp12 <- inner_join(
    attr_burden_disp10,
    attr_burden_disp11,
    by = setdiff(colnames(attr_burden_disp10), c("attr", "overall_value.x", "overall_value.y"))
  )
  
  attr_burden_disp12 <- attr_burden_disp12 %>% mutate(
    mean = 100 * (mean.x - mean.y) / (overall_value.x - overall_value.y),
    lower = mean, upper = mean,
    attr = "attributable", measure3 = "proportion of disparity to average",
    rural_urban_class = rural_urban_class.y,
    Education = Education.y,
    Race = Race.y, Hispanic.Origin = Hispanic.Origin.y,
    rural_urban_class.x = NULL, rural_urban_class.y = NULL,
    Education.x = NULL, Education.y = NULL,
    Race.x = NULL, Race.y = NULL, Hispanic.Origin.x = NULL, Hispanic.Origin.y = NULL,
    overall_value.x = NULL, overall_value.y = NULL, mean.x = NULL, mean.y = NULL,
    lower.x = NULL, lower.y = NULL, upper.x = NULL, upper.y = NULL,
    attr.x = NULL, attr.y = NULL
  )
}

attr_burden$measure3 <- "value"
attr_burden <- rbind(attr_burden, attr_burden_prop, attr_burden_disp3, attr_burden_disp6, attr_burden_disp9)
rm(
  attr_burden_prop, attr_burden_disp1, attr_burden_disp2, attr_burden_disp3, attr_burden_disp4,
  attr_burden_disp5, attr_burden_disp6, attr_burden_disp7, attr_burden_disp8, attr_burden_disp9#,
 # attr_burden_disp10, attr_burden_disp11, attr_burden_disp12
)

## --Find replace----

#rindreplace1 <- c(states$STATEFP, "us", 1:99999) %>% as.list()
#names(rindreplace1) <- c(states$NAME, "United States", 1:99999)
#levels(all_burden$Region) <- rindreplace1
#levels(attr_burden$Region) <- rindreplace1

rindreplace1 <- data.frame(agr_by = c("nation", rep("STATEFP", nrow(states))),
                           RegionFrom = c("us", states$STATEFP),
                           RegionTo = c("United States", states$NAME))
all_burden <- all_burden %>% 
  left_join(rindreplace1, by = c("Region" = "RegionFrom", "agr_by")) %>%
  mutate(Region = coalesce(RegionTo, Region),
         RegionTo = NULL)

attr_burden <- attr_burden %>% 
  left_join(rindreplace1, by = c("Region" = "RegionFrom", "agr_by")) %>%
  mutate(Region = coalesce(RegionTo, Region),
         RegionTo = NULL)

# c("Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative", "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"),
# c(1:7, 666)

rindreplace2 <- list(
  "High school graduate or lower" = "lower",
  "Some college education but no 4-year college degree" = "middle",
  "4-year college graduate or higher" = "higher",
  "666" = "666"
)
levels(all_burden$Education) <- rindreplace2
levels(attr_burden$Education) <- rindreplace2

rindreplace3 <- list("All genders" = "A", "Male" = "M", "Female" = "F")
levels(all_burden$Gender.Code) <- rindreplace3
levels(attr_burden$Gender.Code) <- rindreplace3

rindreplace4 <- list("National Vital Statistics System" = "nvss", "Mortality Data from CDC WONDER" = "wonder")
levels(all_burden$source) <- rindreplace4
levels(attr_burden$source) <- rindreplace4

# rindreplace5 <- setNames(c("Years of Life Lost (YLL)", "Deaths"), c("YLL","Deaths"))
# all_burden$measure1 <- sapply(all_burden$measure1 , function(x) rindreplace5[[x]])
# attr_burden$measure1 <- sapply(attr_burden$measure1 , function(x) rindreplace5[[x]])

rindreplace6 <- list("crude rate per 100,000" = "crude rate", "age-adjusted rate per 100,000" = "age-adjusted rate",
                     "absolute number" = "absolute number")
levels(all_burden$measure2) <- rindreplace6
levels(attr_burden$measure2) <- rindreplace6

all_burden <- all_burden %>%
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
levels(all_burden$Ethnicity) <- rindreplace7
levels(attr_burden$Ethnicity) <- rindreplace7

# rindreplace8 <- list("large central metro" = 1, "large fringe metro" = 2, "medium metro" = 3, "small metro" = 4, "micropolitan" = 5, "non-core" = 6,"All" = 666,"Unknown" = "Unknown")
rindreplace8 <- list("Large metro" = 1, "Small-medium metro" = 2, "Non metro" = 3, "All" = 666, "Unknown" = "Unknown")

levels(all_burden$rural_urban_class) <- rindreplace8
levels(attr_burden$rural_urban_class) <- rindreplace8

rm(rindreplace1, rindreplace2, rindreplace3, rindreplace4, rindreplace6, rindreplace7, rindreplace8)
## --- test final---
all_burden <- all_burden %>% filter(!is.na(rural_urban_class))#TODO

#--write---
# attr_burden<- attr_burden %>% ungroup %>% select(Year, Ethnicity, Education, rural_urban_class,measure1, measure2, measure3, Region, scenario, mean, lower, upper, method)
# all_burden<- all_burden %>% select(Year, Ethnicity, Education, rural_urban_class,measure1, measure2, Region, overall_value)
measure3_all <- attr_burden$measure3 %>% unique()
method_all <- attr_burden$method %>% unique()

all_burden_county <- all_burden %>% filter(agr_by == "county")
all_burden_not_county <- all_burden %>% filter(agr_by != "county")
attr_burden_county <- attr_burden %>% filter(agr_by == "county")
attr_burden_not_county <- attr_burden %>% filter(agr_by != "county")

fwrite(
  all_burden_not_county,
  file.path(summaryDir, paste0("all_burd", ".csv"))
)

for (i in seq_along(measure3_all)) {
  attr_burden_sub <- attr_burden_not_county %>% filter(measure3 == measure3_all[[i]])
  if (nrow(attr_burden_sub) > 0) {
    fwrite(
      attr_burden_sub,
      file.path(summaryDir, paste0("attr_burd_", i, ".csv"))
    )
  }
}

dir.create(file.path(summaryDir, "county"), recursive = T, showWarnings = F)

fwrite(
  all_burden_county,
  file.path(summaryDir,"county", paste0("all_burd", ".csv"))
)

for (i in seq_along(measure3_all)) {
  for (j in seq_along(method_all)) {
    
    attr_burden_sub <- attr_burden_county %>% filter(measure3 == measure3_all[[i]] & method == method_all[[j]])
    if (nrow(attr_burden_sub) > 0) {
      fwrite(
        attr_burden_sub,
        file.path(summaryDir,"county", paste0("attr_burd_", i,"_",j, ".csv"))
      )
    }
  }
}

toc()
