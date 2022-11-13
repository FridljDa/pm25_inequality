#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table",  "testthat", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
agr_by <- args[10]
pafDir <- args[11]
source <- args[14]
totalBurdenParsed2Dir <-args[17]
attr_burdenDir <- args[18]

if (rlang::is_empty(args)) {
  year <- 2010
  agr_by <- "nation"
  source <- "nvss"
  
  tmpDir <-  "data/tmp"
  
  pafDir <- "data/07_gbd_paf"
  
  totalBurdenParsed2Dir <-"data/13_total_burden_rate"
  attr_burdenDir <- "data/14_attr_burd"
}

attr_burdenDir <- file.path(attr_burdenDir, agr_by, source)
dir.create(attr_burdenDir, recursive = T, showWarnings = F)
attr_burdenDir <- file.path(attr_burdenDir, paste0("attr_burd_", toString(year), ".csv"))

if (agr_by == "county") {
  quit()
}

#read some data
states <- file.path(tmpDir, "states.csv") %>% read.csv
total_burden <- file.path(totalBurdenParsed2Dir,agr_by,source, paste0("total_burden_",year,".csv")) %>% 
  fread %>% 
  filter(label_cause %in% c("cvd_ihd","cvd_stroke", "neo_lung", "resp_copd", "lri", "t2_dm")) 

#intense computation
if (Sys.info()["sysname"] == "Windows") memory.limit(size=500000)

## ----calculations-----
if (!file.exists(attr_burdenDir)) {
  ## ----determine join variables
  join_variables <- c("Year", "Race", "Hispanic.Origin","Education","rural_urban_class","Gender.Code", "label_cause","min_age","max_age", agr_by)
  group_variables <- c("Year","Race","Hispanic.Origin","Education","rural_urban_class", "Gender.Code", agr_by)

  ## ----- read paf------
  regions <- states[, agr_by] %>% unique()

  tic("calc_attr_burd: 1 read all PAFs")
  pafs <- lapply(regions, function(region) {
    pafDirX <- file.path(pafDir, agr_by, year, paste0("paf_", toString(year), "_", region, ".csv"))
    if(!file.exists(pafDirX) & year < 2000 & region %in% c("AK","HI")) return(NULL)
    fread(pafDirX)
  }) %>%
    rbindlist %>%
    as.data.frame()
  pafs <- pafs %>% filter(min_age >= 25)
  
  total_burden <- total_burden %>% mutate_at(c( "rural_urban_class"), as.factor)
  pafs <- pafs %>% mutate_at(c("rural_urban_class"), as.factor)
  
  if (agr_by == "STATEFP") {
    possible_regions <- c(1, 4:6, 8:13, 16:42, 44:51, 53:56)
  } else if (agr_by == "Census_Region") {
    pafs$Census_Region <- paste("CENS-R", pafs$Census_Region)
    possible_regions <- paste("CENS-R", 1:4)
  } else if (agr_by == "nation") {
    possible_regions <- "us"
  } else if (agr_by == "Census_division") {
    pafs$Census_division <- paste("CENS-D", pafs$Census_division)
    possible_regions <- paste("CENS-D", 1:9)
  } else if (agr_by == "hhs_region_number") {
    pafs$hhs_region_number <- paste("HHS", pafs$hhs_region_number)
    possible_regions <- paste("HHS", 1:10)
  } else if (agr_by == "county") {
    # pafs$county<-sprintif(%02d,03d,pafs$state,pafs$county)
    # TODO
    pafs$state <- NULL
    possible_regions <- c() # TODO too difficult
  }

  # missing regions
  missing <- setdiff(possible_regions, pafs[, agr_by])
  if (length(missing) > 0) {
    print("Regions in paf data missing:")
    print(missing)
  }
  # give some feedback on what is still missing from total burden
  # one side
  test_variables <- setdiff(join_variables,c("min_age","max_age",agr_by))
  total_burden_test <- total_burden %>%
    select(all_of(test_variables)) %>%
    distinct()
  
  pafs_test <- pafs %>%
    select(all_of(test_variables)) %>%
    distinct()
  
  missing <- anti_join(total_burden_test , pafs_test, by = test_variables) 
  if (nrow(missing) > 0) {
    print(paste(nrow(missing), "rows are still missing in pafs data for", agr_by, ":"))
    print(head(missing))
  }
  
  # other side
  missing <- anti_join(pafs_test, total_burden_test, by = test_variables) 
  if (nrow(missing) > 0) {
    print(paste(nrow(missing), "rows are still missing in total burden data for", agr_by, ":"))
    print(head(missing))
  }
  rm(missing, total_burden_test, pafs_test, test_variables)
  toc()
  ## ----- join total_burden and pafs-----
  tic("calc_attr_burd: 2 joined PAFs and total burden data")
  
  paf_ageDir <- file.path(pafDir, agr_by, year)
  paf_age <- file.path(paf_ageDir, list.files(paf_ageDir)[[1]]) %>% read.csv()
  paf_age <- paf_age %>%
    select(Hispanic.Origin, Race, Education, min_age, max_age, Year, rural_urban_class) %>%
    distinct() %>%
    arrange(min_age, max_age)%>%
    mutate_at(c( "rural_urban_class"), as.factor)
  
  total_burden <- total_burden %>% inner_join(paf_age, by = c("Hispanic.Origin", "Race", "Education","rural_urban_class", "Year"))
  
  # case 1: total_burden inside pad
  # good
  
  total_burden <- total_burden %>%
    filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
    mutate(
      min_age = pmin(min_age.x, min_age.y), max_age = pmax(max_age.x, max_age.y),
      min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL
    )
  
  total_burden <- total_burden %>%
    group_by_at(vars(all_of(setdiff(colnames(total_burden), "value")))) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  
  #burden_paf <- inner_join(total_burden, pafs, by = join_variables)
  burden_paf <- inner_join(total_burden, pafs, by = setdiff(join_variables,c("min_age","max_age"))) 
  
  rm(pafs, paf_age)
  toc()
  
  tic("calc_attr_burd: 3 filtered wrong age combinations")
  
  burden_paf <- burden_paf %>% 
     filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
     mutate(min_age = pmin(min_age.x, min_age.y), max_age = pmax(max_age.x, max_age.y),
             min_age.x = NULL, min_age.y = NULL, max_age.x = NULL, max_age.y = NULL)
  toc()
  
  ## ----- calculate attributable burden------
  tic("calc_attr_burd: 3 pivot_longer")

  burden_paf <- pivot_longer(burden_paf,
                             cols = colnames(burden_paf) %>% grep('draw', ., value=TRUE),
                             names_to = "draw", 
                             values_to = "paf") 
  toc()
  
  tic("calc_attr_burd: 4 calculated attributable burden")
  attr_burden <- burden_paf %>%
    mutate(
      value = value * paf,
      attr = "attributable",
      paf = NULL
    )
  rm(burden_paf)
  toc()

  # group "out" ages
  tic("calc_attr_burd: 5 grouped by group_variables and draw")
  columns <- c(group_variables, "draw", "measure1","measure2", "attr", "scenario")
  attr_burden <- attr_burden %>%
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::summarize(value = sum(value),
                     min_age = min(min_age),
                     max_age = max(max_age)
    )
  toc()
  
  #group "out" draw, mean and confidence interval
  tic("calc_attr_burd: 6 grouped out draws, calculate mean, lower, upper")
  columns <- c(group_variables, "measure1","measure2", "attr", "min_age", "max_age", "scenario")
  attr_burden <- attr_burden %>%
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::summarize(mean = mean(value),
                     lower = quantile(value,p=.025),
                     upper = quantile(value,p=.975) 
              )
  attr_burden <- attr_burden %>% tibble::add_column(source = source)
  attr_burden <- attr_burden %>% tibble::add_column(method = "GBD")
  toc()
  fwrite(attr_burden, attr_burdenDir)
}
