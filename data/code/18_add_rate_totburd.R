#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 03/27/2021
# Purpose: read total burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat",  "tictoc", "readxl")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
dataDir <- args[2]
tmpDir <- args[3]
agr_by <- args[10]
#pafDir <- args[11]
totalBurdenParsedDir <- args[13]
source <- args[14]
pop.summary.dir <- args[16]
totalBurdenParsed2Dir <- args[17]

if (rlang::is_empty(args)) {
  year <- 2005
  
  dataDir <-"data"
  tmpDir <- "data/tmp"
  agr_by <- "county"
  totalBurdenParsedDir <- "data/09_total_burden_parsed"
  source <- "nvss"
  pop.summary.dir <- "data/12_population_summary"
  totalBurdenParsed2Dir <-"data/13_total_burden_rate"
}

totalBurdenParsed2Dir <- file.path(totalBurdenParsed2Dir, agr_by, source)
dir.create(totalBurdenParsed2Dir, recursive = T, showWarnings = F)
totalBurdenParsed2Dir <- file.path(totalBurdenParsed2Dir, paste0("total_burden_", year, ".csv"))


## ----calculations----
if (!file.exists(totalBurdenParsed2Dir)) {
  ##---read total burden parsed data----
    total_burden <- file.path(totalBurdenParsedDir, agr_by, "nvss", paste0("total_burden_nvss_", year, ".csv")) %>%
      read.csv()
  #}
  total_burden <- total_burden %>%
    filter(rural_urban_class != "Unknown") %>%
    mutate_at(c("rural_urban_class", "Education"), as.factor)

  if (agr_by == "nation") {
    total_burden <- total_burden %>%
      complete(Year, nation, source, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class, nesting(label_cause, attr),
        fill = list(Deaths = 0)
      ) %>%
      mutate_at(c("nation"), as.factor)
  } else if (agr_by == "STATEFP") {
    total_burden <- total_burden %>%
      #complete(Year, STATEFP, source, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class, nesting(label_cause, attr),
      #  fill = list(Deaths = 0)
      #) %>%
      filter(STATEFP != "oth") %>% 
      mutate_at(c("STATEFP"), as.factor)
  }else if(agr_by == "county"){
    total_burden <- total_burden %>%
      filter(county != "oth") %>% 
      mutate_at(c("county"), as.factor)
    
    total_burden <- total_burden %>% filter(label_cause == "all-cause" & rural_urban_class == "666")
  }
  
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
  ## --- measure 1: Deaths and YLL-----
  tic(paste("added YLL and age-adjusted rate to total burden in", year, agr_by))
  # Deaths
  total_burden$measure1 <- "Deaths"
  total_burden <- total_burden %>% dplyr::rename(value = Deaths)

  # YLL
  lifeExpectancy <- read.csv(file.path(dataDir, "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))
  total_burden_yll <- total_burden %>%
    dplyr::mutate(
      Life.Expectancy = lifeExpectancy$Life.Expectancy[findInterval(max_age, lifeExpectancy$Age)], # TODO max_age?
      value = value * Life.Expectancy,
      measure1 = "YLL",
      Life.Expectancy = NULL
    )

  total_burden <- rbind(total_burden, total_burden_yll)
  rm(lifeExpectancy, total_burden_yll)
  
  #------measure 2: absolute number, crude rate and age-standartised rates----- 
  # absolute number
  total_burden$measure2 <- "absolute number"

  # crude rate
  pop_summary_agr <- pop_summary %>%
    group_by_at(vars(all_of(setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population"))))) %>%
    summarise(Population = sum(Population))

  total_burden_crude <- total_burden %>%
    inner_join(pop_summary_agr, by = setdiff(colnames(pop_summary_agr), "Population")) %>%
    filter(Population > 0) %>% # TODO
    mutate(
      value =  100000* (value / Population), #0/0 = NaN
      value = ifelse(is.nan(value), 0, value), #
      measure2 = "crude rate",
      Population = NULL
    )
  total_burden_crude <- total_burden_crude %>% filter(!is.na(value)) # TODO
  rm(pop_summary_agr)

  # age-standartised rates
  # see https://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_14.pdf, page 125 for more information, Table VIII
  standartpopulation <- read_excel(file.path(dataDir, "standartpopulation.xlsx"))
  full_stand_popsize <- sum(standartpopulation$standard_popsize)

  total_burden_age_adj <- crossing(pop_summary, standartpopulation)
  total_burden_age_adj <- total_burden_age_adj %>%
    mutate(
        largerInterval = case_when(
        min_age <= standard_min_age & standard_max_age <= max_age ~ 1,
        standard_min_age <= min_age & max_age <= standard_max_age ~ 2
      ),
      min_age = pmin(min_age, standard_min_age), max_age = pmax(max_age, standard_max_age),
      standard_min_age = NULL, standard_max_age = NULL
    )

  total_burden_age_adj1 <- total_burden_age_adj %>%
    filter(largerInterval == 1) %>%
    group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj), "standard_popsize")))) %>%
    summarise(standard_popsize = sum(standard_popsize))

  total_burden_age_adj2 <- total_burden_age_adj %>%
    filter(largerInterval == 2) %>%
    group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj), "Population")))) %>%
    summarise(Population = sum(Population))

  total_burden_age_adj <- rbind(total_burden_age_adj1, total_burden_age_adj2) %>%
    distinct() %>%
    ungroup()
  total_burden_age_adj$largerInterval <- NULL
  rm(total_burden_age_adj1, total_burden_age_adj2)

  # calculate age-adjusted rate
  total_burden_age_adj <- total_burden %>%
    left_join(total_burden_age_adj,
      by = setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population"))
    ) %>%
    filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
    mutate(min_age.x = NULL, max_age.x = NULL) %>%
    rename(min_age = min_age.y, max_age = max_age.y)

  total_burden_age_adj <- total_burden_age_adj %>%
    group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj), "value")))) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  

  total_burden_age_adj <- total_burden_age_adj %>%
    filter(Population >=1 & full_stand_popsize >=1) %>%
    dplyr::mutate(
      value = (value * standard_popsize / Population) * (100000 / full_stand_popsize), 
      value = ifelse(is.nan(value), 0, value),
      measure2 = "age-adjusted rate",
      Population = NULL, standard_popsize = NULL
    )

  
  total_burden <- rbind(total_burden, total_burden_crude, total_burden_age_adj)
  rm(total_burden_crude, total_burden_age_adj, standartpopulation, full_stand_popsize, pop_summary)

  ## ----finish------
  if(agr_by != "nation"){
    total_burden <- total_burden %>%
      filter(measure1 == "Deaths" & measure2 %in% c("absolute number", "age-adjusted rate"))
  }

  #restrict everything to age_group 25+
  total_burden <- total_burden %>%
    filter(min_age >= 25)
  
  total_burden <- total_burden %>% distinct()

  fwrite(total_burden, totalBurdenParsed2Dir)
  toc()
}

