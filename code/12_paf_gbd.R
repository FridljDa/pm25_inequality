#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/18/2020
# Purpose: calculate PAF
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "tidyr","tidyverse", "tictoc", "testthat", "MALDIquant", "ggplot2")

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
exp_rrDir <- args[6]
censDir <- args[8]
cens_agrDir <- args[9]
agr_by <- args[10]
pafDir <- args[11]
totalBurdenParsed2Dir  <- args[17]

# TODO löschen
if (rlang::is_empty(args)) {
  year <- 2013
  agr_by <-"nation"

  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  censDir <- "/Users/default/Desktop/paper2021/data/05_demog"
  cens_agrDir <- "/Users/default/Desktop/paper2021/data/06_dem.agr"
  exp_rrDir <- "/Users/default/Desktop/paper2021/data/04_exp_rr"
  pafDir <- "/Users/default/Desktop/paper2021/data/07_gbd_paf"
  totalBurdenParsed2Dir <- "/Users/default/Desktop/paper2021/data/12_total_burden_parsed2"
  
  
  tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  censDir <- "C:/Users/Daniel/Desktop/paper2021/data/05_demog"
  cens_agrDir <-"C:/Users/Daniel/Desktop/paper2021/data/06_dem.agr"
  exp_rrDir <-"C:/Users/Daniel/Desktop/paper2021/data/04_exp_rr"
  pafDir <- "C:/Users/Daniel/Desktop/paper2021/data/07_gbd_paf"
  totalBurdenParsed2Dir <- "C:/Users/Daniel/Desktop/paper2021/data/12_total_burden_parsed2"
}

#if (year > 2004 & agr_by != "nation") {
#  print(paste("can not aggregate subnational in", year))
#  quit()
#}

if (agr_by == "county") {
  print(paste("GBD PAF not updated for county"))
  quit()
}

# load meta data
census_meta <- file.path(censDir, "meta", paste0("cens_meta_", toString(year), ".csv")) %>%
  fread()

# create directories
cens_agrDir <- cens_agrDir %>% file.path(., agr_by, year)
pafDir <- pafDir %>% file.path(., agr_by, year) # TODO drop year, everything in one file
dir.create(pafDir, recursive = T, showWarnings = F)

# load some data
states <- file.path(tmpDir, "states.csv") %>% read.csv()
causes_ages <- file.path(tmpDir, "causes_ages.csv") %>% read.csv()
# example pm exposures
example_exp_rr <- file.path(exp_rrDir, "cvd_ihd_25.csv") %>% read.csv()
pm_levels <- example_exp_rr$exposure_spline

#calc_conf <- (agr_by == "nation")
calc_conf <- TRUE
##--find largest age intervals---
csv_files <- list.files(totalBurdenParsed2Dir, pattern = paste0(year, ".csv$"), recursive = TRUE)
short_meta <- read.csv(file.path(totalBurdenParsed2Dir, csv_files[1])) %>%
  select(Year, Gender.Code, Race, Hispanic.Origin, Education, min_age, max_age) %>%
  distinct %>%
  mutate(min_age = as.numeric(min_age),
         max_age = as.numeric(max_age))

short_meta <- short_meta %>% arrange(Year, Gender.Code, Race, Hispanic.Origin, Education, min_age, max_age)

short_meta <- short_meta %>%
  group_by(Year, Gender.Code, Race, Hispanic.Origin, Education) %>%
  mutate(cummax = cummax(max_age)) %>%
  filter(max_age >= cummax) %>%
  group_by(Year, Gender.Code, Race, Hispanic.Origin, Education, max_age) %>%
  slice(1)
  
short_meta <- short_meta %>%  
  arrange(desc(row_number())) %>%
  mutate(cummin = cummin(min_age)) %>%
  filter(min_age <= cummin) %>%
  group_by(Year, Gender.Code, Race, Hispanic.Origin, Education, min_age) %>%
  slice(1)

short_meta <- short_meta %>% 
  mutate(cummin = NULL, cummax = NULL)%>%
  ungroup() %>%
  mutate(short_variable_id = row_number()) %>%
  mutate_at(c("Education"), as.factor)

census_meta <- census_meta %>% mutate_at(c("Education"), as.factor)
meta_cross <- inner_join(short_meta, census_meta, by = c("Year", "Gender.Code", "Race", "Hispanic.Origin", "Education"))

meta_cross1<- meta_cross%>%
  filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
  mutate(min_age.x = NULL, max_age.x = NULL) %>%
  rename(min_age = min_age.y, max_age = max_age.y) %>% 
  mutate(variable_id = variable)

meta_cross2<- meta_cross %>%
  filter(min_age.x <= min_age.y & max_age.y <= max_age.x &
           !(min_age.y <= min_age.x & max_age.x <= max_age.y)) %>%
  mutate(min_age.y = NULL, max_age.y = NULL) %>%
  rename(min_age = min_age.x, max_age = max_age.x) %>% 
  mutate(variable_id = short_variable_id)

meta_cross <- rbind(meta_cross1, meta_cross2) %>%
  mutate(short_variable_id = NULL) %>%
  distinct

# add corresponding age_group_id from causes ages
meta_cross <- meta_cross %>%
  mutate(
    age_group_id = c(0,seq(25, 95, 5))[
      findInterval(
        max_age,
        c(0,seq(25, 95, 5)),
        left.open =  F
      ) 
    ]
  )

rm(csv_files, meta_cross1, meta_cross2, short_meta)
### -----calculation-------
regions <- states[, agr_by] %>% unique()
for (region in regions) {
  pafDirX <- paste0("paf_", toString(year), "_", region, ".csv") %>%
    file.path(pafDir, .)

  if (!file.exists(pafDirX)) {
    tic(paste("Calculated PAF in", agr_by, region, "in year", year, "by pm"))

    # read census data aggregated by pm exposure
    cens_agrDirX <- file.path(cens_agrDir,  paste0("cens_agr_", toString(year), "_", region, ".csv")) 
    
    if(!file.exists(cens_agrDirX) & year < 2000 & region %in% c("AK","HI")) break
    
    cens_agr<-fread(cens_agrDirX) 
    
    #some missing, e.g. AAAA00024, because age < 25

    cens_agr <- cens_agr %>% inner_join(meta_cross, by = "variable") 
    
    cens_agr <- cens_agr %>%
      group_by(variable_id, rural_urban_class, scenario, pm) %>%
      summarise(pop_size = sum(pop_size)) %>%
      group_by(variable_id, rural_urban_class, scenario) %>%
      mutate(prop = pop_size/sum(pop_size)) %>%
      select(variable_id, rural_urban_class, scenario, pm, prop )

    # add column, if something from pm_levels missing
    fill_values <- crossing(
      variable_id = cens_agr$variable_id %>% unique(),
      rural_urban_class = cens_agr$rural_urban_class %>% unique(),
      scenario = cens_agr$scenario %>% unique(),
      pm = pm_levels
    ) 
    fill_values$prop <- 0
    cens_agr <- rbind(cens_agr, fill_values)

    # make wider
    cens_agr <- cens_agr %>%
      mutate(pm = sapply(pm, function(x) pm_levels[match.closest(x, pm_levels)])) %>%
      pivot_wider(
        names_from = pm,
        names_sort = TRUE,
        values_fn = sum,
        values_from = prop,
        values_fill = 0
      )
    # as matrix
    matrix_cens_agr <- cens_agr %>%
      subset(select = -c(variable_id, rural_urban_class, scenario))%>%
      as.matrix()
    rownames(matrix_cens_agr) <- paste(cens_agr$variable_id,cens_agr$rural_urban_class,cens_agr$scenario, sep = "-")

    # loop over all exp_rr curves
    pafs <- apply(causes_ages, 1, function(cause_age) {
      label_cause <- cause_age[["label_cause"]]
      age_group_idX <- cause_age[["age_group_id"]]
      tic(paste("Calculated PAF in", agr_by, region, "in year", year, "by pm for", label_cause, age_group_idX))

      exp_rr <- ifelse(age_group_idX == "all ages",
        paste0(label_cause, ".csv"),
        paste0(label_cause, "_", age_group_idX, ".csv")
      ) %>%
        file.path(exp_rrDir, .) %>%
        fread()

      exp_rr <- as.matrix(exp_rr[, -1])
      # too expensive for granular geographic level
      if (!calc_conf) {
        exp_rr <- as.matrix(rowMeans(exp_rr))
        colnames(exp_rr) <- "draw0"
      } 
      rownames(exp_rr) <- pm_levels

      ifelse(age_group_idX == "all ages",
        censMeta <- meta_cross,
        censMeta <- meta_cross %>% filter(age_group_id == as.numeric(age_group_idX))
      )

      if (nrow(censMeta) == 0) {
        return() # TODO
      }
      # subset rows with right age

      matrix_cens_agr_sub <- matrix_cens_agr %>% subset(gsub("-.*","",rownames(.)) %in% censMeta$variable_id)

      # apply formular sum(prop * (rr-1))/(1+sum(prop * (rr-1)))
      result <- matrix_cens_agr_sub %*% (exp_rr - 1)
      result <- apply(result, 1:2, function(x) x / (1 + x))

        # write to dataframe
        result <- as.data.frame(result)
        result <- tibble::add_column(result, label_cause = label_cause, .before = 1)
        result <- tibble::rownames_to_column(result, "variable")
      
      toc()
      return(result)
    }) %>% rbindlist
    pafs <- pafs %>% tidyr::separate(variable,
                                            sep = "-",
                                            into = c("variable_id","rural_urban_class","scenario"))
    
    meta_cross[, agr_by] <- region

    pafs <- right_join(meta_cross %>% 
                         select(setdiff(colnames(meta_cross), c("variable", "age_group_id"))) %>%
                         distinct,
                       pafs, by = "variable_id")

   
    write.csv(pafs, pafDirX, row.names = FALSE)
    toc()
  }
}
""
