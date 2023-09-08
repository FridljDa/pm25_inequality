#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# load packages, install if missing
suppressMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  #library(tidyverse)
})

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)
# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  #year <- 2002

  censDir <- "data/05_demog"
  agr_by <- "county"
  pop.summary.dir <- "data/12_population_summary"

} else {
  censDir <- args[8]
  agr_by <- args[10]
  pop.summary.dir <- args[16]
}


pop.summary.dir <- file.path(pop.summary.dir, paste0("pop_race_educ_nation.csv"))

if(file.exists(pop.summary.dir)) quit()

file_list <- list.files(ethnicityEducPopDir)


pop.summary <- lapply(file_list, function(file){
  pop.summary <- readxl::read_excel(file.path(ethnicityEducPopDir, file))
  possible_ethnicitys <- c("White alone", "Non-Hispanic White alone", "Black alone", "Asian alone")
  ethnicity_ind <- sapply(possible_ethnicitys, function(ethnicity) any(pop.summary == ethnicity))
  ethnicity <- possible_ethnicitys[ethnicity_ind]
  ethnicity <- ethnicity[!is.na(ethnicity)]

  possible_years <- 1990:2016
  title <- c(pop.summary[[1, 1]],colnames(pop.summary))
  year <- stringr::str_sub(title, -4, -1)
  year <- as.integer(year)
  year <- year[!is.na(year)]

  education_levels_title <- which(pop.summary == "Educational Attainment", arr.ind = TRUE)
  education_levels_row <- education_levels_title[1, "row"] + 1
  education_levels_col <- (education_levels_title[1, "col"] + 1):ncol(pop.summary)
  education_levels <- pop.summary[education_levels_row, education_levels_col]
  education_levels <- unlist(education_levels)
  unname(education_levels)

  lower_age_row <- rbind(which(pop.summary == c("25 to 29 years"), arr.ind = TRUE),which(pop.summary == c("..25 to 29 years"), arr.ind = TRUE))
  higher_age_row <- rbind(which(pop.summary == c("75 years and over"), arr.ind = TRUE),which(pop.summary == c("..75 years and over"), arr.ind = TRUE))

  age_groups_pos <- lower_age_row[1,"row"]:higher_age_row[1,"row"]
  age_groups <- pop.summary[age_groups_pos, lower_age_row[[1,"col"]]]
  age_groups <- unlist(age_groups)
  # extract numbers from string
  age_groups <- regmatches(age_groups, gregexpr("[[:digit:]]+", age_groups))
  age_groups <- as.data.frame(do.call(rbind, age_groups))
  colnames(age_groups) <- c("min_age", "max_age")
  age_groups[nrow(age_groups),"max_age"] <- 150

  pop.summary <- pop.summary[age_groups_pos, education_levels_col]
  colnames(pop.summary) <- education_levels
  pop.summary <- cbind(age_groups, pop.summary)

  pop.summary <- pop.summary %>%
    pivot_longer(-c("min_age", "max_age"),
                 names_to = "Education2",
                 values_to = "Population"
    )

  pop.summary <- pop.summary %>% mutate(Population = ifelse(Population == "-",
                                                         0,
                                                         1000 * as.numeric(Population))) # Numbers in thousands

  replace1 <- data.frame(
    Education2= c(
      "None", "1st - 4th grade", "5th - 6th grade", "7th - 8th grade", "9th grade",
      "10th grade", "11th grade2","11th grade /2", "High school graduate", "Some college, no degree","Some college no degree", "Associate's degree, occupational",
      "Associate's degree, academic", "Bachelor's degree", "Master's degree", "Professional degree", "Doctoral degree"
    ),
    Education = c(rep("lower",9), rep("middle",4), rep("higher",4))
  )

  pop.summary <- pop.summary %>%
    left_join(replace1, by = "Education2") %>%
    group_by(min_age, max_age, Education) %>%
    summarise(Population = sum(Population))

  pop.summary$ethnicity2 <- ethnicity

  pop.summary$Year <- year

  pop.summary
})
pop.summary <- rbindlist(pop.summary, use.names = T)

replaces2 <- data.frame(
  Race = c("Black or African American","Asian or Pacific Islander", "White" , "White" , "White" , "White"),
  Hispanic.Origin = c(rep("All Origins", 3), "Not Hispanic or Latino", "Hispanic or Latino", "Hispanic or Latino"),
  ethnicity2 = c("Black alone", "Asian alone", "White alone", "Non-Hispanic White alone","White alone", "Non-Hispanic White alone"),
  coeff = c(rep(1, 5), -1)
)

pop.summary <- pop.summary %>%
  left_join(replaces2, by = "ethnicity2") %>%
  group_by(Year, min_age, max_age, Education, Race, Hispanic.Origin) %>%
  summarise(Population = sum(Population*coeff))

pop.summary$nation <- "us"
pop.summary$Gender.Code <- 'A'
pop.summary$rural_urban_class <- 666
pop.summary$svi_bin <- 666
pop.summary$source2 <- "cens2"
write.csv(pop.summary, pop.summary.dir, row.names = FALSE)
