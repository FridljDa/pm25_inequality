#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 01/16/2021
# Purpose: summarize data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory #test
#rm(list = ls(all = TRUE))

# load packages, install if missing
suppressMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  library(tidyverse)
  library(tictoc)
})


options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
options(scipen = 10000)
# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  agr_by <- "county"

  censDir <- "data/05_demog"
  cdcPopDir <- "data/10_cdc_population"
  pop.summary.dir <- "data/12_population_summary"
} else {
  censDir <- args[8]
  agr_by <- args[10]
  cdcPopDir <- args[15]
  pop.summary.dir <- args[16]
}
# https://wonder.cdc.gov/controller/saved/D134/D140F499
cdcPopDir <- file.path(cdcPopDir, agr_by)
plotDir <- file.path(pop.summary.dir, "plot", agr_by)
dir.create(plotDir, recursive = T, showWarnings = F)
pop.summary.dir <- file.path(pop.summary.dir, paste0("pop_cdc_", agr_by, ".csv"))

agr_by_replace <- c(
  "county" = "County", "Census_Region" = "Census.Region.Code", "Census_division" = "Census.Division.Code",
  "hhs_region_number" = "HHS.Region.Code", "STATEFP" = "State.Code", "nation" = "nation", "county" = "County.Code"
)
agr_by_new <- agr_by_replace[agr_by]

if (file.exists(pop.summary.dir) | agr_by == "county") {
  quit()
}
# if (!file.exists(pop.summary.dir) & agr_by != "county") {
#-----read -----
files <- list.files(cdcPopDir)
cdc_pop <- lapply(files, function(file) {
  fileDir <- file.path(cdcPopDir, file)
  cdc_pop <- read.delim(fileDir)

  notes_hisp_or <- cdc_pop$Notes[grepl("Hispanic Origin:", cdc_pop$Notes, fixed = TRUE)]
  notes_gender <- cdc_pop$Notes[grepl("Gender:", cdc_pop$Notes, fixed = TRUE)]
  notes_race <- cdc_pop$Notes[grepl("Race:", cdc_pop$Notes, fixed = TRUE)]

  cdc_pop <- cdc_pop %>% filter(Notes == "")
  cdc_pop$Notes <- NULL
  cdc_pop <- cdc_pop[!apply(is.na(cdc_pop) | cdc_pop == "", 1, all), ]

  if (!("Race" %in% colnames(cdc_pop))) cdc_pop$Race <- "All"
  # if (!("Ethnicity" %in% colnames(cdc_pop))) cdc_pop$Ethnicity <- "All Origins"
  if (agr_by == "nation") cdc_pop$nation <- "us"

  if (!"Ethnicity" %in% colnames(cdc_pop)) {
    if (rlang::is_empty(notes_hisp_or)) {
      cdc_pop$Ethnicity <- "All Origins"
    } else if (notes_hisp_or == "Hispanic Origin: Hispanic or Latino") {
      cdc_pop$Ethnicity <- "Hispanic or Latino"
    } else if (Ethnicity == "Hispanic Origin: Not Hispanic or Latino") {
      cdc_pop$Hispanic.Origin <- "Not Hispanic or Latino"
    }
  }

  if (!"Gender.Code" %in% colnames(cdc_pop)) {
    if (rlang::is_empty(notes_gender)) {
      cdc_pop$Gender.Code <- "A"
    } else if (notes_gender == "Gender: Female") {
      cdc_pop$Gender.Code <- "F"
    } else if (notes_gender == "Gender: Male") {
      cdc_pop$Gender.Code <- "M"
    } else {
      print(paste("Gender missing in", file))
    }
  }

  cdc_pop <- cdc_pop %>%
    select(all_of(c(
      "min_age" = "Age.Code",
      "max_age" = "Age.Code",
      "Year" = "Yearly.July.1st.Estimates.Code",
      "Hispanic.Origin" = "Ethnicity",
      "Gender.Code" = "Gender.Code",
      "Race" = "Race",
      "Population" = "Population",
      agr_by_new
    )))

  return(cdc_pop)
})

cdc_pop <- cdc_pop %>%
  rbindlist() %>%
  as.data.frame() %>%
  distinct()

cdc_pop <- cdc_pop %>% tibble::add_column(Education = 666)
cdc_pop <- cdc_pop %>% tibble::add_column(rural_urban_class = 666)
cdc_pop <- cdc_pop %>% tibble::add_column(svi_bin = 666)
cdc_pop <- cdc_pop %>% tibble::add_column(svi_bin = 666)
cdc_pop$min_age[cdc_pop$min_age == "85+"] <- 85
cdc_pop$max_age[cdc_pop$max_age == "85+"] <- 150
cdc_pop$min_age <- as.numeric(cdc_pop$min_age)
cdc_pop$max_age <- as.numeric(cdc_pop$max_age)

cdc_pop <- cdc_pop %>% tibble::add_column(source2 = "CDC")
# only consider 25+ population
cdc_pop <- cdc_pop %>% filter(min_age >= 25)

write.csv(cdc_pop, pop.summary.dir, row.names = FALSE)
## ---plot---
for (location in cdc_pop[, agr_by] %>% unique()) {
  cdc_pop_sub <- cdc_pop %>% filter(get(agr_by) == location)
  cdc_pop_sub <- cdc_pop_sub %>%
    mutate(Ethnicity = paste0(Race, ", ", Hispanic.Origin)) %>%
    filter(Ethnicity %in% c(
      "White, Not Hispanic or Latino",
      "White, All Origins",
      "White, Hispanic or Latino",
      "Black or African American, All Origins",
      "Asian or Pacific Islander, All Origins",
      "American Indian or Alaska Native, All Origins"
    ) & Gender.Code == "A")

  cdc_pop_sub <- cdc_pop_sub %>%
    group_by(Ethnicity, Year) %>%
    summarise(Population = sum(Population))

  g <- ggplot(cdc_pop_sub, aes(x = Year, y = Population)) +
    geom_line(aes(color = Ethnicity), size = 1) +
    ylab(paste("Population")) +
    xlab("Year") +
    ylim(0, NA) +
    xlim(1990, 2016) +
    theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin()) +
    guides(col = guide_legend(nrow = 3, byrow = TRUE)) +
    ggtitle(paste("Population in", location))

  ggsave(file.path(plotDir, paste0(location, "_cdc_plot.png")), plot = g)
}
# }
