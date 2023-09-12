
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: cross downloaded and target census meta data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "data.table", 
  "tictoc", "testthat", "rlang"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
if (rlang::is_empty(args)) {
  year <- 1995
  censDir <- "data/05_demog"
}else{
  year <- args[1]
  censDir <- args[8]
}


metaDir <- file.path(censDir, "meta")
dir.create(metaDir, recursive = T, showWarnings = F)
cross_bridgeDir <- file.path(censDir, "cross_bridge")
dir.create(cross_bridgeDir, recursive = T, showWarnings = F)

## ----- determine which variables we want to have ----
aim_metaDir <- file.path(metaDir, paste0("cens_meta_", year, ".csv"))
if (!file.exists(aim_metaDir)) {
  aim_meta1 <- data.frame(
    Race = c("White", "White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American", "All"),
    Hispanic.Origin = c("Not Hispanic or Latino", "Hispanic or Latino", "All Origins", "All Origins", "All Origins", "All Origins"),
    Education = "666"
  )
  aim_meta1 <- merge(data.frame(Gender.Code = c("A")), aim_meta1) # , "M", "F"

  aim_meta1 <- rbind(
    merge(
      aim_meta1,
      merge(
        data.frame(Year = c(2000:2008, 2010)),
        data.frame(
          min_age = c(0, seq(25, 85, 5)),
          max_age = c(24, seq(29, 84, 5), 150)
        )
      )
    ),
    merge(
      aim_meta1,
      merge(
        data.frame(Year = c(2009, 2011:2016)),
        data.frame(
          min_age = c(0, 25, 30, 35, 45, 55, 65, 75, 85),
          max_age = c(24, 29, 34, 44, 54, 64, 74, 84, 150)
        )
      )
    )
  )

  aim_meta2 <- data.frame(Race = "All", Hispanic.Origin = "All Origins", Education = c("lower", "middle", "higher"))
  aim_meta2 <- merge(data.frame(Year = 2009:2016), aim_meta2)
  aim_meta2 <- merge(data.frame(Gender.Code = c("A")), aim_meta2) # , "M", "F"
  # Ignoring 18-25
  aim_meta2 <- merge(aim_meta2, data.frame(
    min_age = c(25, 35, 45, 65),
    max_age = c(34, 44, 64, 150)
  ))

  aim_meta3 <- data.frame(
    Race = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American", "All"),
    Hispanic.Origin = c("All Origins", "All Origins", "All Origins", "All Origins", "All Origins"),
    Education = "666"
  )
  aim_meta3 <- merge(data.frame(Year = 1990:2000), aim_meta3)
  aim_meta3 <- merge(data.frame(Gender.Code = c("A")), aim_meta3)
  aim_meta3 <- merge(
    aim_meta3,
    data.frame(
      min_age = c(0, seq(25, 85, 5)),
      max_age = c(24, seq(29, 84, 5), 150)
    )
  )

  aim_meta4 <- data.frame(
    Race = c("White", "White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American"),
    Hispanic.Origin = c("Not Hispanic or Latino", "Hispanic or Latino", "All Origins", "All Origins", "All Origins")
  )
  aim_meta4 <- merge(
    aim_meta4,
    data.frame(Education = c("lower", "middle", "higher"))
  )
  aim_meta4 <- merge(data.frame(Year = 2009:2016), aim_meta4)
  aim_meta4 <- merge(data.frame(Gender.Code = c("A")), aim_meta4)
  aim_meta4$min_age <- 25
  aim_meta4$max_age <- 150

  aim_meta <- rbind(aim_meta1, aim_meta2, aim_meta3, aim_meta4) %>% distinct()
  rm(aim_meta1, aim_meta2, aim_meta3, aim_meta4)

  aim_meta$variable <- apply(aim_meta, 1, function(row) {
    Race <- switch(row[["Race"]],
      "White" = "W",
      "American Indian or Alaska Native" = "I",
      "Asian or Pacific Islander" = "A",
      "Black or African American" = "B",
      "All" = "U",
    )
    Hispanic.Origin <- substring(row[["Hispanic.Origin"]], 1, 1)
    Education <- switch(row[["Education"]],
      "lower" = "L",
      "middle" = "M",
      "higher" = "H",
      "666" = "A"
    )
    Gender.Code <- row[["Gender.Code"]]
    min_age <- sprintf("%02d", as.numeric(row[["min_age"]]))
    max_age <- sprintf("%03d", as.numeric(row[["max_age"]]))
    paste0(Race, Hispanic.Origin, Education, Gender.Code, min_age, max_age)
  })

  aim_meta <- aim_meta %>% filter(Year == year)

  write.csv(aim_meta, aim_metaDir, row.names = FALSE)
}
## ----- cross-bridge-----
# cross bridge notation from census to own notation
cross_bridgeDir <- file.path(cross_bridgeDir, paste0("cross_meta_", year, ".csv"))
if (!file.exists(cross_bridgeDir) & year %in% c(1990, 2000, 2009:2016)) {
  tic(paste("crossed census meta data for", year))
  aim_meta <- read.csv(aim_metaDir)
  downloaded_meta <- file.path(censDir, "meta_down", paste0("cens_meta_", toString(year), ".csv")) %>% read.csv()
  downloaded_meta <- downloaded_meta %>%
    mutate(Education = as.character(Education)) %>%
    rename(Race2 = Race, Hispanic.Origin2 = Hispanic.Origin, Education2 = Education, Gender.Code2 = Gender.Code)
  downloaded_meta[downloaded_meta == "High school graduate (includes equivalency)"] <- "High school graduate, GED, or alternative"
  downloaded_meta <- downloaded_meta %>% filter(min_age >= 25)

  if (year == 1990) {
    replaces1 <- data.frame(
      Race = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American", "All", "Other race"),
      Race2 = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "Asian or Pacific Islander", "BLACK OR AFRICAN AMERICAN", "all", "Other race")
    )
  } else {
    replaces1 <- data.frame(
      Race = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Asian or Pacific Islander", "Black or African American", "All", "Other race"),
      Race2 = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER", "BLACK OR AFRICAN AMERICAN", "all", "Other race")
    )
  }

  replaces2 <- data.frame(
    Hispanic.Origin = c("Not Hispanic or Latino", "All Origins", "Hispanic or Latino", "Hispanic or Latino"),
    Hispanic.Origin2 = c("NOT HISPANIC OR LATINO", "all", "all", "NOT HISPANIC OR LATINO"),
    coeff = c(1, 1, 1, -1)
  )

  replaces3_1 <- data.frame(
    Education = c("lower", "lower", "middle", "higher", "666"),
    Education2 = c("Less than high school diploma", "High school graduate, GED, or alternative", "Some college or associate's degree", "Bachelor's degree or higher", "666")
  )
  replaces3_1 <- merge(
    replaces3_1,
    data.frame(Race = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Asian or Pacific Islander", "Black or African American"))
  )

  replaces3_2 <- data.frame(
    Education = c(rep("lower", 3), rep("middle", 2), rep("higher", 2), "666"),
    Education2 = c(
      "Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative",
      "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree", "666"
    ),
    Race = "All"
  )
  replaces3 <- rbind(replaces3_1, replaces3_2)
  rm(replaces3_1, replaces3_2)

  replaces4 <- data.frame(
    Gender.Code = c("M", "F", "A", "A"),
    Gender.Code2 = c("M", "F", "M", "F")
  )

  if (year == 1990) {
    replaces5 <- data.frame(
      Race = c("All", "All", "All", "All", "All"),
      Race2 = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "Asian or Pacific Islander", "BLACK OR AFRICAN AMERICAN", "Other race")
    )
  } else {
    replaces5 <- data.frame(
      Race = c("All", "All", "All", "All", "All"),
      Race2 = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER", "BLACK OR AFRICAN AMERICAN")
    )
  }

  cross_bridge <- aim_meta %>%
    left_join(replaces2, by = "Hispanic.Origin") %>%
    left_join(replaces4, by = "Gender.Code")

  cross_bridge <- cross_bridge %>%
    mutate(Education = as.character(Education))  %>%
    left_join(replaces3, by = c("Education","Race"))

  cross_bridge1 <- cross_bridge %>%
    filter(!(Race == "All" & Hispanic.Origin == "All Origins" & Education == "666")) %>%
    left_join(replaces1, by = "Race")

  cross_bridge2 <- cross_bridge %>%
    filter((Race == "All" & Hispanic.Origin == "All Origins" & Education == "666")) %>%
    left_join(replaces5, by = "Race")

  cross_bridge <- rbind(cross_bridge1, cross_bridge2)
  rm(cross_bridge1, cross_bridge2)

  cross_bridge <- cross_bridge %>%
    left_join(downloaded_meta, by = c("Year", "Gender.Code2", "Race2", "Hispanic.Origin2", "Education2"))

  cross_bridge <- cross_bridge %>% filter(min_age.x <= min_age.y & max_age.y <= max_age.x)
  rm(replaces1, replaces2, replaces3, replaces4)

  cross_bridge <- cross_bridge %>% select(variable.x, coeff, variable.y)
  fwrite(cross_bridge, cross_bridgeDir)
  toc()
}
