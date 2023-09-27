
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census meta data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "data.table",  "tictoc", "testthat" #"rlang", "cdcfluview"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
censDir <- args[8]

# quits, if not downloadable year
if (!year %in% c(1990:2016)) {
  print(paste("can not download census data for", year))
  quit()
}

## ----------download useful data to tmp-------------------------------------------------------------------------------
# download states data
filepathStates <- file.path(tmpDir, "states.csv")
if (!file.exists(filepathStates)) {
  # only contiguous US
  # excluding American Samoa, Guam, Commonwealth of the Northern Mariana Islands, Puerto Rico, United States Virgin Islands
  states1 <- states() %>%
    as.data.frame() %>%
    select(c(REGION, DIVISION, STATEFP, STUSPS, NAME)) %>%
    filter(!(STUSPS %in% c("AS", "GU", "MP", "PR", "VI"))) %>%
    arrange(STATEFP) %>%
    # rename it can be merged later
    setnames(
      c("REGION", "DIVISION"),
      c("Census_Region", "Census_division")
    )

  data(hhs_regions)

  states2 <- hhs_regions %>%
    as.data.frame() %>%
    select(c(region_number, state_or_territory)) %>%
    setnames(
      c("state_or_territory", "region_number"),
      c("NAME", "hhs_region_number")
    )

  # merge for full information
  states <- merge(states1, states2) %>%
    mutate(nation = "us")

  write.csv(states, filepathStates, row.names = FALSE)
} else {
  states <- read.csv(filepathStates)
}
rm(filepathStates)

censMetaDir <- file.path(censDir, "meta_down")
dir.create(censMetaDir, recursive = T, showWarnings = F)

filepathCensMeta <- paste0("cens_meta_", toString(year), ".csv") %>%
  file.path(censMetaDir, .)

if (year %in% c(2000, 2009:2016)) {
  ### ------------------------download demographic data-----------------------------------------------------------------
  # Add key to .Renviron
  key <- "your key"
  Sys.setenv(CENSUS_KEY = key)


  ## ----- download census metadata------

  # relevant groups for each year and table names
  if (year %in% 2000:2008) {
    # decennical census, sex by age for races
    table_groups <- data.frame(
      # groups = c("P012A", "P012B", "P012C", "P012D", "P012E", "P012I", "PCT012J", "PCT012K", "PCT012L", "PCT012M"),
      groups = c("P012A", "P012B", "P012C", "P012D", "P012E", "P012I"),
      tablename = "dec/sf1",
      vintage = 2000
    )
  } else if (year == 2010) {
    # decennical census, sex by age for races
    table_groups <- data.frame(
      # groups = c("PCT12A", "PCT12B", "PCT12C", "PCT12D", "PCT12E", "PCT12I", "PCT12J", "PCT12K", "PCT12L", "PCT12M"),
      groups = c("PCT12A", "PCT12B", "PCT12C", "PCT12D", "PCT12E", "PCT12I"),
      tablename = "dec/sf1",
      vintage = year
    )
  } else if (year %in% c(2009,2011:2016)) {
    # american community survey, sex by age for races
    table_groups <- data.frame(
      groups = c("B01001A", "B01001B", "B01001C", "B01001D", "B01001E", "B01001H"),
      tablename = "acs/acs5",
      vintage = year
    )
  }

  if (year %in% 2009:2016) {
    # american community survey, sex by age for educational attainment
    table_groups <- rbind(
      table_groups,
      #data.frame(groups = "B15001", tablename = "acs/acs5", vintage = year)
      data.frame(groups = c("B15001", "C15002A", "C15002B", "C15002C", "C15002D", "C15002E", "C15002H"), 
                 tablename = "acs/acs5", 
                 vintage = year)
    )
  } #

  # download meta data, if necessary
  if (!file.exists(filepathCensMeta)) {
    tic(paste("Downloaded census meta data for year", toString(year)))

    # loop over all relevant groups
    census_meta <- apply(table_groups, 1, function(row) {
      tablename <- row[["tablename"]]
      group <- row[["groups"]]
      # download meta data
      census_meta <- listCensusMetadata(
        name = tablename,
        vintage = row[["vintage"]],
        type = "variables",
        group = group
      ) %>%
        select("name", "label", "concept") # select relevant columns

      # parse information a bit
      census_meta <- census_meta %>% mutate(
        Year = year,
        tablename = tablename,
        group = group,
        ## parse "label", "concept" from String to seperate columns
        label = strsplit(label, "!!"),
      )

      # the acs includes estimates and annotation of estimates
      census_meta$datatype <- sapply(census_meta$label, function(l) ifelse(tablename == "acs/acs5", l[[1]], "Estimate"))

      census_meta$label <- lapply(census_meta$label, function(l) {
        # making acs label notation coherant with dec cens notation
        if (tablename == "acs/acs5") {
          return(l[-1])
        } else {
          return(l)
        }
      })

      #extract age
      # filter relevant rows
      census_meta <- census_meta %>%
        mutate(label_len = sapply(label, length)) %>%
        filter(
          label_len == ifelse(group == "B15001", 4, 3), # filters granular data with gender and age group
          datatype == "Estimate" # filters Estimates, excluding Annotations and Margins of Error
        ) %>%
        mutate(label_len = NULL, datatype = NULL)
      
      census_meta <- census_meta %>% mutate(
        # age
        age = label %>% sapply(function(l) l[3]),
        
        min_age = age %>% sapply(function(a) {
          # age includes "under", min_age = 0
          if (grepl("Under", a)) {
            return(0)
          }
          # else extract first number in String
          str_extract(a, "[:digit:]+") %>%
            as.numeric()
        }),

        max_age = age %>% sapply(function(a) {
          # if includes "and over", max_age = 150, since humans do not get older
          if (grepl("and over", a) | grepl("AND OVER", a)) {
            return(150)
          }
          # otherwise extracts last number in String
          last_num <- str_extract_all(a, "[:digit:]+") %>%
            unlist() %>%
            tail(1) %>%
            as.numeric()
          # if includes "under", max_age = last_num -1
          if (grepl("Under", a)) {
            return(last_num - 1)
          } else { # else just last_num
            return(last_num)
          }
        }),
        age = NULL
      )
      
      census_meta$min_age <- ifelse(
        grepl("25 YEARS AND OVER", census_meta$concept),
        25,
        census_meta$min_age
      )
      
      census_meta$max_age <- ifelse(
        grepl("25 YEARS AND OVER", census_meta$concept),
        150,
        census_meta$max_age
      )
      
      # extracts gender
      possible_genders <- c("Male", "Female")
      census_meta$gender <- sapply(census_meta$label, function(label){
        gender <- intersect(possible_genders, label)
        return(gender)
      })
      census_meta$Gender.Code <- ifelse(census_meta$gender == "Female", "F", "M")
      
      #extract education 
      possible_education <- c("Bachelor's degree or higher", "Less than high school diploma", "Some college or associate's degree",
                              "Less than 9th grade", "9th to 12th grade, no diploma", "High school graduate, GED, or alternative",
                              "Some college, no degree", "Associate's degree", "Bachelor's degree", "Graduate or professional degree",
                              "High school graduate (includes equivalency)"
      )
      census_meta$Education <- sapply(census_meta$label, function(label){
        Education <- intersect(possible_education, label)
        if(length(Education) == 0){
          return("666")
        }else{
          return(Education)
        }
      })
      
      
      if (group == "B15001") {
        census_meta <- census_meta %>% mutate(
          Race = "all",
          Hispanic.Origin = "all"
        )
      } else {
        census_meta <- census_meta %>% mutate(
          race_his = concept %>% sapply(function(conc) {
            # extract the information in the brackets
            # e.g. sex by age (White alone, not hispanic or latino) => White alone, not hispanic or latino
            regmatches(conc, gregexpr("(?<=\\().*?(?=\\))", conc, perl = T))[[1]]
          }),
          # extract race
          Race = race_his %>% sapply(function(race_his) {
            race_his %>%
              strsplit(., ",") %>%
              unlist() %>%
              extract2(1) %>%
              # fetch what comes before ","; e.g.: White alone, not hispanic or latino => White alone
              substr(., 1, nchar(.) - 6)
            # removes " alone", e.g. "White alone" => "White"
          }),
          # extracts hispanic origin. option: not Hispanic or latino, Hispanic or latino, all
          Hispanic.Origin = race_his %>% sapply(function(race_his) {
            a <- race_his %>%
              strsplit(., ",") %>%
              unlist()
            # if no comma, "all"
            ifelse(length(a) <= 1,
              "all",
              a[2] %>% substring(., 2)
            )
            # extract what comes after ","; e.g.: White alone, not hispanic or latino => not hispanic or latino
          }),
          race_his = NULL
        )
      }
      census_meta <- census_meta %>% mutate(label = NULL, concept = NULL)
      return(census_meta)
    }) %>%
     rbindlist(use.names = TRUE)

    setnames(census_meta, "name", "variable") # rename for later purpose

    # drop unrequired information
    fwrite(census_meta, filepathCensMeta)

    toc()
  }
}else if(year %in% 1990:1999){
  if (!file.exists(filepathCensMeta)) {
    # meta1990 <- data.frame(Race = c("White", "Black or African American", "American Indian or Alaska Native","Asian or Pacific Islander","Other race"))
    meta1990 <- data.frame(Race = c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN AND ALASKA NATIVE","Asian or Pacific Islander", "Other race"))
    meta1990 <- merge(
      data.frame(Gender.Code = c("M", "F")),
      meta1990
    )
    meta1990 <- merge(
      data.frame(
        min_age = c(0, 1, 3, 5, 6, 7, 10, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 25, 30, 35, 40, 45, 50, 55, 60, 62, 65, 70, 75, 80, 85),
        max_age = c(0, 2, 4, 5, 6, 9, 11, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 29, 34, 39, 44, 49, 54, 59, 61, 64, 69, 74, 79, 84, 150)
      ),
      meta1990
    )
    meta1990$variable <- sprintf("ET4%03d", seq.int(nrow(meta1990)))
    meta1990$Education <- 666
    meta1990$Year <- 1990
    meta1990$group <- "NP12"
    # meta1990$Hispanic.Origin <- "All Origins"
    meta1990$Hispanic.Origin <- "all"
    write.csv(meta1990, filepathCensMeta, row.names = F)
  }
}
