#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 03/27/2021
# Purpose: calculate attributable burden
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat",  "readxl", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

dataDir <- args[2]
tmpDir <- args[3]
totalBurdenParsedDir <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  tmpDir <- "/Users/default/Desktop/paper2021/data/tmp"
  totalBurdenParsedDir <- "/Users/default/Desktop/paper2021/data/09_total_burden_parsed"
  dataDir <- "/Users/default/Desktop/paper2021/data"

  # dataDir <- "C:/Users/Daniel/Desktop/paper2021/data/data"
  # tmpDir <- "C:/Users/Daniel/Desktop/paper2021/data/tmp"
  # totalBurdenParsedDir<- "C:/Users/Daniel/Desktop/paper2021/data/09_total_burden_parsed"

  tmpDir <- "/Volumes/fridljand/R/HIGH/data/tmp"
  totalBurdenParsedDir <- "/Volumes/fridljand/R/HIGH/data/09_total_burden_parsed"
  dataDir <- "/Volumes/fridljand/R/HIGH/data"
  
  tmpDir <- "data/tmp"
  totalBurdenParsedDir <- "data/09_total_burden_parsed"
  dataDir <- "data"
}
findreplaceDir <- file.path(totalBurdenParsedDir, "findreplace.csv")
states <- file.path(tmpDir, "states.csv") %>% read.csv()
rural_urban_class <- read.csv(file.path(dataDir, "rural_urban_class.csv"))
not_interested_states <- c("ZZ", "YY", "PR", "CC", "MX", "VI", "GU", "CU", "AS", "MP", "XX", "ON", "QC")

if (!file.exists(findreplaceDir)) {
  #### ----- 1990-1991------
  findreplaces1 <-
    rbind(
      data.frame(
        replacecolumns = "Year",
        from = c(90:99),
        to = c(1990:1999)
      ),
      data.frame(
        replacecolumns = "Hispanic.Origin",
        from = c(0, 1, 2, 3, 4, 5, 99),
        to = c("Not Hispanic or Latino", rep("Hispanic or Latino", 5), "Unknown")
      ),
      data.frame(
        replacecolumns = "Gender.Code",
        from = c(1, 2),
        to = c("M", "F")
      ),
      data.frame(
        replacecolumns = "Race",
        from = c(1, 2, 3, 4:8, 9),
        to = c("White", "Black or African American", "American Indian or Alaska Native", rep("Asian or Pacific Islander", 5), "Unknown")
      ),
      data.frame(
        replacecolumns = "STATEFP",
        from = c(1:51, 52:62, NA),
        to = c(states$STATEFP, rep(0, 12))
      ), data.frame(
        replacecolumns = "interested_state",
        from = c(1:51, 52:62, NA),
        to = c(rep(1, 51), rep(0, 12))
      ),
      data.frame(
        replacecolumns = "min_age",
        from = c(1:199, sprintf("2%02d", c(0:11, 99)), sprintf("3%02d", c(0:3, 99)), sprintf("4%02d", c(0:27, 99)), sprintf("5%02d", c(0:23, 99)), sprintf("6%02d", c(0:59, 99)), 999),
        to = c(1:199, rep("0", 13 + 5 + 29 + 25 + 61), "Unknown")
      ),
      data.frame(
        replacecolumns = "max_age",
        from = c(1:199, sprintf("2%02d", c(0:11, 99)), sprintf("3%02d", c(0:3, 99)), sprintf("4%02d", c(0:27, 99)), sprintf("5%02d", c(0:23, 99)), sprintf("6%02d", c(0:59, 99)), 999),
        to = c(1:199, rep("0", 13 + 5 + 29 + 25 + 61), "Unknown")
      )
    )
  findreplaces1 <- merge(data.frame(Year = 1990:1991), findreplaces1)

  #### ----- 1992-2002------
  findreplaces2 <-
    rbind(
      data.frame(
        replacecolumns = "Year",
        from = c(90:99, 0:16, 1990:2016),
        to = c(1990:2016, 1990:2016)
      ),
      data.frame(
        replacecolumns = "Hispanic.Origin",
        from = c(0, 1, 2, 3, 4, 5, 99),
        to = c("Not Hispanic or Latino", rep("Hispanic or Latino", 5), "Unknown")
      ),
      data.frame(
        replacecolumns = "Gender.Code",
        from = c(1, 2),
        to = c("M", "F")
      ),
      data.frame(
        replacecolumns = "Race",
        from = c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 68, 78),
        to = c("White", "Black or African American", "American Indian or Alaska Native", rep("Asian or Pacific Islander", 8), "Unknown", rep("Asian or Pacific Islander", 2))
      ),
      data.frame(
        replacecolumns = "STATEFP",
        from = c(1:51, 52:62, NA),
        to = c(states$STATEFP, rep(0, 12))
      ),
      data.frame(
        replacecolumns = "interested_state",
        from = c(1:51, 52:62, NA),
        to = c(rep(1, 51), rep(0, 12))
      ),
      data.frame(
        replacecolumns = "min_age",
        from = c(1:199, sprintf("2%02d", c(0:11, 99)), sprintf("3%02d", c(0:3, 99)), sprintf("4%02d", c(0:27, 99)), sprintf("5%02d", c(0:23, 99)), sprintf("6%02d", c(0:59, 99)), 999),
        to = c(1:199, rep("0", 13 + 5 + 29 + 25 + 61), "Unknown")
      ),
      data.frame(
        replacecolumns = "max_age",
        from = c(1:199, sprintf("2%02d", c(0:11, 99)), sprintf("3%02d", c(0:3, 99)), sprintf("4%02d", c(0:27, 99)), sprintf("5%02d", c(0:23, 99)), sprintf("6%02d", c(0:59, 99)), 999),
        to = c(1:199, rep("0", 13 + 5 + 29 + 25 + 61), "Unknown")
      )
    )
  findreplaces2 <- merge(data.frame(Year = 1992:2002), findreplaces2)
  #### ----- 2003-2016 -------
  findreplaces3 <-
    rbind(
      data.frame(
        replacecolumns = "Hispanic.Origin",
        from = c(1:5, 6:8, 9),
        to = c(rep("Hispanic or Latino", 5), rep("Not Hispanic or Latino", 3), "Unknown")
      ),
      data.frame(
        replacecolumns = "STATEFP",
        from = c(states$STUSPS, not_interested_states, NA),
        to = c(states$STATEFP, rep(0, length(not_interested_states) + 1))
      ),
      data.frame(
        replacecolumns = "interested_state",
        from = c(states$STUSPS, not_interested_states, NA),
        to = c(rep(1, nrow(states)), rep(0, length(not_interested_states) + 1))
      ),
      data.frame(
        replacecolumns = "Race",
        from = c(1, 2, 3, 4, 5, 6, 7, 18, 28, 38, 48, 58, 68, 78),
        to = c("White", "Black or African American", "American Indian or Alaska Native", rep("Asian or Pacific Islander", 8), "Asian or Pacific Islander", rep("Asian or Pacific Islander", 2))
      ),
      # data.frame(replacecolumns = "min_age", from = c(1:52), to = c(rep(0, 22), 1, 2, 3, 4, seq(5, 120, 5), 125, "unknown") ),
      # data.frame(replacecolumns = "max_age", from = c(1:52), to = c(rep(0, 22), 1, 2, 3, 4, seq(9, 124, 5), 150, "unknown")),
      data.frame(
        replacecolumns = "min_age",
        from = c(sprintf("1%03d", 1:135), sprintf("2%03d", c(0:11, 999)), sprintf("4%03d", c(0:27, 999)), sprintf("5%03d", c(0:23, 999)), sprintf("6%03d", c(0:59, 999)), "1999", "9999"),
        to = c(sprintf("%03d", 1:135), rep("0", 13 + 29 + 25 + 61), "Unknown", "Unknown")
      ),
      data.frame(
        replacecolumns = "max_age",
        from = c(sprintf("1%03d", 1:135), sprintf("2%03d", c(0:11, 999)), sprintf("4%03d", c(0:27, 999)), sprintf("5%03d", c(0:23, 999)), sprintf("6%03d", c(0:59, 999)), "1999", "9999"),
        to = c(sprintf("%03d", 1:135), rep("0", 13 + 29 + 25 + 61), "Unknown", "Unknown")
      ),
      data.frame(
        replacecolumns = "Education1989",
        from = c(0:17, 99, NA),
        to = c(rep("lower", 13), rep("middle", 3), rep("higher", 2), "Unknown", 101)
      ),
      data.frame(
        replacecolumns = "Education2003",
        # 7 = Master’s degree
        # 8 = Doctorate or professional degree
        # 9 = unknown
        # 10 = 1989 revision, not comparable
        from = c(1:9, NA),
        # to = c(1:7,7,9,10)
        to = c(rep("lower", 3), rep("middle", 2), rep("higher", 3), "Unknown", 101)
      )
    )

  findreplaces3 <- merge(data.frame(Year = 2003:2016), findreplaces3)

  ## --- county code---
  # maximum number of counties per state
  maximum_number_counties <- 840
  concat <- function(state, county) sprintf("%s%03d", state, county)

  findreplaces4 <- rbind(
    data.frame(
      replacecolumns = "county",
      from = c(outer(states$STATEFP, 1:maximum_number_counties, FUN = concat)),
      to = c(outer(states$STATEFP, 1:maximum_number_counties, FUN = concat))
    ),
    data.frame(
      replacecolumns = "county",
      from = c(
        c(outer(1:62, c(0, 999), FUN = concat)),
        57:62,
        c(outer(57:62, 0:maximum_number_counties, FUN = concat)),
        paste0(states$STATEFP, "999"),
        NA,
        0
      ),
      to = "Unknown"
    )
  )

  findreplaces4 <- rbind(
    findreplaces4 %>% mutate(from = str_pad(from, 5, pad = "0")),
    findreplaces4
  ) %>% distinct()

  findreplaces4 <- merge(
    data.frame(Year = 1990:2002),
    findreplaces4
  )

  findreplaces5 <- rbind(
    data.frame(
      replacecolumns = "county",
      from = c(outer(states$STUSPS, 1:maximum_number_counties, FUN = concat)),
      to = c(outer(states$STATEFP, 1:maximum_number_counties, FUN = concat))
    ),
    data.frame(
      replacecolumns = "county",
      from = c(
        c(outer(states$STUSPS, c(0, 999), FUN = concat)),
        not_interested_states,
        c(outer(not_interested_states, 0:maximum_number_counties, FUN = concat)),
        paste0(states$STUSPS, "999"),
        NA,
        0
      ),
      to = "Unknown"
    )
  ) %>% distinct()

  findreplaces5 <- merge(
    data.frame(Year = 2003:2009),
    findreplaces5
  )

  findreplaces6 <- rbind(
    data.frame(
      replacecolumns = "county",
      from = c(outer(states$STUSPS, 1:maximum_number_counties, FUN = concat)),
      to = c(outer(states$STATEFP, 1:maximum_number_counties, FUN = concat))
    ),
    data.frame(
      replacecolumns = "county",
      from = c(
        c(outer(states$STUSPS, c(0, 999), FUN = concat)),
        not_interested_states,
        c(outer(not_interested_states, 0:maximum_number_counties, FUN = concat)),
        paste0(states$STUSPS, "999"),
        NA,
        0
      ),
      to = "Unknown"
    )
  ) %>% distinct()

  # https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2010.html
  findreplaces6$to[findreplaces6$to == 15009] <- 15005
  findreplaces6$to[findreplaces6$to == 51515] <- 51019
  findreplaces6$to[findreplaces6$to == 46113] <- 46102
  findreplaces6$to[findreplaces6$to == 02270] <- 02158

  findreplaces6 <- merge(
    data.frame(Year = 2010:2016),
    findreplaces6
  )

  findreplaces_county <- rbind(findreplaces4, findreplaces5, findreplaces6)
  rm(findreplaces4, findreplaces5, findreplaces6, maximum_number_counties, concat)
  ### ---- rural_urban_class---
  findreplaces7 <- left_join(rural_urban_class %>% mutate(FIPS.code = as.character(FIPS.code)),
    findreplaces_county,
    by = c("FIPS.code" = "to")
  )

  findreplaces7 <- findreplaces7 %>%
    filter(fromYear <= Year) %>%
    group_by(FIPS.code, Year) %>%
    filter(fromYear == max(fromYear)) %>%
    mutate(fromYear = NULL) %>%
    ungroup()

  findreplaces7 <- findreplaces7 %>%
    select("Year", "from", "to" = "rural_urban_class") %>%
    mutate(replacecolumns = "rural_urban_class")

  findreplaces7 <- rbind(
    findreplaces_county %>%
      filter(to == "Unknown") %>%
      mutate(replacecolumns = "rural_urban_class"),
    merge(
      data.frame(Year = 1990:1999),
      data.frame(
        replacecolumns = "rural_urban_class",
        from = c("02232", "02282"), # corrected manually, somehow missing
        to = c(5, 6)
      )
    ),
    findreplaces7
  )


  findreplaces <- rbind(findreplaces1, findreplaces2, findreplaces3, findreplaces_county, findreplaces7)

  write.csv(findreplaces, findreplaceDir, row.names = FALSE)
  rm(findreplaces1, findreplaces2, findreplaces3, findreplaces7)
}

causesDir <- file.path(totalBurdenParsedDir, "causes.csv")
if (!file.exists(causesDir)) {
  # https://www150.statcan.gc.ca/n1/pub/82-003-x/2013007/article/11852/tbl/appb-eng.htm
  causes1 <- data.frame(
    replacecolumns = "label_cause",
    from = c(
      414,
      434, 432, 430,
      161, 162,
      490:496,
      519,
      250,
      140:242, 244:259, 270:279, 282:285, 296:319, 324:380, 383:459, 470:478, 490:611, 617:629, 680:759,
      # lower respitory infections
      519
    ),
    to = c("cvd_ihd", rep("cvd_stroke", 3), rep("neo_lung", 2), rep("resp_copd", 7), "lri", "t2_dm", rep("ncd_lri", 516))
  )
  causes1 <- merge(data.frame(Year = 1990:1998), causes1)
  causes2 <- data.frame(
    replacecolumns = "label_cause",
    from = c(
      "I20", "I21", "I22", "I23", "I24", "I25",
      "G45", "G46", "I61", "I62", "I63", "I65", "I66", "I67", "I68", "I69",
      "C33", "C34", "D02", "D14", "D38",
      "J41", "J42", "J43", "J44",
      "A48", "A70", "B97", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J20", "J21", "P23", "U04",
      "E11",
      sprintf("C%02d", 0:97), sprintf("D%02d", 0:48), sprintf("D%02d", 55:89),
      sprintf("E%02d", 3:7), sprintf("E%02d", 10:16), sprintf("E%02d", 20:34),
      sprintf("E%02d", 65:88), sprintf("F%02d", 1:99), sprintf("F%02d", 1:99),
      sprintf("G%02d", 6:98), sprintf("H%02d", 0:61), sprintf("H%02d", 68:93),
      sprintf("I%02d", 0:99), sprintf("J%02d", 30:98), sprintf("K%02d", 0:92),
      sprintf("N%02d", 0:64), sprintf("N%02d", 75:98), sprintf("L%02d", 0:98),
      sprintf("M%02d", 0:99), sprintf("Q%02d", 0:99),
      # lower respitory infections
      sprintf("J%02d", 9:22)
    ),
    to = c(
      rep("cvd_ihd", 6), rep("cvd_stroke", 10), rep("neo_lung", 5), rep("resp_copd", 4), rep("lri", 15), "t2_dm",
      rep("ncd_lri", 1276)
    )
  )
  causes2 <- merge(data.frame(Year = 1999:2016), causes2)
  causes <- rbind(causes1, causes2)
  write.csv(causes, causesDir, row.names = FALSE)
}
