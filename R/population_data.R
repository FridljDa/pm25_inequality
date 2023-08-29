library(tidyr)
get_population_data <- function(agr_by, year, pop.summary.dir = "data/12_population_summary"){
  #---read population data----
  # Mapping Summary:
  # ------------------------------------------------------------------
  # | education | race  | county | rural_urban_class | svi_bin | resulting_df |
  # ------------------------------------------------------------------
  # |     *     |   *   |  Yes   |        Yes        |   Yes   | pop_summary2 |
  # |     *     |   *   |  Yes   |        Yes        |   Yes   | pop_summary2 |
  # |     *     |   *   |  Yes   |        No         |   No    | pop_summary2 |
  # |    No     |  Yes  |   No   |        No         |   No    | pop_summary1 |
  # |    Yes    |  No   |   No   |        No         |   No    | pop_summary1 |
  # |    Yes    |  Yes  |   No   |        No         |   No    | pop_summary3 |
  # ------------------------------------------------------------------
  # * = Yes/No

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
    pop_summary2 <- pop_summary2 %>%
      filter(!Education == 666) #rural_urban_class == 666 &
  }else{
    pop_summary2 <- pop_summary2 %>%
      mutate(county = FIPS.code,
             state = NULL,
             FIPS.code = NULL)
  }

  if(agr_by == "nation"){
    #for race-education interaction
    pop_summary3 <- file.path(pop.summary.dir, paste0("pop_race_educ_nation.csv")) %>%
      read.csv() %>%
      filter(Year == year)
  }else{
    pop_summary3 <- NULL
  }

  #add svi_bin
  if (!is.null(pop_summary1) & !"svi_bin" %in% names(pop_summary1)) pop_summary1 <- pop_summary1 %>% mutate(svi_bin = "666")
  if (!is.null(pop_summary2) &!"svi_bin" %in% names(pop_summary2)) pop_summary2 <- pop_summary2 %>% mutate(svi_bin = "666")
  if (!is.null(pop_summary3) &!"svi_bin" %in% names(pop_summary3)) pop_summary3 <- pop_summary3 %>% mutate(svi_bin = "666")


  pop_summary <- rbind(pop_summary1, pop_summary2, pop_summary3) %>% distinct

  pop_summary <- pop_summary %>%
    mutate_at(c("rural_urban_class", "svi_bin", "Education"), as.factor) %>%
    mutate(source2 = NULL)

  if(agr_by == "county") pop_summary <- pop_summary %>% select(-rural_urban_class, -svi_bin)

  #rm(pop_summary1, pop_summary2, pop_summary3)

  if (agr_by == "nation") {
    #pop_summary <- pop_summary %>%
    #  complete(Year, nation, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
    #           fill = list(Population = 0)
    #  )%>%
    #  mutate_at(c("nation"), as.factor)
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
      mutate_at(c("county"), as.integer)

    #pop_summary <- pop_summary %>% filter(Race != "All")
  }

  # Subset to exclude the 'Population' column
  pop_summary_sub <- pop_summary[, !(names(pop_summary) %in% "Population")]

  # Check for duplicates in the subset data frame
  if (any(duplicated(pop_summary_sub))) {
    dublicates <- pop_summary[duplicated(pop_summary_sub), ]
    warning("Duplicates found in the data frame pop_summary in add_age_adjusted_rate():")
    print(dublicates)
  }

  pop_summary <- pop_summary %>% filter(svi_bin != "Unknown")
  #
  return(pop_summary)
}
#test <- get_population_data(agr_by = "nation",
#                                year = 2010,
#                                pop.summary.dir = "data/12_population_summary")

#population data used for age_standartisation
get_population_data_old <- function(agr_by, year, pop.summary.dir = "data/12_population_summary"){
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
}
get_population_data_old <- function(agr_by, year, pop.summary.dir = "data/12_population_summary"){
  #TODO fix for county level

  if (file.exists(file.path(pop.summary.dir, paste0("pop_", agr_by, ".csv"))) & agr_by != "county") {
    pop_summary1 <- file.path(pop.summary.dir, paste0("pop_", agr_by, ".csv")) %>%
      read_data() %>%
      filter(Year == year)
  } else {
    pop_summary1 <- NULL
  }

  pop_summary2 <- file.path(pop.summary.dir, agr_by, paste0("pop_sum_", year, ".csv")) %>%
    read_data() %>%
    filter(Year == year)

  if (agr_by != "county") {
    pop_summary2 <- pop_summary2 %>% filter(!(rural_urban_class == 666 & Education == 666))
  }

  if (agr_by == "nation") {
    pop_summary3 <- file.path(pop.summary.dir, paste0("pop_race_educ_nation.csv")) %>%
      read_data() %>%
      filter(Year == year)
  } else {
    pop_summary3 <- NULL
  }

  pop_summary <- rbind(pop_summary1, pop_summary2, pop_summary3) %>% distinct()

  pop_summary <- pop_summary %>%
    #mutate_at(c("rural_urban_class", "Education"), as.factor) %>%
    mutate(source2 = NULL)


  if (agr_by == "nation") {
    pop_summary <- pop_summary %>%
      #tidyr::complete(Year, nation, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
      #         fill = list(Population = 0)
      #) %>%
      mutate_at(c("nation"), as.factor)
  } else if (agr_by == "STATEFP") {
    pop_summary <- pop_summary %>%
      #tidyr::complete(Year, STATEFP, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
      #         fill = list(Population = 0)
      #) %>%
      mutate_at(c("STATEFP"), as.factor)
  } else if (agr_by == "county") {
    #pop_summary <- pop_summary %>%
      # complete(Year, county, nesting(Gender.Code, Race, min_age, max_age, Hispanic.Origin, Education), rural_urban_class,
      #         fill = list(Population = 0)
      # )%>%
      #mutate_at(c("county"), as.factor)

    # pop_summary <- pop_summary %>% filter(Race != "All")
  }

  if(agr_by == "county") pop_summary <- pop_summary %>% mutate(county = as.integer(county))

  return(pop_summary)
}
