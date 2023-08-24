library(tidyr)
#population data used for age_standartisation

get_population_data <- function(agr_by, year, pop.summary.dir = "data/12_population_summary"){
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
