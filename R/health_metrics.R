#' Calculate Age-Adjusted Rate
#'
#' This function calculates the age-adjusted rate for a given total burden and population summary, using a standard population file.
#'
#' @param total_burden A data frame containing the total burden information.
#' @param year A numeric value representing the year for which the calculation is to be done.
#' @param agr_by A character string specifying the aggregation level (e.g., "nation", "STATEFP", "county").
#' @param pop.summary.dir A character string specifying the directory path to the population summary data (default is "../data/12_population_summary").
#' @import dplyr
#' @import tidyr
#' @importFrom readxl read_excel
#' @references \url{https://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_14.pdf}, page 125, Table VIII
#' @return A data frame containing the calculated age-adjusted rate.
#' @export
add_age_adjusted_rate <- function(total_burden, year, agr_by, pop.summary.dir = "data/12_population_summary"){
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
  # Deaths
  total_burden$measure1 <- "Deaths"
  #total_burden <- total_burden %>% dplyr::rename(value = Deaths)

  # YLL
  #lifeExpectancy <- read.csv(file.path("../data", "IHME_GBD_2019_TMRLT_Y2021M01D05.csv"))
  #total_burden_yll <- total_burden %>%
  #  dplyr::mutate(
  #    Life.Expectancy = lifeExpectancy$Life.Expectancy[findInterval(max_age, lifeExpectancy$Age)], # TODO max_age?
  #    value = value * Life.Expectancy,
  #    measure1 = "YLL",
  #    Life.Expectancy = NULL
  #  )

  #total_burden <- rbind(total_burden, total_burden_yll)
  #rm(lifeExpectancy, total_burden_yll)

  #------measure 2: absolute number, crude rate and age-standartised rates-----
  # absolute number
  total_burden$measure2 <- "absolute number"

  # crude rate
#  pop_summary_agr <- pop_summary %>%
#    group_by_at(vars(all_of(setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population"))))) %>%
 #   summarise(Population = sum(Population))

  #total_burden_crude <- total_burden %>%
  #  inner_join(pop_summary_agr, by = setdiff(colnames(pop_summary_agr), "Population")) %>%
  #  filter(Population > 0) %>% # TODO
  #  mutate(
  #    value =  100000* (value / Population), #0/0 = NaN
  #    value = ifelse(is.nan(value), 0, value), #
  #    measure2 = "crude rate",
  #    Population = NULL
  #  )
  #total_burden_crude <- total_burden_crude %>% filter(!is.na(value)) # TODO
  #rm(pop_summary_agr)

  # age-standartised rates
  # see https://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_14.pdf, page 125 for more information, Table VIII
  standartpopulation <- read_excel(file.path("data", "standartpopulation.xlsx"))
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

  if("value" %in% colnames(total_burden_age_adj)){
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
  }else if(all(c("lower","mean","upper") %in% names(total_burden))){
    total_burden_age_adj <- total_burden_age_adj %>%
      group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj), c("mean","lower","upper"))))) %>%
      summarise(mean = sum(mean),
                lower = sum(lower),
                upper = sum(upper)) %>%
      ungroup()

    if (any(total_burden_age_adj[["mean"]] >= total_burden_age_adj[["Population"]])) {
      browser()
      stop(paste0("In age-adjustment, Mean is not less than Population in one or more rows."))
    }

    total_burden_age_adj <- total_burden_age_adj %>%
      filter(Population >= 1 & full_stand_popsize >= 1) %>%
      dplyr::mutate(
        mean = (mean * standard_popsize / Population) * (100000 / full_stand_popsize),
        mean = ifelse(is.nan(mean), 0, mean),
        lower = (lower * standard_popsize / Population) * (100000 / full_stand_popsize),
        lower = ifelse(is.nan(lower), 0, lower),
        upper = (upper * standard_popsize / Population) * (100000 / full_stand_popsize),
        upper = ifelse(is.nan(upper), 0, upper),
        measure2 = "age-adjusted rate",
        Population = NULL, standard_popsize = NULL
      )

    if (any(total_burden_age_adj[["mean"]] >= 100000)) {
      stop(paste0("In age-adjustment, after adjustment rows with >= 100000"))
    }
  }

  total_burden <- rbind(total_burden,  total_burden_age_adj) #total_burden_crude,
  rm( total_burden_age_adj, standartpopulation, full_stand_popsize, pop_summary)

  ## ----finish------
  #if(agr_by != "nation"){
  #  total_burden <- total_burden %>%
  #    filter(measure1 == "Deaths" & measure2 %in% c("absolute number", "age-adjusted rate"))
  #}

  #restrict everything to age_group 25+
  total_burden <- total_burden %>%
    filter(min_age >= 25)

  total_burden <- total_burden %>% distinct()

  return(total_burden)
}
#' Calculate Age-Adjusted Rate
#'
#' This function calculates the age-adjusted rate for a given total burden and population summary, using a standard population file.
#'
#' @param total_burden A data frame containing the total burden. Must contain columns "value", "min_age", and "max_age".
#' @param pop_summary A data frame containing the population summary. Must contain columns "min_age", "max_age", and "Population".
#' @param path_to_standartpopulation A string representing the path to the standard population Excel file.
#' @return A data frame containing the age-adjusted rate.
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate filter group_by_at summarise distinct ungroup
#' @importFrom tidyr crossing
#' @references \url{https://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_14.pdf}, page 125, Table VIII
#' @export
add_age_adjusted_rate_old <- function(total_burden, pop_summary, path_to_standartpopulation = "data/standartpopulation.xlsx"){
  if (!all(c("min_age", "max_age") %in% names(total_burden))) {
    stop("total_burden must contain columns 'min_age', and 'max_age'")
  }

  if (!all(c("value") %in% names(total_burden)) & !all(c("lower","mean","upper") %in% names(total_burden))) {
    stop("total_burden must contain columns 'value' or 'mean', 'lower', and 'upper'")
  }

  # Validate pop_summary
  if (!all(c("min_age", "max_age", "Population") %in% names(pop_summary))) {
    stop("pop_summary must contain columns 'min_age', 'max_age', and 'Population'")
  }

  if(any(total_burden$max_age - total_burden$min_age >= 30)){
    stop("age adjustment is supposed to be used for granular age bins")
    #TODO
  }

  if ('svi_bin' %in% names(pop_summary)) {
    pop_summary <- pop_summary %>% select(-svi_bin)
  }

  if ('rural_urban_class' %in% names(pop_summary)) {
    pop_summary <- pop_summary %>% select(-rural_urban_class)
  }

  standartpopulation <- read_excel(path_to_standartpopulation)
  #274633642
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

  anti_joined <- anti_join(total_burden,
                          total_burden_age_adj,
                          by = setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population")),
  )
  if(nrow(anti_joined) > 0){
    warning(paste("in add_age_adjusted_rate() population data and total_burden data not joinable in", nrow(anti_joined)," rows:"))
    warning(head(anti_joined))
  }

  # calculate age-adjusted rate
  #total_burden_age_adj <- inner_join_age_right_outer(total_burden,
  #                                                   total_burden_age_adj,
  #                                                   by = setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population")),
  #                                                   group_column = NULL
  #)

   total_burden_age_adj <- total_burden %>%
    left_join(total_burden_age_adj,
      by = setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population")),
      multiple = "all"
    ) %>%
    filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
   mutate(min_age.x = NULL, max_age.x = NULL) %>%
    rename(min_age = min_age.y, max_age = max_age.y)

  if(all(c("lower","mean","upper") %in% names(total_burden))){
    total_burden_age_adj <- total_burden_age_adj %>%
      group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj), c("lower","mean","upper", "Population"))))) %>%
      summarise(mean = sum(mean),
                lower = sum(lower),
                upper = sum(upper),
                Population = sum(Population)) %>%
      ungroup()

    if (any(total_burden_age_adj[["mean"]] >= total_burden_age_adj[["Population"]])) {
      browser()
      stop(paste0("In age-adjustment, Mean is not less than Population in one or more rows."))
    }

    total_burden_age_adj <- total_burden_age_adj %>%
      filter(Population >= 1 & full_stand_popsize >= 1) %>%
      dplyr::mutate(
        mean = (mean * standard_popsize / Population) * (100000 / full_stand_popsize),
        mean = ifelse(is.nan(mean), 0, mean),
        lower = (lower * standard_popsize / Population) * (100000 / full_stand_popsize),
        lower = ifelse(is.nan(lower), 0, lower),
        upper = (upper * standard_popsize / Population) * (100000 / full_stand_popsize),
        upper = ifelse(is.nan(upper), 0, upper),
        measure2 = "age-adjusted rate",
        Population = NULL, standard_popsize = NULL
      )

    if (any(total_burden_age_adj[["mean"]] >= 100000)) {
      stop(paste0("In age-adjustment, after adjustment rows with >= 100000"))
    }

  }else{
    total_burden_age_adj <- total_burden_age_adj %>%
      group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj), c("value","Population"))))) %>%
      summarise(value = sum(value),
                Population = sum(Population)) %>%
      ungroup()

    if (any(total_burden_age_adj[["value"]] >= total_burden_age_adj[["Population"]])) {
      stop(paste0("In age-adjustment, value is not less than Population in one or more rows."))
    }

    total_burden_age_adj <- total_burden_age_adj %>%
      filter(Population >= 1 & full_stand_popsize >= 1) %>%
      dplyr::mutate(
        value = (value * standard_popsize / Population) * (100000 / full_stand_popsize),
        value = ifelse(is.nan(value), 0, value),
        measure2 = "age-adjusted rate",
        Population = NULL, standard_popsize = NULL
      )

    if (any(total_burden_age_adj[["value"]] >= 100000)) {
      stop(paste0("In age-adjustment, after adjustment rows with >= 100000"))
    }
  }

  total_burden_age_adj
}


#health_metrics
add_years_of_life_lost <- function(df, path_life_expectancy = "data/IHME_GBD_2019_TMRLT_Y2021M01D05.csv"){
  #TODO check df
}
#if(FALSE){
  #' Calculate Crude Rate
  #'
  #' This function calculates the crude rate for a given total burden and population summary.
  #'
  #' @param total_burden A data frame containing the total burden. Must contain columns "value".
  #' @param pop_summary A data frame containing the population summary. Must contain columns "Population".
  #' @return A data frame containing the crude rate.
  #' @importFrom dplyr inner_join filter mutate group_by_at summarise
  #' @export
#  add_crude_rate <- function(total_burden, pop_summary) {
    # Validate total_burden
#    if (!"value" %in% names(total_burden)) {
#      stop("total_burden must contain the 'value' column")
#    }

    # Validate pop_summary
#    if (!"Population" %in% names(pop_summary)) {
#      stop("pop_summary must contain the 'Population' column")
#    }
#
    # Aggregate population summary by grouping variables, excluding "min_age", "max_age", "source2", and "Population"
#    pop_summary_agr <- pop_summary %>%
#      group_by_at(vars(all_of(setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population"))))) %>%
#      summarise(Population = sum(Population))
    #TODO how is age relevant?

    # Join total burden with aggregated population summary, filter out zero populations, and calculate crude rate
#    total_burden_crude <- total_burden %>%
#      inner_join(pop_summary_agr, by = setdiff(colnames(pop_summary_agr), "Population")) %>%
#      filter(Population > 0) %>%
#      mutate(
#        value = 100000 * (value / Population), # Calculate crude rate per 100,000 population
#        value = ifelse(is.nan(value), 0, value), # Replace NaN with 0
#        measure2 = "crude rate",
#        Population = NULL
#      )

    # Filter out NA values (if any)
#    total_burden_crude <- total_burden_crude %>% filter(!is.na(value))

#    total_burden_crude
#  }
#}



