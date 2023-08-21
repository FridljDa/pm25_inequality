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
add_age_adjusted_rate <- function(total_burden, pop_summary, path_to_standartpopulation = "data/standartpopulation.xlsx"){
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
    #stop("age adjustment is supposed to be used for granular age bins")
    #TODO
  }

  standartpopulation <- read_excel(path_to_standartpopulation)
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
  total_burden_age_adj <- inner_join_age_right_outer(total_burden,
                                                     total_burden_age_adj,
                                                     by = setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population")),
                                                     group_column = NULL
  )

  # total_burden_age_adj <- total_burden %>%
  #  left_join(total_burden_age_adj,
  #    by = setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population")),
  #    multiple = "all"
  #  ) %>%
  #  filter(min_age.y <= min_age.x & max_age.x <= max_age.y) %>%
  #  mutate(min_age.x = NULL, max_age.x = NULL) %>%
  #  rename(min_age = min_age.y, max_age = max_age.y)

  if(all(c("lower","mean","upper") %in% names(total_burden))){
    total_burden_age_adj <- total_burden_age_adj %>%
      group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj), c("lower","mean","upper"))))) %>%
      summarise(mean = sum(mean),
                lower = sum(lower),
                upper = sum(upper)) %>%
      ungroup()

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

  }else{
    total_burden_age_adj <- total_burden_age_adj %>%
      group_by_at(vars(all_of(setdiff(colnames(total_burden_age_adj), "value")))) %>%
      summarise(value = sum(value)) %>%
      ungroup()

    total_burden_age_adj <- total_burden_age_adj %>%
      filter(Population >= 1 & full_stand_popsize >= 1) %>%
      dplyr::mutate(
        value = (value * standard_popsize / Population) * (100000 / full_stand_popsize),
        value = ifelse(is.nan(value), 0, value),
        measure2 = "age-adjusted rate",
        Population = NULL, standard_popsize = NULL
      )
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



