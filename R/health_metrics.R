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
  if (!all(c("min_age", "max_age") %in% names(total_burden))) {
    stop("total_burden must contain columns 'min_age', and 'max_age'")
  }

  if (!all(c("value") %in% names(total_burden)) & !all(c("lower","mean","upper") %in% names(total_burden))) {
    stop("total_burden must contain columns 'value' or 'mean', 'lower', and 'upper'")
  }

  if(any(total_burden$max_age - total_burden$min_age >= 30)){
    stop("age adjustment is supposed to be used for granular age bins")
    #TODO
  }

  pop_summary <- get_population_data(agr_by = agr_by, year = year, pop.summary.dir = pop.summary.dir)
  if(agr_by == "county"){
    if ('svi_bin' %in% names(pop_summary)) {
      pop_summary <- pop_summary %>% select(-svi_bin)
    }

    if ('svi_bin1' %in% names(pop_summary)) {
      pop_summary <- pop_summary %>% select(-svi_bin1)
    }

    if ('svi_bin2' %in% names(pop_summary)) {
      pop_summary <- pop_summary %>% select(-svi_bin2)
    }
    if ('svi_bin3' %in% names(pop_summary)) {
      pop_summary <- pop_summary %>% select(-svi_bin3)
    }
    if ('svi_bin4' %in% names(pop_summary)) {
      pop_summary <- pop_summary %>% select(-svi_bin4)
    }

    if ('rural_urban_class' %in% names(pop_summary)) {
      pop_summary <- pop_summary %>% select(-rural_urban_class)
    }
  }

  ## --- measure 1: Deaths and YLL-----
  # Deaths
  total_burden$measure1 <- "Deaths"

  #------measure 2: absolute number, crude rate and age-standartised rates-----
  # absolute number
  total_burden$measure2 <- "absolute number"

  # age-standartised rates
  # see https://www.cdc.gov/nchs/data/nvsr/nvsr57/nvsr57_14.pdf, page 125 for more information, Table VIII
  standartpopulation <- read_excel(file.path("data", "standartpopulation.xlsx"))
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

  #test anti join
  anti_join <- diagnose_join_issues(df1 = total_burden,
                       df2 = total_burden_age_adj,
                       join_cols = setdiff(colnames(pop_summary), c("min_age", "max_age", "source2", "Population")))
  if(nrow(anti_join) > 0){
    warning("diagnose_join_issues() in add_age_adjusted_rate(): total_burden, total_burden_age_adj")
  }

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

    # Identify problematic rows and calculate how much percent "mean" is higher than "Population"
    problematic_data <- total_burden_age_adj %>%
      filter(mean >= Population + 3) %>%
      mutate(percent_higher = ((mean - Population) / Population) * 100)

    # Calculate the mean of percent_higher
    mean_percent_higher <- mean(problematic_data$percent_higher)
    num_problematic_rows <- nrow(problematic_data)

    # Get the total number of rows
    total_rows <- nrow(total_burden_age_adj)

    # Stop execution if any problematic rows are found
    if (num_problematic_rows/total_rows > 0.01) {
      stop(paste0("In age-adjustment, Mean is not less than Population in ",
                  num_problematic_rows, " out of ", total_rows, " rows. ",
                  "Mean of percent_higher: ", round(mean_percent_higher, 2),
                  "%."))
    }else if(0.01 >= num_problematic_rows/total_rows & num_problematic_rows/total_rows > 0){
      warning(paste0("In age-adjustment, Mean is not less than Population in ",
                  num_problematic_rows, " out of ", total_rows, " rows. ",
                  "Mean of percent_higher: ", round(mean_percent_higher, 2),
                  "%."))
    }

    total_burden_age_adj <- total_burden_age_adj %>%
      filter(full_stand_popsize >= 1) %>%
      dplyr::mutate(
        mean = pmin(mean, Population),
        lower = pmin(lower, Population),
        upper = pmin(upper, Population)
      ) %>%
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
