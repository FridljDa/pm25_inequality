
suppressMessages({
  library(tidyr)
  library(purrr)
  })

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
      filter(!(Education == 666 & rural_urban_class == 666) |
               !(Education == 666 & svi_bin == 666)) %>%
      filter(!(Education != 666 & Race != "All"))#rural_urban_class == 666 &
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
    mutate_at(c("rural_urban_class", "svi_bin", "Education"), as.factor)

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

  if("svi_bin" %in% colnames(pop_summary)) pop_summary <- pop_summary %>% filter(svi_bin != "Unknown")

  #filter out rows, which are also available in more granular
  pop_summary <- pop_summary %>%
    group_by(across(-all_of(c("Population", "min_age", "max_age", "source2")))) %>%
    arrange(min_age, max_age) %>%
    filter(!(min_age <= lag(min_age) & lag(max_age) <= max_age) | is.na(lag(min_age))) %>%
    ungroup()

  sanity_check <- pop_summary %>%
    group_by(across(-all_of(c("Population", "min_age", "max_age", "source2")))) %>%
    nest() %>%
    mutate(is_partition = map(data, ~ is_partition(.x))) %>%
    unnest(cols = c(is_partition)) %>%
    as.data.frame()

  if(any(!sanity_check$is_partition)){
    browser()
    #diff_df <- anti_join(pop_summary, pop_summary_filtered)
    #test <- pop_summary %>% filter(svi_bin == 1 & Race == "All" & Education == "666")
    stop("in population_data.R, pop.summary has not is_partition")
  }

  pop_summary <- pop_summary %>%
    mutate(source2 = NULL)

  return(pop_summary)
}

#' Check if the given data.frame represents a partition of an interval
#'
#' @param df A data.frame with columns "min_age" and "max_age"
#' @return A boolean value indicating if the data.frame represents a partition.
#' @export
#' @examples
#' df <- data.frame(min_age = c(25, 30, 35, 40), max_age = c(29, 34, 39, 44))
#' is_partition(df)
is_partition <- function(df) {

  # Check if data.frame has the required columns
  if (!all(c("min_age", "max_age") %in% names(df))) {
    stop("The data.frame must have 'min_age' and 'max_age' columns.")
  }

  # Sort by min_age
  df <- df[order(df$min_age),]

  # Find min and max values from the interval
  min_val <- min(df$min_age)
  max_val <- max(df$max_age)

  # Check for gaps and overlaps
  for (i in seq_len(nrow(df) - 1)) {
    if (df$max_age[i] + 1 != df$min_age[i + 1]) {
      return(FALSE)
    }
  }

  # Check if the data frame covers the whole interval [min(min_age), max(max_age)]
  if (df$min_age[1] != min_val || df$max_age[nrow(df)] != max_val) {
    return(FALSE)
  }

  return(TRUE)
}

#' Check if the given data.frame has overlapping age intervals
#'
#' @param df A data.frame with columns "min_age" and "max_age"
#' @return A boolean value indicating if the data.frame has overlapping age intervals.
#' @export
#' @examples
#' df <- data.frame(min_age = c(25, 26), max_age = c(25, 26))
#' has_overlaps(df)
has_overlaps <- function(df) {
  require(dplyr)

  # Check if data.frame has the required columns
  if (!all(c("min_age", "max_age") %in% names(df))) {
    stop("The data.frame must have 'min_age' and 'max_age' columns.")
  }

  # Sort by min_age using dplyr
  df <- df %>% arrange(min_age)

  # Create a new column with the 'min_age' from the next row
  df <- df %>% mutate(next_min = lead(min_age))

  # Check for overlaps
  overlaps <- any(df$max_age >= df$next_min, na.rm = TRUE)

  return(overlaps)
}
