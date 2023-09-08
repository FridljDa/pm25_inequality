suppressMessages({
  library(dplyr)
  library(magrittr)
  library(data.table)
  # library(tidyverse)
  library(tictoc)
})

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)
suppressMessages({pkgload::load_all()})

# Pass in arguments
args <- commandArgs(trailingOnly = T)

if (rlang::is_empty(args)) {
  agr_by <- "STATEFP"
  year <- 2016
} else {
  year <- args[1]
  agr_by <- args[10]
}
pop.summary.dir <- "data/12_population_summary"
#totalBurdenRateDir <- "data/13_total_burden_rate"
#totalBurdenRateDir <- "09_total_burden_parsed"
summaryHigherTotalDir <- "data/16_sum_higher_geog_level_total"
# should have #min_age = min_age.x, max_age = min_age.x, for age specific
# in l184 in 21_calc_attr_burd_di.R

# TODO
summaryHigherTotalDir <- file.path(summaryHigherTotalDir, agr_by)
dir.create(summaryHigherTotalDir, recursive = T, showWarnings = F)
summaryHigherTotalDir <- file.path(summaryHigherTotalDir, paste0("total_burden_age_adjusted_", year, ".csv"))

if (file.exists(summaryHigherTotalDir)) {
  quit()
}
## --- read attr burden----
#totalBurdenRateDir <- file.path(totalBurdenRateDir, "county")
files <- list.files("data/09_total_burden_parsed/county/nvss")
files <- files[grepl(year, files)]

total_burden <- lapply(files, function(file) {
  total_burden_i <- read_data(file.path("data/09_total_burden_parsed/county/nvss", file))
  if("Deaths" %in% colnames(total_burden_i)) total_burden_i <- total_burden_i %>% rename(value = Deaths)

  if(!"measure1" %in% colnames(total_burden_i)){
    total_burden_i <- total_burden_i %>%
      mutate(
        measure1 = "Deaths"
      )
  }
  if(!"measure2" %in% colnames(total_burden_i)){
    total_burden_i <- total_burden_i %>%
      mutate(
        measure2 = "absolute number"
      )
  }
  return(total_burden_i)
}) %>% rbindlist(use.names = TRUE, fill = TRUE)

total_burden <- total_burden %>%
  filter(attr == "overall")

if(year <= 2008){
  total_burden <- total_burden %>%
    filter(Education == "666")
}

#total_burden <- total_burden %>% sample_n(20)
## --sum up geographic levels from county----

if (agr_by != "county") {
  cat("marginalised svi and rural_urban class info to total_burden -starting\n")
  tic("marginalised svi and rural_urban class info to total_burden")

  total_burden <- rbind(
    total_burden %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin3 = "666"),
    total_burden %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin4 = "666"),
    total_burden %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin1 = "666", svi_bin3 = "666", svi_bin4 = "666"),
    total_burden %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin2 = "666", svi_bin3 = "666", svi_bin4 = "666"),
    total_burden %>% mutate(rural_urban_class = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin3 = "666", svi_bin4 = "666"),
    total_burden %>% mutate(svi_bin1 = "666", svi_bin2 = "666", svi_bin3 = "666", svi_bin4 = "666"),
    total_burden %>% mutate(rural_urban_class = "666", svi_bin = "666", svi_bin1 = "666", svi_bin2 = "666", svi_bin3 = "666", svi_bin4 = "666")
  )

  total_burden <- total_burden %>%
    group_by_at(setdiff(
      colnames(total_burden),
      c("value")
    )) %>%
    summarise(value = sum(value), .groups = 'drop')
  toc()

  #cat("marginalised svi and rural_urban class info to attr_burden 1-starting\n")
  #tic("marginalised svi and rural_urban class info to attr_burden 1")
  #total_burden_with_rural_urban_class <- total_burden %>%
  #  group_by_at(setdiff(
  #    colnames(total_burden),
  #    c("rural_urban_class", "value")
  #  )) %>%
  #  summarise(value = sum(value)) %>%
  #  mutate(rural_urban_class = as.factor(666))
  #toc()

  #cat("marginalised svi and rural_urban class info to attr_burden 2-starting\n")
  #tic("marginalised svi and rural_urban class info to attr_burden 2")
  #total_burden_with_svi_bin <- total_burden %>%
  #  group_by_at(setdiff(
  #    colnames(total_burden),
  #    c("svi_bin", "value")
  #  )) %>%
  #  summarise(value = sum(value)) %>%
  #  mutate(svi_bin = as.factor(666))
  #toc()

  #cat("marginalised svi and rural_urban class info to attr_burden 3-starting\n")
  #tic("marginalised svi and rural_urban class info to attr_burden 3")
  #total_burden_with_all <- total_burden %>%
  #  group_by_at(setdiff(
  #    colnames(total_burden),
  #    c("rural_urban_class", "svi_bin", "value")
  #  )) %>%
  #  summarise(value = sum(value)) %>%
  #  mutate(rural_urban_class = as.factor(666), svi_bin = as.factor(666))
  #toc()

  #total_burden <- rbind(
  #  total_burden_with_rural_urban_class,
  #  total_burden_with_svi_bin,
  #  total_burden_with_all
  #)

  #rm(
  #  total_burden_with_rural_urban_class,
  #  total_burden_with_svi_bin,
  #  total_burden_with_all
  #)
  #toc()
} else {
  #total_burden$rural_urban_class <- NA
  #total_burden$svi_bin <- NA
}

#filter out combination
total_burden <- total_burden %>%
  filter(!(Education != "666" & Race != "All" &
             (rural_urban_class != "666" | svi_bin != "666" | svi_bin1 != "666" | svi_bin2 != "666"
              | svi_bin3 != "666" | svi_bin4 != "666")))

## ----group out counties---

if (agr_by == "STATEFP") {
  total_burden <- total_burden %>%
    mutate(
      STATEFP = stringr::str_sub(county, 1, -4) %>%
        as.integer() %>%
        as.factor()
    )
  group_variables <- setdiff(
    colnames(total_burden),
    c("value", "county")
  )
} else if (agr_by == "nation") {
  total_burden <- total_burden %>%
    mutate(
      nation = "us"
    )
  group_variables <- setdiff(colnames(total_burden), c("value", "county"))
} else if (agr_by == "county") {
  group_variables <- setdiff(colnames(total_burden), c("value"))
}

tic(paste("summed up county level estimates to ", agr_by, "in year ", year))
total_burden <- total_burden %>%
  group_by_at(vars(all_of(c(group_variables)))) %>%
  summarise(
    value = sum(value)
  ) %>%
  ungroup()

toc()

#------ age-standartised rates-------
total_burden_absolute_number <- total_burden %>%
  filter(measure1 == "Deaths" &
    measure2 == "absolute number")

tic("age standardised attributable burden")
total_burden <- add_age_adjusted_rate(total_burden_absolute_number,
                                               year,
                                               agr_by,
                                               pop.summary.dir = "data/12_population_summary")


toc()

#total_burden <- rbind(
#  total_burden_age_adj,
#  total_burden_absolute_number
#)

#rm(total_burden_age_adj, total_burden_absolute_number)
### ----sum out age ----

group_variables <- setdiff(colnames(total_burden), c("value", "min_age", "max_age"))
total_burden_over_25 <- total_burden %>%
  filter(min_age >= 25) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(
    min_age = 25,
    max_age = 150
  )

total_burden_over_65 <- total_burden %>%
  filter(min_age >= 65) %>%
  group_by_at(vars(all_of(group_variables))) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(
    min_age = 65,
    max_age = 150
  )

total_burden <- rbind(total_burden_over_25, total_burden_over_65)

fwrite(total_burden, summaryHigherTotalDir)
