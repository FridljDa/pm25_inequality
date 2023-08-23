#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 04/16/2021
# Purpose: interact with results in UI
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("data.table", "dplyr", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "tidyr")

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(scipen = 10000)

# load calculated data


#if (!file.exists(summaryDir)) summaryDir <- "https://raw.github.com/FridljDa/paper2021/master/data/17_summary"

file_list <- list.files("data/17_summary")
file_list <- file.path("data/17_summary", file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = T)
rm(file_list)

#variability by Race-ethnicity for each method
attr_burd_methods <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           Ethnicity != "All, All origins" & Education == 666 & rural_urban_class == "All" &
           attr == "attributable" &
           source == "National Vital Statistics System" & scenario == "A" &
           agr_by == "nation" & 
           measure3 == "value")

attr_burd_methods <- attr_burd_methods %>%
  group_by(Year, method) %>%
  summarise(CoV = sqrt(var(mean))/mean(mean))

plot_attr_burd_methods <- ggplot(attr_burd_methods, aes(x = Year, y = CoV, color = method)) +
  geom_line(size = 1.5)

#variability by Race-ethnicity for each scenario
attr_burd_scenario <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           Ethnicity != "All, All origins" & Education == 666 & rural_urban_class == "All" &
           attr == "attributable" &
           source == "National Vital Statistics System" & method == "di_gee" &
           agr_by == "nation" & 
           measure3 == "value")

attr_burd_scenario <- attr_burd_scenario %>%
  group_by(Year, scenario) %>%
  summarise(CoV = sqrt(var(mean))/mean(mean))

plot_attr_burd_scenario <- ggplot(attr_burd_scenario, aes(x = Year, y = CoV, color = scenario)) +
  geom_line(size = 1.5)

##same exposure
year <- 2016
total_burden <- file.path("data/13_total_burden_rate/nation/nvss", paste0("total_burden_", year, ".csv")) %>%
  fread()

total_burden <- total_burden %>% mutate_at(c("rural_urban_class", "Education"), as.factor)
total_burden <- total_burden %>% filter(label_cause == "all-cause" & nation == "us" & 
                                          Education == 666 & rural_urban_class == 666 & measure1 == "Deaths" &
                                          measure2 == "age-adjusted rate" & min_age >= 25)
total_burden <- total_burden %>% 
  group_by(Year, Race, Hispanic.Origin) %>%
  summarise(value = sum(value))

hr_race_specific <- data.frame(
  method = rep("di_gee", 1),
  Race = c("White","White", "Black American", "Asian or Pacific Islander", "White", "American Indian or Alaska Native", "All"), # TODO
  Hispanic.Origin = c("All Origins","Not Hispanic or Latino", "All Origins", "All Origins", "Hispanic or Latino", "All Origins", "All Origins"), # TODO
  label_cause =  rep("all-cause", 1),
  hr_mean = c(1.063, 1.063, 1.208, 1.096, 1.116, 1.1, 1.073),
  hr_lower = c(1.06, 1.06, 1.199, 1.075, 1.1, 1.06, 1.071),
  hr_upper = c(1.065,1.065, 1.217, 1.117, 1.133,1.14, 1.075),
  min_age = rep(25, 1)
)

paf_di <- inner_join(total_burden, hr_race_specific, by = c("Race", "Hispanic.Origin"))

pm <- seq(0, 30, by = 0.5)
#median = 7.383503

attr_burd_di <- tidyr::crossing(paf_di, pm)

attr_burd_di <- attr_burd_di %>%
  mutate(
    attributable_burden = case_when(
      pm < 5 ~ 0,
      pm >= 5 ~ value * (pm - 5) * (hr_mean - 1) / 10
    )
  )

plot_attr_burd_di <- ggplot(data = attr_burd_di, aes(color = paste(Race, Hispanic.Origin), x = pm, y = attributable_burden)) +
  geom_line()

##---get median pm ---
pm_summary <- readr::read_csv("data/17_summary/pm_summary.csv") 
pm_summary <- pm_summary %>% filter(scenario == "A" & rural_urban_class == "All" & Region == "United States" &
                                      Ethnicity == "All, All Origins" & Education == 666 & Year == 2016 &
                                      pm_metric == "median")
attr_burd_di_median <- paf_di %>% mutate(pm = pm_summary$value)
attr_burd_di_median <- attr_burd_di_median %>%
  mutate(
    attributable_burden = case_when(
      pm < 5 ~ 0,
      pm >= 5 ~ value * (pm - 5) * (hr_mean - 1) / 10
    )
  )
