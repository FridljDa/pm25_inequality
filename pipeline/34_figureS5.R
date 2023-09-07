#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
#rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "data.table", "magrittr", "shiny", "ggplot2", "ggpubr", "scales", "grid", "cowplot",
  "dplyr", "stringr", "tidyr",
  "gridExtra", "grid", "lattice"
)

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(scipen = 10000)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

summaryDir <- args[7]
figuresDir <- args[8]
scenarioI <- args[10]
methodI <- args[11]
min_ageI <- args[13]

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "data/17_summary"
  figuresDir <- "data/18_figures"

  min_ageI <- 25

  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
all_burden <- fread(file.path(summaryDir, "all_burd.csv"))
all_burden <- all_burden %>% filter(min_age == min_ageI)
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))
pm_summ <- pm_summ %>% filter(min_age == min_ageI)
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----
#TODO more granular
pm_summ <- pm_summ %>%
  filter(Gender.Code == "All genders" & agr_by == "STATEFP" & pm_metric == "mean" & scenario == scenarioI)

pm_summ <- pm_summ %>%
  filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All") %>%
  select(Year, Education, Ethnicity, rural_urban_class, value, Region)

all_burden <- all_burden %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           source == "National Vital Statistics System" & agr_by == "STATEFP")

all_burden <- all_burden %>%
  filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All") %>%
  select(Year, Education, Ethnicity, rural_urban_class, overall_value = value, Region)

all_burden_pm_summ <- inner_join(pm_summ, all_burden,
                                 by = c("Year", "Education", "Ethnicity", "rural_urban_class", "Region"))

g1 <- ggplot(all_burden_pm_summ, aes(x = value, y = overall_value, color = Year)) +
  geom_point()+
  labs(
       x= "Population-weighted mean PM2.5 exposure (\u03bcg/m^3)",
       y= "Age-adjusted mortality\n per 100,000 from all causes") +
  theme(text = element_text(size=15))

g1
# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(file.path(figuresDir, paste0(methodI, "-", scenarioI, "-", min_ageI), "figureS5.png"), dpi = 300, g1, height = 4, width = 8)
