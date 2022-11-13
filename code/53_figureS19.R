#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

library(data.table)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

library(ggalt)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

summaryDir <- args[7]
figuresDir <- args[8]
scenarioI <- args[10]
methodI <- args[11]

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "data/15_summary"
  figuresDir <- "data/16_figures"

  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files("data/15_summary")
file_list <- file.path("data/15_summary", file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = T)
all_burd <- file.path(summaryDir, "all_burd.csv") %>% fread()
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI)

all_burd <- all_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & agr_by == "nation" &
    Ethnicity != "All, All Origins" & rural_urban_class == "All" & Education == 666) %>%
  filter(Year %in% c(2000, 2016) &
    Ethnicity != "White")

all_burd <- all_burd %>%
  select(Year, Ethnicity, overall_value)
## ---colors ----
group.colors <- c(
  RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1:6, 8:10, 12)],
  RColorBrewer::brewer.pal(n = 6, name = "Spectral")[1:2]
)
group.colors[c(12, 2)] <- group.colors[c(2, 12)]
names(group.colors) <- c(
  "NH White",
  "Hispanic or Latino White",
  "Black American",
  "White",
  "Asian or Pacific Islander",
  "American Indian or Alaska Native",
  "High school graduate or lower",
  "Some college education but no 4-year college degree",
  "4-year college graduate or higher",
  "Non metro",
  "Large metro",
  "Small-medium metro"
)
group.colors <- group.colors[c(1:2, 4:6)]
## -- figure 3, attributable burden---

attr_burd1 <- attr_burd %>%
  filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" &
    measure3 == "prop. of overall burden" & rural_urban_class == "All" &
    method %in% c("di_gee"))

attr_burd1 <- attr_burd1 %>%
  filter(Year %in% c(2000, 2016) &
    Ethnicity != "White") %>%
  left_join(all_burd,
    by = c("Year", "Ethnicity")
  ) %>%
  mutate(Year = as.factor(Year))

all_burd_attr_burd1 <- attr_burd1 %>%
  pivot_wider(
    names_from = Year,
    values_from = c(mean, overall_value),
    names_prefix = "year_",
    id_cols = c(Ethnicity)
  )


g3 <- ggplot() +
  geom_segment(
    data = all_burd_attr_burd1,
    aes(
      y = Ethnicity,
      yend = Ethnicity,
      x = mean_year_2000,
      xend = mean_year_2016
    )
  ) +
  geom_point(
    data = attr_burd1,
    aes(y = Ethnicity,x = mean,colour = Year,size = overall_value)
  ) +
  # scale_colour_manual(values=c(2000 ="#a3c4dc", 2016 = "#0e668b")) +
  scale_size_area() +
  labs(
    x = "Percent of all-cause mortality attributable to PM2.5",
    y = "Race-Ethnicity"
  ) +
  scale_colour_manual(values = c("2000" = "#a3c4dc", "2016" = "#0e668b")) +
  guides(size = "none") +
  theme(legend.position = "bottom")


g3

ggsave(file.path(
  "data/16_figures",
  paste0(methodI, "-", scenarioI), "figureS19.png"
), g3, height = 4, width = 8)
