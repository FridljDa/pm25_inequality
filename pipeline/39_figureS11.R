#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2022
# Purpose: plot data as map
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

library(data.table)
library(magrittr)
library(dplyr)
library(testthat)
#library(tidyverse)

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
  min_ageI <- 25
  summaryDir <- "data/17_summary"
  figuresDir <- "data/18_figures"

  methodI <- "di_gee"
  scenarioI <- "real"
}
pkgload::load_all()
options(bitmapType = "cairo")

#source(paste0("https://raw.githubusercontent.com/mkiang/",
#              "opioid_hotspots/master/code/mk_nytimes.R"))
theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
## --- read files---
file_list <- list.files(file.path(summaryDir, "county"))
file_list <- file.path(summaryDir, "county", file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, read_data) %>% rbindlist(use.names = TRUE)
attr_burd <- attr_burd %>% filter(min_age == min_ageI)
#rm(file_list)

## --- filter---
attr_burd <- attr_burd %>%
  filter(
    Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" & method == methodI &
      attr == "attributable" & source == "National Vital Statistics System" & agr_by == "county" & measure3 == "value"
    & scenario == scenarioI
  )

## --- filter -----
attr_burd_ethn_2000 <- attr_burd %>%
  filter(
    Education == 666,
    Year == 2000,
    Ethnicity %in% c(
      "NH White",
      "Black American"
    )
  ) %>%
  select(Year, Region, Education, Ethnicity, mean) %>%
  pivot_wider(names_from = Ethnicity, values_from = mean)

attr_burd_ethn_2016 <- attr_burd %>%
  filter(
    Education == 666,
    Year == 2016,
    Ethnicity %in% c(
      "NH White",
      "Black American"
    )
  ) %>%
  select(Year, Region, Education, Ethnicity, mean) %>%
  pivot_wider(names_from = Ethnicity, values_from = mean)

attr_burd_educ_2009 <- attr_burd %>%
  filter(
    Ethnicity == "All, All Origins",
    Year == 2009,
    Education %in% c(
      "4-year college graduate or higher",
      "High school graduate or lower"
    )
  ) %>%
  select(Year, Region, Education, Ethnicity, mean) %>%
  pivot_wider(names_from = Education, values_from = mean)

attr_burd_educ_2016 <- attr_burd %>%
  filter(
    Ethnicity == "All, All Origins",
    Year == 2016,
    Education %in% c(
      "4-year college graduate or higher",
      "High school graduate or lower"
    )
  ) %>%
  select(Year, Region, Education, Ethnicity, mean) %>%
  pivot_wider(names_from = Education, values_from = mean)

###--- get ranges----
range_ethn_x <- range(c(
  attr_burd_ethn_2000[, "Black American", drop = TRUE],
  attr_burd_ethn_2016[, "Black American", drop = TRUE]
),
na.rm = TRUE
)

range_ethn_y <- range(c(
  attr_burd_ethn_2000[, "NH White", drop = TRUE],
  attr_burd_ethn_2016[, "NH White", drop = TRUE]
),
na.rm = TRUE
)

range_educ_x <- range(c(
  attr_burd_educ_2009[, "High school graduate or lower", drop = TRUE],
  attr_burd_educ_2016[, "High school graduate or lower", drop = TRUE]
),
na.rm = TRUE
)

range_educ_y <- range(c(
  attr_burd_educ_2009[, "4-year college graduate or higher", drop = TRUE],
  attr_burd_educ_2016[, "4-year college graduate or higher", drop = TRUE]
),
na.rm = TRUE
)

#range_x <- range(c(
#  attr_burd_ethn_2000[, "Black American"],
#  attr_burd_ethn_2016[, "Black American"],
#  attr_burd_educ_2009[, "High school graduate or lower"],
#  attr_burd_educ_2016[, "High school graduate or lower"]
#),
#na.rm = TRUE
#)

#range_y <- range(c(
#  attr_burd_ethn_2000[, "NH White"],
#  attr_burd_ethn_2016[, "NH White"],
#  attr_burd_educ_2009[, "4-year college graduate or higher"],
#  attr_burd_educ_2016[, "4-year college graduate or higher"]
#),
#na.rm = TRUE
#)
## ---plot----
plot_attr_burd_ethn_2000 <- ggplot(
  attr_burd_ethn_2000,
  aes(x = `Black American`, y = `NH White`)
) +
  geom_point(alpha = .3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_cartesian(xlim = range_ethn_x, ylim = range_ethn_y) +
  ggtitle("2000")#+
  #mk_nytimes()


plot_attr_burd_ethn_2016 <- ggplot(
  attr_burd_ethn_2016,
  aes(x = `Black American`, y = `NH White`)
) +
  geom_point(alpha = .3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_cartesian(xlim = range_ethn_x, ylim = range_ethn_y) +
  ggtitle("2016")#+
  #mk_nytimes()

plot_attr_burd_educ_2009 <- ggplot(
  attr_burd_educ_2009,
  aes(x = `High school graduate or lower`, y = `4-year college graduate or higher`)
) +
  geom_point(alpha = .3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_cartesian(xlim = range_educ_x, ylim = range_educ_y)+
  ggtitle("2009")#+
  #mk_nytimes()

plot_attr_burd_educ_2016 <- ggplot(
  attr_burd_educ_2016,
  aes(x = `High school graduate or lower`, y = `4-year college graduate or higher`)
) +
  geom_point(alpha = .3) +
  geom_abline(intercept = 0, slope = 1) +
  coord_cartesian(xlim = range_educ_x, ylim = range_educ_y)+
  ggtitle("2016")#+
  #mk_nytimes()

#blank_space <- 0.05
#figure_width <- 1.3
#figure_hight <- 1

#lay <- rbind(
#  c(NA, NA, 13, NA, NA, 14),
#  c(10, 7, 1, NA, 8, 4),
#  c(NA, 7, NA, NA, 8, NA),
#  c(11, 7, 2, NA, 8, 5),
#  c(NA, 7, NA, NA, 8, NA),
#  c(12, 7, 3, NA, 8, 6),
#  c(NA, NA, 9, 9, 9, 9)
#)

#g_combined <- grid.arrange(
#  grobs = gs,
#  widths = c(0.1, 0.1, figure_width , blank_space, 0.1, figure_width),
#  heights = c(0.2, figure_hight, blank_space, figure_hight, blank_space, figure_hight, 0.6),
#  layout_matrix = lay
#)

g_combined <- cowplot::plot_grid(plot_attr_burd_ethn_2000, plot_attr_burd_educ_2009,
  plot_attr_burd_ethn_2016, plot_attr_burd_educ_2016
)

ggsave(file.path(figuresDir, paste0(methodI,"-",scenarioI), "figureS11.png"), dpi = 300, g_combined, height = 9, width = 8)
