#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 05/02/2021
# Purpose: plot data
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

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

# TODO delete
if (rlang::is_empty(args)) {
  summaryDir <- "/Users/default/Desktop/paper2021/data/15_summary"
  # summaryDir <- "/Users/default/Desktop/data_summary_old"
  figuresDir <- "/Users/default/Desktop/paper2021/data/16_figures"

  summaryDir <- "/g/huber/users/fridljand/R/HIGH/data/15_summary"
  figuresDir <- "/g/huber/users/fridljand/R/HIGH/data/16_figures"

  summaryDir <- "data/15_summary"
  figuresDir <- "data/16_figures"

  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

pm_summ <- pm_summ %>%
  filter(Gender.Code == "All genders" & Region == "United States" & pm_metric == "mean" & scenario == scenarioI)

pm_summ <- pm_summ %>%
  filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All")

## -- figure 3, attributable burden---
g1 <- ggplot(pm_summ, aes(x = Year, y = value)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  labs(title="PM2.5 exposure") +
  ylab(expression(paste("Population-weighted mean PM2.5 exposure (", mu, g, "/", m^3, ")", sep = ""))) +
  theme(text = element_text(size=10))
g1

### ----all-burden---
file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
all_burden <- fread(file.path(summaryDir, "all_burd.csv"))
rm(file_list)

all_burden <- all_burden %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    source == "National Vital Statistics System" & Region == "United States")
all_burden <- all_burden %>%
  filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class == "All")

g2 <- ggplot(all_burden, aes(x = Year, y = overall_value)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  ylab("Age-adjusted mortality per 100,000") +
  labs(title="All-cause mortality rate") +
  theme(text = element_text(size=10))
g2

## ----attributable burden----
file_list <- list.files(summaryDir)
file_list <- file.path(summaryDir, file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = TRUE)
rm(file_list)

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
    attr == "attributable" &
    source == "National Vital Statistics System" & scenario == scenarioI & method == methodI)

attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity == "All, All Origins" & measure3 == "value" & rural_urban_class == "All")
g3 <- ggplot(attr_burd1, aes(x = Year, y = mean)) +
  geom_line(size = 1.5) +
  xlab("Year") +
  geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, 
              alpha = 0, show.legend = FALSE) +
  ylab("Age-adjusted mortality per 100,000") +
  labs(title="Mortality attributable\n to PM2.5 exposure") +
  theme(text = element_text(size=10))

g_combined <- ggarrange(g1, g2, g3, nrow = 1)
g_combined

# https://stackoverflow.com/questions/40265494/ggplot-grobs-align-with-tablegrob
ggsave(#filename = "figureS19.png",
       filename = file.path(figuresDir, paste0(methodI, "-", scenarioI), "figureS19.png"),
       plot = g_combined,
       dpi = 300, height = 4, width = 8)
