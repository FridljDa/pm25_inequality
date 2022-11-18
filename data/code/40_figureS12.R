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
library(grid)
library(gridExtra)

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
  #summaryDir <- "/Users/default/Desktop/data_summary_old"
  figuresDir <- "/Users/default/Desktop/paper2021/data/16_figures"
  
  summaryDir <- "/g/huber/users/fridljand/R/HIGH/data/15_summary"
  figuresDir <- "/g/huber/users/fridljand/R/HIGH/data/16_figures"
  
  summaryDir <- "data/15_summary"
  figuresDir <- "data/16_figures"
  
  scenarioI <- "real"
  methodI <- "di_gee"
}

file_list <- list.files("data/15_summary")
file_list <- file.path("data/15_summary", file_list[grepl("attr_bur", file_list)])
attr_burd <- lapply(file_list, fread) %>% rbindlist(use.names = T)
rm(file_list)

theme_set(theme_classic(base_family = "Helvetica")); options(bitmapType ="cairo");
# dir.create(file.path(figuresDir, methodI), recursive = T, showWarnings = F)
### ----- read stuff----

attr_burd <- attr_burd %>%
  filter(Gender.Code == "All genders" & measure1 == "Deaths" & measure2 == "age-adjusted rate per 100,000" &
           attr == "attributable" &
           source == "National Vital Statistics System" & scenario == scenarioI)

## -- figure 3, attributable burden---
#TODO method di_gee/burnett

attr_burd1 <- attr_burd %>% filter(agr_by == "nation" & Education == 666 & Ethnicity != "All, All Origins" & measure3 == "proportion of disparity to Black or African American attributable" & rural_urban_class == "All"
                                   & method %in% c("di_gee","di_gee_white")) #"di_gee","di_gee_white"
range(attr_burd1$mean) %>% round(1)
g1 <- ggplot(attr_burd1, aes(x = Year, y = mean, color = Ethnicity, linetype = method))
g1 + geom_line()
#attr_burd2 <- attr_burd %>% filter(agr_by == "nation" & Education != 666 & Ethnicity == "All, All Origins" & measure3 == "proportion of disparity to lower educational attainment"  & rural_urban_class == "All"
#                                   & method == "di_gee")
#g2 <- ggplot(attr_burd2, aes(x = Year, y = mean, color = Education))


attr_burd$measure3 %>% unique
## --set range---
#min1 <- min(c(attr_burd1$lower, attr_burd2$lower))
#max1 <- max(c(attr_burd1$upper, attr_burd2$upper))

#g1 <- g1 + ylim(min1, max1)
#g2 <- g2 + ylim(min1, max1)

#plots <- list(g1, g2)
#plots <- list(g1)
#rm(min1, max1)
#rm(
#  attr_burd1, attr_burd2, 
#  g1, g2
#)
#----formatting------
group.colors <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1:6,8:10, 12)], 
                  RColorBrewer::brewer.pal(n = 6, name = "Spectral")[1:2])
group.colors[c(12,2)] <- group.colors[c(2,12)] 
names(group.colors) <- c("NH White",
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
group.colors <- group.colors[c(1:2,4:6)]

g1 <- g1 +
  geom_line(size = 1.5) +
  xlab("Year") +
  #geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0, show.legend = FALSE) +
  scale_colour_manual(values=group.colors) +
  theme(legend.title = element_blank()) +
  guides(color=guide_legend(ncol=3,byrow=TRUE), linetype = "none")+
  ylab("%") +
  theme(legend.position = "bottom")+
  theme(text = element_text(size=15))

g1


ggsave(file.path("data/16_figures", paste0(methodI,"-",scenarioI), "figureS12.png"),plot = g1, dpi = 300, height = 4, width = 8)
